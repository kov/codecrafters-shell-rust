use lazy_static::lazy_static;
use os_pipe::{PipeReader, PipeWriter};
use rustyline::{
    completion::{Completer, Pair},
    error::ReadlineError,
    highlight::Highlighter,
    hint::Hinter,
    history::{History as _, MemHistory, SearchDirection},
    validate::Validator,
    Config, Context, Editor, Helper,
};
use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    env::{current_dir, set_current_dir},
    fs::{self, File},
    io::{self, stdout, Write as _},
    os::{fd::OwnedFd, unix::fs::PermissionsExt as _},
    path::{Path, PathBuf},
    process::Child,
    rc::Rc,
    str::CharIndices,
    sync::Mutex,
};

lazy_static! {
    static ref BUILTINS: Mutex<Box<[&'static str]>> =
        Mutex::new(Box::new(["echo", "exit", "type", "pwd", "cd", "history"]));
}

fn is_executable(entry: &fs::DirEntry) -> bool {
    let path = entry.path();

    if let Ok(metadata) = fs::metadata(&path) {
        if metadata.is_file() {
            let mode = metadata.permissions().mode();
            return mode & 0o111 != 0;
        }
    }

    false
}

fn collect_executables_in_path() -> io::Result<Vec<String>> {
    let path_env = std::env::var("PATH").expect("Failed to read PATH environment variable");
    let path_candidates: Vec<&str> = path_env.split(':').collect();

    let mut executables = vec![];
    for candidate in path_candidates {
        let dir = fs::read_dir(candidate);

        // We may have directories that do not exist anymore on PATH.
        if dir.is_err() {
            continue;
        }

        // We protect against this unwrap panicking by checking and continuing above.
        for entry in dir.unwrap() {
            let entry = entry?;
            if is_executable(&entry) {
                if let Some(filename) = entry.path().file_name() {
                    executables.push(filename.to_string_lossy().into_owned());
                }
            }
        }
    }

    Ok(executables)
}

type LineEditor = Rc<RefCell<Editor<LineHelper, MemHistory>>>;

#[derive(Clone)]
struct LineHelper {}

impl LineHelper {
    fn new() -> Self {
        LineHelper {}
    }
}

impl Completer for LineHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>), rustyline::error::ReadlineError> {
        let word = &line[..pos];

        let mut matches = (*BUILTINS.lock().expect("poisoned lock")).to_vec();

        let executables = collect_executables_in_path()?;
        let executables: Vec<&str> = executables.iter().map(|s| s.as_str()).collect();
        matches.extend_from_slice(&executables);

        matches.retain(|s| s.starts_with(word));
        matches.sort_unstable();
        matches.dedup();

        Ok((
            0,
            matches
                .iter()
                .map(|s| Pair {
                    display: s.to_string(),
                    replacement: format!("{} ", s),
                })
                .collect(),
        ))
    }
}

impl Helper for LineHelper {}
impl Hinter for LineHelper {
    type Hint = String;
}
impl Highlighter for LineHelper {}
impl Validator for LineHelper {}

struct ReplIter<'r> {
    command: &'r str,
    chars: CharIndices<'r>,
}

impl<'r> Iterator for ReplIter<'r> {
    type Item = Result<Cow<'r, str>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut start = None;
        let mut treat_next_char_as_regular = false;
        let mut in_single_quotes = false;
        let mut in_double_quotes = false;
        let mut edited: Option<String> = None;

        while let Some((i, c)) = self.chars.next() {
            if treat_next_char_as_regular {
                treat_next_char_as_regular = false;

                // Backslash should keep special meaning when followed by \ $ " \n, but
                // only when under double quotes
                if !in_single_quotes && (c == '\\' || c == '$' || c == '"' || c == '\n') {
                    edited.as_mut().map(|s| s.push(c));
                    continue;
                }

                // I assume we should also escape single quotes when under single quotes...
                if in_single_quotes && c == '\'' {
                    edited.as_mut().map(|s| s.push(c));
                    continue;
                }

                // If we are in quotes we actually want not to treat \ as special other than
                // in the cases handled above.
                if in_single_quotes || in_double_quotes {
                    edited.as_mut().map(|s| s.push('\\'));
                }

                edited.as_mut().map(|s| s.push(c));
                continue;
            }

            if c.is_ascii_whitespace() && !in_single_quotes && !in_double_quotes {
                if let Some(start) = start {
                    if let Some(edited) = edited {
                        return Some(Ok(Cow::Owned(edited)));
                    } else {
                        return Some(Ok(Cow::Borrowed(&self.command[start..i])));
                    }
                }
                continue;
            }

            match c {
                '\\' => {
                    treat_next_char_as_regular = true;

                    if edited.is_none() {
                        if let Some(start) = start {
                            edited = Some(String::from(&self.command[start..i]));
                        } else {
                            edited = Some(String::new())
                        }
                    }

                    if start.is_none() {
                        start = Some(i + 1);
                    }
                }
                '\'' | '"' => {
                    if c == '\'' {
                        if !in_double_quotes {
                            in_single_quotes = !in_single_quotes;
                        } else {
                            edited.as_mut().map(|s| s.push(c));
                        }
                    } else if c == '"' {
                        if !in_single_quotes {
                            in_double_quotes = !in_double_quotes;
                        } else {
                            edited.as_mut().map(|s| s.push(c));
                        }
                    }

                    if edited.is_none() {
                        if let Some(start) = start {
                            edited = Some(String::from(&self.command[start..i]));
                        } else {
                            edited = Some(String::new())
                        }
                    }

                    if start.is_none() {
                        start = Some(i + 1);
                    }
                }
                _ => {
                    if start.is_none() {
                        start = Some(i);
                    }

                    edited.as_mut().map(|s| s.push(c));
                }
            }
        }

        // Open quotes with no closing, error...
        if in_single_quotes || in_double_quotes {
            return Some(Err(format!("Mismatched quotes in {}", self.command)));
        }

        if let Some(start) = start {
            if let Some(edited) = edited {
                return Some(Ok(Cow::Owned(edited)));
            } else {
                return Some(Ok(Cow::Borrowed(&self.command[start..])));
            }
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct Command<'r> {
    command: Cow<'r, str>,
    args: Vec<Cow<'r, str>>,
}

#[derive(Debug)]
struct RedirectOptions {
    append: bool,
    path: PathBuf,
}

#[derive(Debug)]
enum PipeTarget<'r> {
    Redirect {
        stdout: Option<RedirectOptions>,
        stderr: Option<RedirectOptions>,
    },
    CommandLine(Box<CommandLine<'r>>),
}

#[derive(Debug)]
enum CommandLine<'r> {
    Empty,
    Command(Command<'r>),
    Pipe {
        source: Command<'r>,
        target: PipeTarget<'r>,
    },
}

fn parse_parts<'r>(mut parts: ReplIter<'r>) -> Result<CommandLine<'r>, String> {
    let mut command = None;
    let mut args = vec![];
    while let Some(part) = parts.next() {
        let part = part?;

        match part.as_ref() {
            "1>" | ">" => {
                let Some(command) = command else {
                    return Err(format!("Tried to redirect empty command"));
                };
                let args = args.drain(..).collect();
                let target = match parts.next() {
                    Some(target) => PathBuf::from(target?.as_ref()),
                    None => return Err(format!("Missing redirect target")),
                };
                if let Some(token) = parts.next() {
                    let token = token?;
                    return Err(format!("Unexpected token after redirect target: {token}"));
                }
                return Ok(CommandLine::Pipe {
                    source: Command { command, args },
                    target: PipeTarget::Redirect {
                        stdout: Some(RedirectOptions {
                            append: false,
                            path: target,
                        }),
                        stderr: None,
                    },
                });
            }
            "2>" => {
                let Some(command) = command else {
                    return Err(format!("Tried to redirect empty command"));
                };
                let args = args.drain(..).collect();
                let target = match parts.next() {
                    Some(target) => PathBuf::from(target?.as_ref()),
                    None => return Err(format!("Missing redirect target")),
                };
                if let Some(token) = parts.next() {
                    let token = token?;
                    return Err(format!("Unexpected token after redirect target: {token}"));
                }
                return Ok(CommandLine::Pipe {
                    source: Command { command, args },
                    target: PipeTarget::Redirect {
                        stdout: None,
                        stderr: Some(RedirectOptions {
                            append: false,
                            path: target,
                        }),
                    },
                });
            }
            "1>>" | ">>" => {
                let Some(command) = command else {
                    return Err(format!("Tried to redirect empty command"));
                };
                let args = args.drain(..).collect();
                let target = match parts.next() {
                    Some(target) => PathBuf::from(target?.as_ref()),
                    None => return Err(format!("Missing redirect target")),
                };
                if let Some(token) = parts.next() {
                    let token = token?;
                    return Err(format!("Unexpected token after redirect target: {token}"));
                }
                return Ok(CommandLine::Pipe {
                    source: Command { command, args },
                    target: PipeTarget::Redirect {
                        stdout: Some(RedirectOptions {
                            append: true,
                            path: target,
                        }),
                        stderr: None,
                    },
                });
            }
            "2>>" => {
                let Some(command) = command else {
                    return Err(format!("Tried to redirect empty command"));
                };
                let args = args.drain(..).collect();
                let target = match parts.next() {
                    Some(target) => PathBuf::from(target?.as_ref()),
                    None => return Err(format!("Missing redirect target")),
                };
                if let Some(token) = parts.next() {
                    let token = token?;
                    return Err(format!("Unexpected token after redirect target: {token}"));
                }
                return Ok(CommandLine::Pipe {
                    source: Command { command, args },
                    target: PipeTarget::Redirect {
                        stdout: None,
                        stderr: Some(RedirectOptions {
                            append: true,
                            path: target,
                        }),
                    },
                });
            }
            "|" => {
                let Some(command) = command else {
                    return Err(format!("Tried to redirect empty command"));
                };
                let args = args.drain(..).collect();
                let target = PipeTarget::CommandLine(Box::new(parse_parts(parts)?));
                return Ok(CommandLine::Pipe {
                    source: Command { command, args },
                    target,
                });
            }
            _ => {
                if command.is_none() {
                    command = Some(part);
                } else {
                    args.push(part);
                }
            }
        }
    }

    let Some(command) = command else {
        return Ok(CommandLine::Empty);
    };

    Ok(CommandLine::Command(Command { command, args }))
}

fn parse_command<'r>(command: &'r str) -> Result<CommandLine<'r>, String> {
    let parts = ReplIter {
        command,
        chars: command.char_indices(),
    };

    //let res = parse_parts(parts);
    //dbg!(&res);
    //res
    parse_parts(parts)
}

fn search_path(executable: &str) -> Option<PathBuf> {
    let path_env = std::env::var("PATH").expect("Failed to read PATH environment variable");
    let path_candidates: Vec<&str> = path_env.split(':').collect();

    for candidate in path_candidates {
        let mut path = PathBuf::from(candidate);
        path.push(executable);

        if path.is_file() {
            return Some(path);
        }
    }

    None
}

fn handle_cmd_history(editor: &LineEditor, args: &[Cow<'_, str>]) {
    let editor_ref = editor.borrow_mut();
    let history = editor_ref.history();

    let limit = if args.len() >= 1 {
        match args[0].parse::<usize>() {
            Ok(n) => n,
            Err(err) => {
                return eprintln!("failed to parse {} as a positive number: {}", args[0], err)
            }
        }
    } else {
        history.len()
    };

    let mut items = vec![];
    for i in 0.. {
        let Ok(Some(result)) = history.get(i, SearchDirection::Forward) else {
            break;
        };
        items.push(result.entry.to_string());
    }

    let iter = items.iter().enumerate().rev().take(limit);
    iter.rev().for_each(|(count, command)| {
        // Test starts counting from 1...
        let count = count + 1;
        println!("{count:>5} {command}");
    });
}

fn handle_cmd_type(args: &[Cow<'_, str>]) {
    for arg in args {
        match arg.as_ref() {
            "echo" | "exit" | "type" | "pwd" | "cd" | "history" => {
                println!("{arg} is a shell builtin")
            }
            _ => match search_path(arg.as_ref()) {
                Some(path) => println!("{arg} is {}", path.to_string_lossy()),
                None => println!("{arg}: not found"),
            },
        }
    }
}

fn change_directory(to: &Path) {
    if let Err(_) = set_current_dir(to) {
        println!("cd: {}: No such file or directory", to.display());
    }
}

fn handle_cmd_cd(args: &[Cow<'_, str>]) {
    if args.len() > 1 {
        println!("cd: too many arguments...");
        return;
    }

    let current_dir = current_dir().expect("Unable to get current working directory");
    match args.first().map(|s| s.as_ref()) {
        Some(".") => (),
        Some("..") => {
            if let Some(parent) = current_dir.parent() {
                change_directory(parent)
            }
        }
        Some("~") | None => {
            let home = dirs::home_dir().expect("Unable to get home directory");
            change_directory(PathBuf::from(home).as_path());
        }
        Some(path_str) => {
            if path_str.starts_with('/') {
                change_directory(PathBuf::from(path_str).as_path());
            } else {
                let mut path = current_dir.clone();
                path.push(path_str);
                change_directory(path.as_path());
            }
        }
    }
}

fn handle_command(
    editor: &LineEditor,
    command: &Command,
    stdin: Option<PipeReader>,
    stdout: Option<PipeWriter>,
    stderr: Option<PipeWriter>,
    children: &mut Vec<Child>,
) -> Result<(), String> {
    let (command, args) = (command.command.as_ref(), &command.args);
    match command {
        "echo" => {
            if let Some(mut stdout) = stdout {
                writeln!(stdout, "{}", args.join(" ")).expect("Broken pipe!")
            } else {
                println!("{}", args.join(" "));
            }
        }
        "history" => handle_cmd_history(editor, args),
        "type" => handle_cmd_type(args),
        "cd" => handle_cmd_cd(args),
        "pwd" => {
            println!(
                "{}",
                current_dir()
                    .expect("Failed to get current directory")
                    .display()
            );
        }
        "exit" => {
            if args.len() > 1 {
                println!("Too many arguments for `exit': {args:?}`");
            } else if args.len() == 1 {
                let exit_code = args.first().unwrap().parse().unwrap();
                std::process::exit(exit_code);
            } else {
                std::process::exit(0);
            }
        }
        _ => {
            if let Some(_executable) = search_path(command) {
                let mut process = std::process::Command::new(command);
                let mut process = process.args(args.iter().map(|arg| arg.as_ref()));
                if let Some(stdin) = stdin {
                    process = process.stdin(stdin);
                }
                if let Some(stdout) = stdout {
                    process = process.stdout(stdout);
                }
                if let Some(stderr) = stderr {
                    process = process.stderr(stderr);
                }
                match process.spawn() {
                    Ok(child) => children.push(child),
                    Err(e) => println!("Failed to execute {command}: {e}"),
                }
            } else {
                println!("{command}: command not found")
            }
        }
    }
    Ok(())
}

fn handle_command_line(
    editor: &LineEditor,
    command_line: CommandLine<'_>,
    stdin: Option<PipeReader>,
    stdout: Option<PipeWriter>,
    stderr: Option<PipeWriter>,
    children: &mut Vec<Child>,
) -> Result<(), String> {
    match command_line {
        CommandLine::Empty => Ok(()),
        CommandLine::Command(command) => {
            handle_command(&editor, &command, stdin, stdout, stderr, children)
        }
        CommandLine::Pipe { source, target } => {
            let (stdout, stderr) = match target {
                PipeTarget::Redirect { stdout, stderr } => {
                    let stdout = if let Some(RedirectOptions { append, path }) = stdout {
                        let fd: OwnedFd = File::options()
                            .create(true)
                            .write(true)
                            .append(append)
                            .open(path)
                            .map_err(|err| err.to_string())?
                            .into();
                        Some(PipeWriter::from(fd))
                    } else {
                        None
                    };
                    let stderr = if let Some(RedirectOptions { append, path }) = stderr {
                        let fd: OwnedFd = File::options()
                            .create(true)
                            .write(true)
                            .append(append)
                            .open(path)
                            .map_err(|err| err.to_string())?
                            .into();
                        Some(PipeWriter::from(fd))
                    } else {
                        None
                    };
                    (stdout, stderr)
                }
                PipeTarget::CommandLine(command_line) => {
                    let (reader, writer) = os_pipe::pipe().map_err(|err| err.to_string())?;
                    handle_command_line(
                        &editor,
                        *command_line,
                        Some(reader),
                        stdout,
                        stderr,
                        children,
                    )?;
                    (Some(writer), None)
                }
            };
            handle_command(&editor, &source, stdin, stdout, stderr, children)
        }
    }
}

fn handle_input(editor: LineEditor, input: &str) {
    let input = input.trim();

    {
        let mut editor_ref = editor.borrow_mut();
        let history = editor_ref.history_mut();
        history.add(input).expect("Failed to add to history");
    }

    match parse_command(input) {
        Ok(command_line) => {
            let mut children = vec![];
            if let Err(msg) =
                handle_command_line(&editor, command_line, None, None, None, &mut children)
            {
                eprintln!("{}", msg)
            }
            while !children.is_empty() {
                children.retain_mut(|c| match c.try_wait() {
                    // TODO: we should keep a value for $?
                    Ok(Some(_)) => false,
                    Err(_) => false,
                    Ok(None) => true,
                });
            }
        }
        Err(msg) => {
            eprintln!("{}", msg);
        }
    };
}

fn main() {
    let config = Config::builder()
        .completion_type(rustyline::CompletionType::List)
        .build();
    let editor = Rc::new(RefCell::new(
        rustyline::Editor::with_history(config, MemHistory::new())
            .expect("Failed to create default editor for rustyline"),
    ));
    let helper = LineHelper::new();
    editor.borrow_mut().set_helper(Some(helper));

    loop {
        let input = match editor.borrow_mut().readline("$ ") {
            Ok(input) => Some(input),
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::WindowResized) => None,
            Err(err) => panic!("Error: {err}"),
        };

        // This is to detach handling the input from the borrow_mut above.
        if let Some(input) = input {
            handle_input(editor.clone(), input.as_str())
        }
    }
}
