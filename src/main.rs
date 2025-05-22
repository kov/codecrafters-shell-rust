use os_pipe::PipeWriter;
use std::{
    borrow::Cow,
    env::{current_dir, set_current_dir},
    fs::File,
    io::{self, Write as _},
    os::fd::OwnedFd,
    path::{Path, PathBuf},
    str::CharIndices,
};

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

fn handle_cmd_type(args: &[Cow<'_, str>]) {
    for arg in args {
        match arg.as_ref() {
            "echo" | "exit" | "type" | "pwd" | "cd" => println!("{arg} is a shell builtin"),
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
    command: &Command,
    stdout: Option<PipeWriter>,
    stderr: Option<PipeWriter>,
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
                if let Some(stdout) = stdout {
                    process = process.stdout(stdout);
                }
                if let Some(stderr) = stderr {
                    process = process.stderr(stderr);
                }
                match process.spawn() {
                    Ok(mut child) => {
                        let _ = child.wait();
                    }
                    Err(e) => println!("Failed to execute {command}: {e}"),
                }
            } else {
                println!("{command}: command not found")
            }
        }
    }
    Ok(())
}

fn handle_command_line(command_line: CommandLine<'_>) -> Result<(), String> {
    match command_line {
        CommandLine::Empty => Ok(()),
        CommandLine::Command(command) => handle_command(&command, None, None),
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
                    unimplemented!();
                    (None, None)
                }
            };
            handle_command(&source, stdout, stderr)
        }
    }
}

fn handle_input(input: &str) {
    match parse_command(input.trim()) {
        Ok(command_line) => {
            if let Err(msg) = handle_command_line(command_line) {
                eprintln!("{}", msg)
            }
        }
        Err(msg) => {
            eprintln!("{}", msg);
        }
    };
}

fn main() {
    loop {
        // Uncomment this block to pass the first stage
        print!("$ ");
        io::stdout().flush().unwrap();

        // Wait for user input
        let stdin = io::stdin();
        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();

        handle_input(input.as_str());
    }
}
