use std::{
    borrow::Cow,
    env::{current_dir, set_current_dir},
    io::{self, Write},
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
                    if in_single_quotes || in_double_quotes {
                        edited.as_mut().map(|s| s.push(c));
                        continue;
                    }

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

fn parse_command(command: &str) -> Result<(Cow<'_, str>, Vec<Cow<'_, str>>), String> {
    let mut parts = ReplIter {
        command,
        chars: command.char_indices(),
    };
    let Some(command) = parts.next() else {
        return Err(format!("Bad input: {}", command));
    };
    let command = command?;
    let args: Vec<Cow<'_, str>> = parts.collect::<Result<_, _>>()?;
    Ok((command, args))
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

fn handle_cmd_type(args: Vec<Cow<'_, str>>) {
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

fn handle_cmd_cd(args: Vec<Cow<'_, str>>) {
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

fn handle_input(input: &str) {
    let Ok((command, args)) = parse_command(input.trim()) else {
        return;
    };
    match command.as_ref() {
        "echo" => println!("{}", args.join(" ")),
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
            if let Some(_executable) = search_path(command.as_ref()) {
                match std::process::Command::new(command.as_ref())
                    .args(args.iter().map(|arg| arg.as_ref()))
                    .spawn()
                {
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
