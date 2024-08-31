use std::{
    env::{current_dir, set_current_dir},
    io::{self, Write},
    path::{Path, PathBuf},
};

fn parse_command(command: &str) -> (&str, Vec<&str>) {
    let mut parts = command.split_ascii_whitespace();
    let command = parts.next().unwrap();
    let args = parts.collect();
    (command, args)
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

fn handle_cmd_type(args: Vec<&str>) {
    for arg in args {
        match arg {
            "echo" | "exit" | "type" | "pwd" | "cd" => println!("{arg} is a shell builtin"),
            _ => match search_path(arg) {
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

fn handle_cmd_cd(args: Vec<&str>) {
    if args.len() > 1 {
        println!("cd: too many arguments...");
        return;
    }

    let current_dir = current_dir().expect("Unable to get current working directory");
    match args.first().map(|s| *s) {
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
    let (command, args) = parse_command(input);
    match command {
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
            if let Some(executable) = search_path(command) {
                match std::process::Command::new(executable).args(args).spawn() {
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
