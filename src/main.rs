#[allow(unused_imports)]
use std::io::{self, Write};

fn handle_input(input: &str) {
    let mut parts = input.trim().split_ascii_whitespace();
    let command = parts.next();
    match command {
        Some(command) => println!("{command}: command not found"),
        None => (),
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
