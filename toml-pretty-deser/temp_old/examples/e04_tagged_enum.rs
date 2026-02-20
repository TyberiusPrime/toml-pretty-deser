//! # Example 04: Tagged enums
//!
//! Demonstrates `#[tpd(tag = "...")]` for enums whose variants carry struct data.
//! The tag field in TOML determines which variant to deserialize.

use toml_pretty_deser::prelude::*;

#[tpd(no_verify)]
#[derive(Debug)]
struct HttpAction {
    url: String,
    method: String,
}

#[tpd(no_verify)]
#[derive(Debug)]
struct ShellAction {
    command: String,
}

#[tpd(tag = "type")]
#[derive(Debug)]
enum Action {
    Http(HttpAction),
    Shell(ShellAction),
}

#[tpd(root, no_verify)]
#[derive(Debug)]
struct Job {
    name: String,
    #[tpd(nested)]
    action: Action,
}

fn main() {
    let toml_str = r#"
name = "deploy"
[action]
type = "Http"
url = "https://api.example.com/deploy"
method = "POST"
"#;

    match Job::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(job) => {
            println!("Job '{}': {:?}", job.name, job.action);
        }
        Err(e) => eprintln!("{}", e.pretty("job.toml")),
    }

    let toml_str2 = r#"
name = "cleanup"
[action]
type = "Shell"
command = "rm -rf /tmp/build"
"#;

    match Job::tpd_from_toml(toml_str2, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(job) => {
            println!("Job '{}': {:?}", job.name, job.action);
            match job.action {
                Action::Http(http) => println!("HTTP action: {} {}", http.method, http.url),
                Action::Shell(shell) => println!("Shell action: {}", shell.command),
            }
        }
        Err(e) => eprintln!("{}", e.pretty("job.toml")),
    }
}
