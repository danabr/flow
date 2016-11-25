extern crate time;

use std::env;
use std::path;
use std::io::{Read, Write};

enum Action {
    Complete,
    Fail,
    Help,
    Start,
}

fn main() {
    match args_to_action(env::args()) {
        Action::Complete => complete(),
        Action::Fail => fail(),
        Action::Help => help(),
        Action::Start => start(),
    }
}

fn fail() {
    match std::fs::remove_file(flow_path()) {
        Ok(_) => (),
        Err(error) => {
            if error.kind() != std::io::ErrorKind::NotFound {
                panic!("Could not remove flow file!")
            }
        }
    }
}

fn help() {
    println!("Usage:");
    println!("flow [start] - start a new flow");
    println!("flow d[one]  - record a succesful flow");
    println!("flow f[ail]  - fail the current flow");
}

fn start() {
    let path = flow_path();
    match std::fs::OpenOptions::new().write(true).create_new(true).open(path) {
        Ok(mut file) => {
            let now = time::now_utc();
            let formatted = time_to_str(&now);
            match file.write_fmt(format_args!("{}", formatted)) {
                Ok(_) => println!("Flow started."),
                Err(_) => println!("Failed to write flow file."),
            }
        }
        Err(error) => {
            if error.kind() == std::io::ErrorKind::AlreadyExists {
                println!("A flow has already been started.");
            } else {
                println!("Failed to create flow file!");
            }
        }
    }
}

fn complete() {
    let flow_path = flow_path();
    match std::fs::File::open(&flow_path) {
        Ok(mut file) => {
            let mut s = String::new();
            file.read_to_string(&mut s).expect("Failed to read flow file!");
            let start_str = s.trim();
            let fmt = "%Y-%m-%dT%H:%M:%S%z";
            match time::strptime(&s, fmt) {
                Ok(start) => {
                    let stop = time::now_utc();
                    let duration = (stop - start).num_seconds();
                    match std::fs::OpenOptions::new()
                        .create(true)
                        .append(true)
                        .open(ledger_path()) {
                        Ok(mut ledge_file) => {
                            let formatted = time_to_str(&stop);
                            match ledge_file.write_fmt(format_args!("{},{},{}\n",
                                                                    start_str,
                                                                    formatted,
                                                                    duration)) {
                                Ok(_) => {
                                    std::fs::remove_file(flow_path)
                                        .expect("Failed to remove flow file!");
                                    println!("Flow lasted {} seconds.", duration);
                                }
                                Err(_) => println!("Failed to write flow file."),
                            }
                        }
                        Err(_) => println!("Failed to open ledger file"),
                    }
                }
                Err(_) => println!("Failed to parse flow file!"),
            }
        }
        Err(_) => println!("Failed to open flow path!"),
    }
}

fn time_to_str(time: &time::Tm) -> String {
    let fmt = "%Y-%m-%dT%H:%M:%S%z";
    match time::strftime(fmt, &time) {
        Ok(formatted) => formatted,
        Err(_) => panic!("Failed to parse time!"),
    }
}

fn args_to_action(mut args: std::env::Args) -> Action {
    args.next(); // Skip first argument (program name)
    match args.next() {
        None => Action::Start,
        Some(string) => {
            match string.as_ref() {
                "d" => Action::Complete,
                "done" => Action::Complete,
                "f" => Action::Fail,
                "fail" => Action::Fail,
                "start" => Action::Start,
                _ => Action::Help,
            }
        }
    }
}

fn flow_path() -> std::ffi::OsString {
    match std::env::home_dir() {
        None => {
            let mut path = path::PathBuf::new();
            path.push("tmp");
            path.push(".flow");
            path.into_os_string()
        }
        Some(mut path) => {
            path.push(".flow");
            path.into_os_string()
        }
    }
}

fn ledger_path() -> std::ffi::OsString {
    match std::env::home_dir() {
        None => {
            let mut path = path::PathBuf::new();
            path.push("tmp");
            path.push("flow_ledger");
            path.into_os_string()
        }
        Some(mut path) => {
            path.push("flow_ledger.csv");
            path.into_os_string()
        }
    }
}
