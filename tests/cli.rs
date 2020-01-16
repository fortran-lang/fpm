use std::process::Command;  // Run programs
use assert_cmd::prelude::*; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions
use assert_cmd::assert::Assert;
#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;
use std::process::ExitStatus;

pub trait Success2 {
    // Our own function with better reporting of errors
    fn success2(self) -> Self;
}

#[cfg(unix)]
fn get_signal(status: ExitStatus) -> Option<i32> {
    status.signal()
}

#[cfg(not(unix))]
fn get_signal(_status: ExitStatus) -> Option<i32> {
    None
}

impl Success2 for Assert {
    fn success2(self) -> Self {
        if !self.get_output().status.success() {
            let code = self.get_output().status.code();
            if cfg!(unix) {
                if code.is_none() {
                    let signal = get_signal(self.get_output().status).unwrap();
                    panic!("INTERRUPTED with signal: {}", signal);
                }
            }
            let actual_code = code.unwrap();
            panic!("Non zero exit code: {}", actual_code);
        }
        self
    }
}

#[test]
fn test_help() {
    let mut cmd = Command::cargo_bin("fpm").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success2()
        .stdout(
            predicate::str::contains("--help       Prints help information"));
}


#[test]
fn test_1() {
    let mut build = Command::cargo_bin("fpm").unwrap();
    build.arg("build")
        .current_dir("tests/1");
    build.assert()
        .success2()
        .stdout(predicate::str::contains("Built target p1")
                .and(predicate::str::contains("TEST1 OK").not()));

    let mut run = Command::cargo_bin("fpm").unwrap();
    run.arg("run")
        .current_dir("tests/1");
    run.assert()
        .success2()
        .stdout(predicate::str::contains("TEST1 OK"));
}

#[test]
fn test_2() {
    let mut build = Command::cargo_bin("fpm").unwrap();
    build.arg("build")
        .current_dir("tests/2");
    build.assert()
        .success2()
        .stdout(predicate::str::contains("Built target p1")
                .and(predicate::str::contains("TEST2 OK").not()));

    let mut run = Command::cargo_bin("fpm").unwrap();
    run.arg("run")
        .current_dir("tests/2");
    run.assert()
        .success2()
        .stdout(predicate::str::contains("TEST2 OK"));
}
