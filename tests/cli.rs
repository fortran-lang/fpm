use std::process::Command;  // Run programs
use assert_cmd::prelude::*; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions
use assert_cmd::assert::Assert;
use std::os::unix::process::ExitStatusExt;

// Our own function, so that we can better debug the failure
fn success(self0: Assert) -> Assert {
    if !self0.get_output().status.success() {
        let code = self0.get_output().status.code();
        if code.is_none() {
            let signal = self0.get_output().status.signal().unwrap();
            panic!("INTERRUPTED with signal: {}", signal);
        }
        let actual_code = code.unwrap();
        panic!("Non zero exit code: {}", actual_code);
    }
    self0
}

#[test]
fn test_help() {
    let mut cmd = Command::cargo_bin("fpm").unwrap();
    cmd.arg("--help");
    success(cmd.assert())
        .stdout(
            predicate::str::contains("--help       Prints help information"));
}


#[test]
fn test_1() {
    let mut build = Command::cargo_bin("fpm").unwrap();
    build.arg("build")
        .current_dir("tests/1");
    success(build.assert())
        .stdout(predicate::str::contains("Built target p1")
                .and(predicate::str::contains("TEST1 OK").not()));

    let mut run = Command::cargo_bin("fpm").unwrap();
    run.arg("run")
        .current_dir("tests/1");
    success(run.assert())
        .stdout(predicate::str::contains("TEST1 OK"));
}

#[test]
fn test_2() {
    let mut build = Command::cargo_bin("fpm").unwrap();
    build.arg("build")
        .current_dir("tests/2");
    success(build.assert())
        .stdout(predicate::str::contains("Built target p1")
                .and(predicate::str::contains("TEST2 OK").not()));

    let mut run = Command::cargo_bin("fpm").unwrap();
    run.arg("run")
        .current_dir("tests/2");
    success(run.assert())
        .stdout(predicate::str::contains("TEST2 OK"));
}
