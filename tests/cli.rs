use std::process::Command;  // Run programs
use assert_cmd::prelude::*; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions

#[test]
fn test_help() {
    let mut cmd = Command::cargo_bin("fpm").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(
            predicate::str::contains("--help       Prints help information"));
}

#[test]
fn test_1() {
    let mut build = Command::cargo_bin("fpm").unwrap();
    build.arg("build")
        .current_dir("tests/1");
    build.assert()
        .success()
        .stdout(predicate::str::contains("Built target p1")
                .and(predicate::str::contains("TEST1 OK").not()));

    let mut run = Command::cargo_bin("fpm").unwrap();
    run.arg("run")
        .current_dir("tests/1");
    run.assert()
        .success()
        .stdout(predicate::str::contains("TEST1 OK"));
}

#[test]
fn test_2() {
    let mut build = Command::cargo_bin("fpm").unwrap();
    build.arg("build")
        .current_dir("tests/2");
    build.assert()
        .success()
        .stdout(predicate::str::contains("Built target p1")
                .and(predicate::str::contains("TEST2 OK").not()));

    let mut run = Command::cargo_bin("fpm").unwrap();
    run.arg("run")
        .current_dir("tests/2");
    run.assert()
        .success()
        .stdout(predicate::str::contains("TEST2 OK"));
}
