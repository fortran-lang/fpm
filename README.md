# Fortran Package Manager

A prototype version.

## How to try it out

Install Rust, then:
```
cargo build
```
Go to a test directory and execute:
```
cd tests/1
../../target/debug/fpm build
../../target/debug/fpm run
```
And the same in the other test directory (one can also call `run` directly):
```
cd tests/2
../../target/debug/fpm run
```

## Development Details

The command line interface (CLI) program `fpm` is build according to the
following Rust CLI tutorial:

https://rust-cli.github.io/book/index.html

and the user API is inspired by Cargo. Here is Cargo project layout:

https://doc.rust-lang.org/cargo/guide/project-layout.html
