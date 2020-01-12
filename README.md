# Fortran Package Manager

A prototype version.

# How to try

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
