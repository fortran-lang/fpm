# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Fortran Package Manager (fpm) is a package manager and build system for Fortran, modeled after Rust's Cargo. It handles building Fortran projects, running executables/tests/examples, and managing dependencies.

## Build Commands

fpm bootstraps itself - use an existing fpm installation to build:

```bash
# Build fpm (requires fpm v0.12.0+ installed)
fpm build

# Build with specific flags
fpm build --flag "-g -fbacktrace -O3"

# Run the built fpm
fpm run

# Run tests
fpm test                              # Run all test suites
fpm test --target fpm-test            # Run main unit test suite
fpm test --target cli-test            # Run CLI tests
fpm test --target new-test            # Run new command tests
fpm test --target help-test           # Run help tests

# Run a specific test suite within fpm-test
fpm run --target fpm-test -- fpm_manifest       # Run manifest tests
fpm run --target fpm-test -- fpm_compiler       # Run compiler tests
fpm run --target fpm-test -- fpm_filesystem     # Run filesystem tests

# Run a specific test within a suite
fpm run --target fpm-test -- fpm_manifest test_name

# Run integration tests on example packages
./ci/run_tests.sh ./build/gfortran_*/app/fpm

# Install a locally-built fpm to ./build/bin/fpm for use in subsequent tests
fpm install --prefix build
```

**IMPORTANT**: Do not use the system-wide `fpm` from the PATH for anything besides
bootstrapping itself. Do not try to search the build dir for `fpm` with wildcards
(like `./build/gfortran_*/app/fpm`) because multiple versions are cached. Instead:
1. Use `fpm install --prefix build` to install to `./build/bin/fpm`
2. Then use `./build/bin/fpm` for all testing and development work

## Code Architecture

### Entry Point
- `app/main.f90` - CLI entry point, dispatches to command handlers based on settings type

### Core Modules (src/)
- `fpm.f90` - Main orchestration: `cmd_build`, `cmd_run`, `cmd_clean`, `build_model`
- `fpm_command_line.f90` - CLI argument parsing using M_CLI2, defines all `fpm_*_settings` types
- `fpm_model.f90` - Build model types (`fpm_model_t`, `srcfile_t`) and scope constants
- `fpm_compiler.F90` - Compiler detection, flags, and command generation for gfortran, ifort, ifx, etc.
- `fpm_targets.f90` - Build target graph construction and dependency resolution
- `fpm_backend.F90` - Parallel build execution with job scheduling
- `fpm_sources.f90` - Source file discovery and classification
- `fpm_source_parsing.f90` - Fortran/C source parsing for module/dependency extraction

### Manifest Handling (src/fpm/)
- `manifest.f90` - Top-level manifest parsing
- `manifest/*.f90` - Individual manifest section parsers (package, dependency, library, executable, test, etc.)
- `toml.f90` - TOML file utilities using toml-f library

### Command Implementations (src/fpm/cmd/)
- `new.f90` - `fpm new` command
- `install.f90` - `fpm install` command
- `update.f90` - `fpm update` command
- `cmake.f90` - `fpm generate cmake` command (exports CMake build files)
- `publish.f90` - `fpm publish` command

### Metapackages (src/metapackage/)
System library integration for MPI, OpenMP, BLAS, HDF5, NetCDF, stdlib, minpack.

### Dependencies
Defined in `fpm.toml`:
- toml-f - TOML parsing
- M_CLI2 - Command line parsing
- jonquil - JSON handling
- fortran-regex - Regex support
- fortran-shlex - Shell lexing

## Testing Structure

Unit tests in `test/fpm_test/`:
- Tests organized by module (test_manifest.f90, test_compiler.f90, etc.)
- Each test module exports a `collect_*` procedure that registers tests
- Run specific suite: `fpm run --target fpm-test -- <suite_name>`

Integration tests: `example_packages/` contains ~70 test projects exercised by `ci/run_tests.sh`

## Code Style

Follow the [Fortran stdlib style guide](https://github.com/fortran-lang/stdlib/blob/master/STYLE_GUIDE.md).
