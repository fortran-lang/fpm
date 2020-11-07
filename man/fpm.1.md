% fpm(1) Fortran Package Manager

# NAME
fpm - Fortran package manager and build system


# SYNOPSIS

**fpm** [--version] [--help] [--list]

**fpm** _command_ [_args_]


# DESCRIPTION

This program is a package manager and build system for Fortran projects.


# COMMANDS

## Build Commands

**fpm-build**(1)
:   Compile the project.

**fpm-run**(1)
:   Run a binary or example of the project.

**fpm-test**(1)
:   Execute unit and integration tests of the project.


## Package Commands

**fpm-new**(1)
:   Create a new fpm project.


## General Commands

**fpm-help**(1)
:   Display help information about fpm.

NOTE: Only supported in Fortran fpm

**fpm-list**(1)
:   Display brief descriptions of all commands.

NOTE: Only supported in Fortran fpm


# OPTIONS

**--version**
:   Print version information and exit.

NOTE: Only supported in Fortran fpm

**--list**
:   Display brief descriptions of all commands.

NOTE: Only supported in Fortran fpm

**-h,--help**
:   Print help information.

NOTE: **-h** flag only supported in Bootstrap fpm


# EXAMPLES

1. Build a project and all of its dependencies:

   fpm build

2. Build a project with optimizations:

   fpm build --release

3. Run unit tests in with debug options:

   fpm test

4. Create a new project in a given directory

   fpm new myproject


# BUGS

See https://github.com/fortran-lang/fpm/issues to report issues.
