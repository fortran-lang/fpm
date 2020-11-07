% fpm-build(1)

# NAME
fpm-build - Build a project with fpm


# SYNOPSIS
**fpm** **build** [--release] [--list] [--help] [--version]


# DESCRIPTION
Compile project and all of its dependencies.


# OPTIONS

## Compilation Options

**--release**
Build with high optimization instead of full debug options.

## General Options

**--list**
:   List candidates instead of building or running them

NOTE: Only supported in Fortran fpm

**--help**
:   Show help and exit

**--version**
:   Print program version information and exit

NOTE: Only supported in Fortran fpm


# EXAMPLES

1. Build a project and all of its dependencies:

   fpm build

2. Build a project with optimizations:

   fpm build --release


# SEE ALSO
fpm(1)
