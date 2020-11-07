% fpm-new(1)

# NAME
fpm-new - Create a new project with fpm


# SYNOPSIS
**fpm** **new** [_options_] _name_

# DESCRIPTION

IMPORTANT: Fortran fpm and Bootstrap fpm behaviour is different for this command.

The default behaviour is to create a project with a library in `src/name.f90`.
Fortran fpm creates by default also executables in `app/main.f90` and `test/main.f90`.


# OPTIONS

## New Options

**--lib**, **--src**
:   Create a project with a library in `src/name.f90`, where _name_ is replaced
    by the project name.

NOTE: Only supported in Fortran fpm

**--app**
:   Create a project with an executable in app/main.f90

NOTE: Only supported in Fortran fpm

**--test**
:   Create a project with an executable in test/main.f90

NOTE: Only supported in Fortran fpm

**--backfill**
:   Recreate project, but keeps existing project files.

NOTE: Only supported in Fortran fpm

**--with-executable**
:   Create a project with an executable in app/main.f90

NOTE: Only supported in Bootstrap fpm

**--with-test**
:   Create a project with an executable in test/main.f90

NOTE: Only supported in Bootstrap fpm


# EXAMPLES

1. Create a new project in a given directory

   fpm new myproject


# SEE ALSO
fpm(1)
