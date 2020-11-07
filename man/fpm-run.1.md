% fpm-run(1)

# NAME
fpm-run - Run the project application

# SYNOPSIS
**fpm** **run** [_options_] [_target_...] [-- _args_]

# DESCRIPTION

Run an application of the current project.
The target name corresponds to an executable defined in the package manifest.
Multiple executables can be selected.

All arguments following the two dashes (--) are passed to the selected application.


# OPTIONS

**--runner** _CMD_
:   A command to prefi the program execution paths with.

**--release**
:   Use targets with high optimization instead of full debug options.

**--list**
:   List all possible targets instead of running them


# EXAMPLES

1. Build the current project and run the main application

   fpm run

2. Run several applications in release mode

   fpm run --release prg1 prg2

3. Pass additional arguments to the selected application

   fpm run reader -- ./examples/package.toml

4. Run an application using `gdb(1)` to debug

   fpm run --runner gdb


# SEE ALSO
fpm(1), fpm-build(1)
