# Fortran Package Manager

This is the repository of the Fortran Package Manager (*fpm*). If you are
looking for *fpm – packaging made simple* instead, see
[jordansissel/fpm](https://github.com/jordansissel/fpm).

Fortran Package Manager is an early prototype. You can use it to build and
package your Fortran projects, as well as to include supported Fortran
dependency projects. As a prototype, changes to *fpm*’s behavior and inputs may
occur as development continues. Please follow the
[issues](https://github.com/fortran-lang/fpm/issues) to contribute and/or stay
up to date with the development. As the prototype matures and we enter
production, we will do our best to stay backwards compatible.

To report a bug report or suggest a feature, please read our
[contributor guidelines](CONTRIBUTING.md).

## Getting started

### Install Haskell

To install **Haskell Stack**, follow these
[instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/),
users without superuser (admin) permissions should follow the
[manual installation](https://docs.haskellstack.org/en/stable/install_and_upgrade/#manual-download_2)
procedure.

### Download this repository

```bash
$ git clone https://github.com/fortran-lang/fpm
$ cd fpm/
```

### Build and test fpm

Bootstrap *fpm* using:

```bash
$ cd bootstrap/
$ stack build
```

To test:

```bash
$ stack test
```

To install:

```bash
$ stack install
```

On Linux, the above command installs `fpm` to `${HOME}/.local/bin/`.

### Creating a new project

Creating a new *fpm* project is as simple as running the command
`fpm new project_name`. This will create a new folder in your current directory
with the following contents and initialized as a git repository.

* `fpm.toml` – with your project’s name and some default standard meta-data
* `README.md` – with your project’s name
* `.gitgnore`
* `src/project_name.f90` – with a simple hello world subroutine
* `app/main.f90` (if `--with-executable` flag used) – a program that calls the subroutine
* `test/main.f90` (if `--with-test` flag used) – an empty test program

### Building your Fortran project with fpm

*fpm* understands the basic commands:

* `fpm build` – build your library, executables and tests
* `fpm run` – run executables
* `fpm test` – run tests

The command `fpm run` can optionally accept the name of the specific executable
to run, as can `fpm test`; like `fpm run specifc_executable`. Command line
arguments can also be passed to the executable(s) or test(s) with the option
`--args "some arguments"`.

See additional instructions in the [Packaging guide](PACKAGING.md).
