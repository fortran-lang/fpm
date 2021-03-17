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

### Setting up fpm

#### Binary download
`x86-64` binaries are available [to download](https://github.com/fortran-lang/fpm/releases) for Windows, MacOS and Linux.

__Note:__ On Linux and MacOS, you will need to enable executable permission before you can use the binary.

_e.g._ `$ chmod u+x fpm-v0.1.0-linux-x86_64`

#### Conda

If you haven't yet, first enable the conda-forge channel:

```
conda config --add channels conda-forge
```

Fpm can be installed with:

```
conda create -n fpm fpm
conda activate fpm
```

#### Github Actions

To setup *fpm* within Github actions for automated testing, you can use the [fortran-lang/setup-fpm](https://github.com/marketplace/actions/setup-fpm) action.

#### Bootstraping on other platforms

For other platforms and architectures have a look at the [bootstrapping instructions](#bootstrapping-instructions).

### Creating a new project

Creating a new *fpm* project is as simple as running the command
`fpm new project_name`. This will create a new folder in your current directory
with the following contents and initialized as a git repository.

* `fpm.toml` – with your project’s name and some default standard meta-data
* `README.md` – with your project’s name
* `.gitignore`
* `src/project_name.f90` – with a simple hello world subroutine
* `app/main.f90` (if `--with-executable` flag used) – a program that calls the subroutine
* `test/main.f90` (if `--with-test` flag used) – an empty test program

### Building your Fortran project with fpm

*fpm* understands the basic commands:

* `fpm build` – build your library, executables and tests
* `fpm run` – run executables
* `fpm test` – run tests

The command `fpm run` can optionally accept the name of the specific executable
to run, as can `fpm test`; like `fpm run specific_executable`. Command line
arguments can also be passed to the executable(s) or test(s) with the option
`-- some arguments`.

See additional instructions in the [Packaging guide](PACKAGING.md) or
the [manifest reference](manifest-reference.md).


### Bootstrapping instructions

This guide explains the process of building *fpm* on a platform for the first time.
If your platform and architecture are already supported, download the binary from the [release page](https://github.com/fortran-lang/fpm/releases) instead.

#### Download this repository

```bash
$ git clone https://github.com/fortran-lang/fpm
$ cd fpm/
```

#### Build a bootstrap version of fpm

You can use the install script to bootstrap and install *fpm*:

```bash
$ ./install.sh
```

By default, the above command installs `fpm` to `${HOME}/.local/bin/`.
To specify an alternative destination use the `--prefix=` flag, for example:

```bash
$ ./install.sh --prefix=/usr/local
```

which will install *fpm* to `/usr/local/bin`.

To test that everything is working as expected you can now build *fpm*
with itself and run the tests with:

```bash
$ cd fpm
$ fpm test
```

