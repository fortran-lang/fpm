<p align="center">
    <a href="https://fpm.fortran-lang.org/"><img src="https://raw.githubusercontent.com/fortran-lang/assets/main/fpm/logo/2-color-alt/png/full-color-alt.png" width="800"/></a>
</p>

# [Fortran Package Manager](https://fpm.fortran-lang.org/)

Fortran Package Manager (fpm) is a package manager and build system for Fortran.
Its key goal is to improve the user experience of Fortran programmers.
It does so by making it easier to build your Fortran program or library, run the
executables, tests, and examples, and distribute it as a dependency to other
Fortran projects.
Fpm's user interface is modeled after [Rust's Cargo](https://doc.rust-lang.org/cargo/),
so if you're familiar with that tool, you will feel at home with fpm.
Fpm's long term vision is to nurture and grow the ecosystem of modern Fortran
applications and libraries.

Fpm is an early prototype and is evolving rapidly.
You can use it to build and package your Fortran projects, as well as to use
[existing fpm packages](https://github.com/fortran-lang/fpm-registry) as dependencies.
Fpm's behavior and user interface may change as it evolves, however as fpm
matures and we enter production, we will aim to stay backwards compatible.
Please follow the [issues](https://github.com/fortran-lang/fpm/issues) to
contribute and/or stay up to date with the development.
Before opening a bug report or a feature suggestion, please read our
[Contributor Guide](CONTRIBUTING.md). You can also discuss your ideas and
queries with the community in
[fpm discussions](https://github.com/fortran-lang/fpm/discussions),
or more broadly on [Fortran-Lang Discourse](https://fortran-lang.discourse.group/).

Fortran Package Manager is not to be confused with
[Jordan Sissel's fpm](https://github.com/jordansissel/fpm), a more general,
non-Fortran related package manager.

**Website: <https://fpm.fortran-lang.org/>**

## [Download](https://fpm.fortran-lang.org/en/how-to/installation.html)

Fpm is available on many platforms and through multiple package managers, see our Documentation
webpage for a list of **[All Supported Installations](https://fpm.fortran-lang.org/en/how-to/installation.html)**.

The easiest installation routes are shown below.

### [Conda]

Fpm is available on [conda-forge], to add `conda-forge` to your channels use:

```sh
conda config --add channels conda-forge
```

Fpm can be installed with:

```sh
conda create -n fpm fpm
conda activate fpm
```

The conda package manager can be installed from [miniforge](https://github.com/conda-forge/miniforge/releases)
or from [miniconda](https://docs.conda.io/en/latest/miniconda.html).

[Conda]: https://conda.io
[conda-forge]: https://conda-forge.org/

### [Homebrew](https://brew.sh/)

The Fortran Package Manager (fpm) is available for the [Homebrew](https://brew.sh/) package manager via an additional tap.
To install fpm via brew, include the new tap and install using

```sh
brew tap fortran-lang/fortran
brew install fpm
```

Binary distributions are available for MacOS 11 (Catalina) and 12 (Big Sur) for x86_64 architectures. For other platforms fpm will be built locally from source automatically.

Fpm should be available and functional after those steps.
For more details checkout the tap [here](https://github.com/fortran-lang/homebrew-fortran).

## [Get started](https://fpm.fortran-lang.org/en/tutorial/index.html)

**Follow our [Quickstart Tutorial](https://fpm.fortran-lang.org/en/tutorial/hello-fpm.html) to get familiar with fpm**.

### Start a new project

Creating a new *fpm* project is as simple as running the command
`fpm new project_name`. This will create a new folder in your current directory
with the following contents and initialized as a git repository.

* `fpm.toml` – with your project’s name and some default standard meta-data
* `README.md` – with your project’s name
* `.gitignore`
* `src/project_name.f90` – with a simple hello world subroutine
* `app/main.f90` (if `--app` flag used) – a program that calls the subroutine
* `test/main.f90` (if `--test` flag used) – an empty test program

### Building your Fortran project with fpm

*fpm* understands the basic commands:

* `fpm build` – build your library, executables and tests
* `fpm run` – run executables
* `fpm test` – run tests
* `fpm install` - installs the executables locally

The command `fpm run` can optionally accept the name of the specific executable
to run, as can `fpm test`; like `fpm run specific_executable`. Command line
arguments can also be passed to the executable(s) or test(s) with the option
`-- some arguments`.

See additional instructions in the [Packaging guide](PACKAGING.md) or
the [manifest reference](https://fpm.fortran-lang.org/en/spec/manifest.html).

<!-- 
### Bootstrapping instructions

This guide explains the process of building *fpm* on a platform for the first time.
To build *fpm* without a prior *fpm* version a single source file version is available
at each release.

To build manually using the single source distribution, run the following code (from within the current directory)

```sh
mkdir _tmp
curl -LJ https://github.com/fortran-lang/fpm/releases/download/current/fpm.F90 > _tmp/fpm.F90
gfortran -J _tmp _tmp/fpm.F90 -o _tmp/fpm
_tmp/fpm install --flag "-g -fbacktrace -O3"
rm -r _tmp
```

To automatically bootstrap using this appoach run the install script

```sh
./install.sh
``` -->

## Environmental variables

The table below lists the environment variables that control `fpm`'s choice of compilers, 
compiler flags, archiver locations, and link flags, each of which can be overridden by 
passing `fpm` flags also shown in the table.

| Environment Variable | Defines               | Overridden by  |
| :------------------- | :-------------------- | :------------- |
| `FPM_FC`             | Fortran compiler path | `--compiler`   |
| `FPM_CC`             | C compiler path       | `--c-compiler` |
| `FPM_FFLAGS`         | Fortran compiler flags| `--flag`       |
| `FPM_CFLAGS`         | C compiler flags      | `--c-flag`     |
| `FPM_AR`             | Archiver path         | `--archiver`   |
| `FPM_LDFLAGS`        | Link flags            | `--link-flag`  |
