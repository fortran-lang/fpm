# Fortran Package Manager

A prototype version.

## Getting started

### Install Haskell

To install [Haskell Stack](https://haskellstack.org/) on
Linux without root access, follow the [manual download](https://docs.haskellstack.org/en/stable/install_and_upgrade/#manual-download_2) procedure:
```
wget https://get.haskellstack.org/stable/linux-x86_64-static.tar.gz
tar xaf linux-x86_64-static.tar.gz
```
and put the `stack` binary in your path, for example:
```
export PATH="$PATH:`pwd`/stack-2.1.3-linux-x86_64-static/"
```

### Download this repository

```
git clone https://github.com/fortran-lang/fpm
cd fpm
```

### Build and Test FPM

Build FPM using:
```
stack build
```
To test:
```
stack test
```
To install:
```
stack install
```

On Linux, the above command installs `stack` to `${HOME}/.local/bin`.

### Building your Fortran project with FPM

Please follow the instructions in the [Packaging guide](PACKAGING.md).
