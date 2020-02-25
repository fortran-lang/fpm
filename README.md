# Fortran Package Manager

A prototype version.

## How to try it out

### Install Haskell

Install Haskell Stack from (https://haskellstack.org/). E.g., to install on
Linux without root access, follow the [manual download](https://docs.haskellstack.org/en/stable/install_and_upgrade/#manual-download_2):
```
wget https://get.haskellstack.org/stable/linux-x86_64-static.tar.gz
tar xaf linux-x86_64-static.tar.gz
```
and put the `stack` binary in your path, for example by:
```
export PATH="$PATH:`pwd`/stack-2.1.3-linux-x86_64-static/"
```

### Build and Test FPM

Build using:
```
stack build
```
To test:
```
stack test
```
