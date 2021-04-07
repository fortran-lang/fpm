#!/bin/sh

set -e # exit on error

usage()
{
    echo "Fortran Package Manager Bootstrap Script"
    echo ""
    echo "USAGE:"
    echo "./install.sh [--help | [--prefix=PREFIX]"
    echo ""
    echo " --help             Display this help text"
    echo " --prefix=PREFIX    Install binary in 'PREFIX/bin'"
    echo "                    Default prefix='\$HOME/.local/bin'"
    echo ""
    echo "FC and FFLAGS environment variables can be used to select the"
    echo "Fortran compiler and the build flags."
    echo ""
}

PREFIX="$HOME/.local"

while [ "$1" != "" ]; do
    PARAM=$(echo "$1" | awk -F= '{print $1}')
    VALUE=$(echo "$1" | awk -F= '{print $2}')
    case $PARAM in
        -h | --help)
            usage
            exit
            ;;
        --prefix)
            PREFIX=$VALUE
            ;;
        *)
            echo "ERROR: unknown parameter \"$PARAM\""
            usage
            exit 1
            ;;
    esac
    shift
done

set -u # error on use of undefined variable

SOURCE_URL="https://github.com/fortran-lang/fpm/releases/download/v0.2.0/fpm-0.2.0.f90"
BOOTSTRAP_DIR="build/bootstrap"
if [ -z ${FC+x} ]; then
    FC="gfortran"
fi
if [ -z ${FFLAGS+x} ]; then
    FFLAGS="-g -fbacktrace -O3"
fi

mkdir -p $BOOTSTRAP_DIR

if command -v curl > /dev/null 2>&1; then
  fetch=curl
  fetch_flag="-LJ"
elif command -v wget > /dev/null 2>&1; then
  fetch=wget
  fetch_flag="-O -"
else
  echo "No download mechanism found.  Tried curl and wget."
  exit 1
fi

$fetch ${fetch_flag:-} $SOURCE_URL > $BOOTSTRAP_DIR/fpm.f90

$FC $FFLAGS -J $BOOTSTRAP_DIR $BOOTSTRAP_DIR/fpm.f90 -o $BOOTSTRAP_DIR/fpm

$BOOTSTRAP_DIR/fpm install --compiler "$FC" --flag "$FFLAGS" --prefix "$PREFIX"
rm -r $BOOTSTRAP_DIR
