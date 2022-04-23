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

# pushd and popd is a bash built-in feature
# Custom pushd and popd implementation for shells other than bash
# Not an actual stack!

my_pushd()
{
    export SAVEDIR=$(pwd)
    cd "$1"
}

my_popd()
{
    cd "$SAVEDIR"
    unset -v SAVEDIR
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

SOURCE_URL="https://github.com/fortran-lang/fpm/releases/download/v0.5.0/fpm-0.5.0.F90"
BOOTSTRAP_DIR="build/bootstrap"
if [ -z ${FC+x} ]; then
    FC="gfortran"
fi
if [ -z ${FFLAGS+x} ]; then
    FFLAGS="-g -fbacktrace -O3"
fi

mkdir -p $BOOTSTRAP_DIR

if command -v curl > /dev/null 2>&1; then
    FETCH="curl -L"
elif command -v wget > /dev/null 2>&1; then
    FETCH="wget -O -"
else
    echo "No download mechanism found. Install curl or wget first."
    exit 1
fi

$FETCH $SOURCE_URL > $BOOTSTRAP_DIR/fpm.F90

if command -v pushd > /dev/null 2>&1; then
    PUSHD=pushd
    POPD=popd
else
    PUSHD=my_pushd
    POPD=my_popd
fi

$PUSHD $BOOTSTRAP_DIR
    $FC $FFLAGS fpm.F90 -o fpm
$POPD

$BOOTSTRAP_DIR/fpm update
$BOOTSTRAP_DIR/fpm install --compiler "$FC" --flag "$FFLAGS" --prefix "$PREFIX"
rm -r $BOOTSTRAP_DIR
