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

# Return a download command
get_fetch_command()
{
    if command -v curl > /dev/null 2>&1; then
        echo "curl -L"
    elif command -v wget > /dev/null 2>&1; then
        echo "wget -O -"
    else
        echo "No download mechanism found. Install curl or wget first."
        return 1
    fi
}

# Return value of the latest published release on GitHub, with no heading "v" (e.g., "0.7.0")
get_latest_release()
{
     $2 "https://api.github.com/repos/$1/releases/latest" | # Get latest release from GitHub api
     grep '"tag_name":'        |                            # Get tag line
     sed -E 's/.*"([^"]+)".*/\1/' |                         # Pluck JSON value
     sed -E 's/^v//'                                        # Remove heading "v" if present
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

# Get download command
FETCH=$(get_fetch_command)
if [ $? -ne 0 ]; then
  echo "No download mechanism found. Install curl or wget first."
  exit 2
fi

# Use 0.8.0 too bootstrap
BOOTSTRAP_RELEASE="0.8.0"
SOURCE_URL="https://github.com/fortran-lang/fpm/releases/download/v${BOOTSTRAP_RELEASE}/fpm-${BOOTSTRAP_RELEASE}.F90"
BOOTSTRAP_DIR="build/bootstrap"

if [ -z ${FC+x} ]; then
    FC="gfortran"
fi
if [ -z ${FFLAGS+x} ]; then
    FFLAGS="-g -fbacktrace -O3"
fi

mkdir -p $BOOTSTRAP_DIR

$FETCH $SOURCE_URL > $BOOTSTRAP_DIR/fpm.F90

SAVEDIR="$(pwd)"
cd $BOOTSTRAP_DIR
$FC $FFLAGS fpm.F90 -o fpm
cd "$SAVEDIR"

$BOOTSTRAP_DIR/fpm update
$BOOTSTRAP_DIR/fpm install --compiler "$FC" --flag "$FFLAGS" --prefix "$PREFIX"
rm -r $BOOTSTRAP_DIR
