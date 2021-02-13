#!/bin/sh

set -e # exit on error

usage()
{
    echo "Fortran Package Manager Bootstrap Script"
    echo ""
    echo "USAGE:"
    echo "./install.sh [--help | [--prefix=PREFIX] [--update[=REF]]"
    echo "               [--no-openmp] [--static] [--haskell] ]"
    echo ""
    echo " --help             Display this help text"
    echo " --prefix=PREFIX    Install binary in 'PREFIX/bin'"
    echo "                    Default prefix='\$HOME/.local/bin'"
    echo " --update[=REF]     Update repository from latest release tag"
    echo "                    or from git reference REF if specified"
    echo " --no-openmp        Don't build fpm with openmp support"
    echo " --static           Statically link fpm executable"
    echo "                     (implies --no-openmp)"
    echo " --haskell          Only install Haskell fpm"
    echo ""
    echo " '--no-openmp' and '--static' do not affect the Haskell fpm"
    echo " build."
    echo ""
}

PREFIX="$HOME/.local"
UPDATE=false
OMP=true
STATIC=false
HASKELL_ONLY=false

STACK_BIN_PATH="$HOME/.local/bin"
REF=$(git tag | tail -n1)
RELEASE_FLAGS="--flag -g --flag -fbacktrace --flag -O3"

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
        --update)
            UPDATE=true
            if [ "$VALUE" != "" ]; then
              REF=$VALUE
            fi
            ;;
        --no-openmp)
            OMP=false
            ;;
        --static)
            STATIC=true
            OMP=false
            ;;
        --haskell)
            HASKELL_ONLY=true
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

INSTALL_PATH="$PREFIX/bin"

if command -v stack 1> /dev/null 2>&1 ; then
  echo "Found stack"
else
  echo "Haskell stack not found."
  echo "Installing Haskell stack"
  curl -sSL https://get.haskellstack.org/ | sh
  if command -v stack 1> /dev/null 2>&1 ; then
    echo "Haskell stack installation successful."
  else
    echo "ERROR: Haskell stack installation unsuccessful."
    exit 1
  fi
fi

if [ -x "$INSTALL_PATH/fpm" ]; then
  echo "Overwriting existing fpm installation in $INSTALL_PATH"
fi

if [ "$UPDATE" = true ]; then
  git checkout "$REF"
  if [ $? != 0 ]; then
    echo "ERROR: Unable to checkout $REF."
    exit 1
  fi
fi

cd bootstrap
stack install

if [ "$STACK_BIN_PATH" != "$INSTALL_PATH" ]; then
  mv "$STACK_BIN_PATH/fpm" "$INSTALL_PATH/"
fi

if [ "$HASKELL_ONLY" = true ]; then
  exit
fi

if [ "$STATIC" = true ]; then
  RELEASE_FLAGS="$RELEASE_FLAGS --flag -static"
fi

if [ "$OMP" = true ]; then
  RELEASE_FLAGS="$RELEASE_FLAGS --flag -fopenmp"
fi

cd ../fpm
"$INSTALL_PATH/fpm" run $RELEASE_FLAGS --runner mv -- "$INSTALL_PATH/"

if [ -x "$INSTALL_PATH/fpm" ]; then
  echo "fpm installed successfully to $INSTALL_PATH"
else
  echo "ERROR: fpm installation unsuccessful: fpm not found in $INSTALL_PATH"
  exit 1
fi