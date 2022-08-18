#!/usr/bin/env bash

set -ex

# Target directory for saving preprocessed project.
destdir="${DESTDIR:-build}"

# Get fypp preprocessor
fypp="${FYPP:-$(which fypp)}"

# Number of parallel jobs for preprocessing
if [ $(uname) = "Darwin" ]; then
  njob="$(sysctl -n hw.ncpu)"
else
  njob="$(nproc)"
fi

# Create directory for saving preprocessed files.
mkdir -p "$destdir/preprocessed_files"

# Preprocess all files with .fypp extension in source directory.
# Save generated files in build directory.
find src -iname "*.fypp" \
    -exec basename {} .fypp ';' \
    | cut -f1 -d. | xargs -P "$njob" -I{} "$fypp" "src/{}.fypp" "$destdir/preprocessed_files/{}.f90" \

args=("$@")

exec gfortran "${args[@]}"


