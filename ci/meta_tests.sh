#!/usr/bin/env bash
set -ex

# ***********************
# This script tests all example packages using any metapackage/system dependencies
# ***********************

cd "$(dirname $0)/.."

if [ "$1" ]; then
   fpm="$1"
else
   fpm=fpm
fi

# Build example packages
pushd example_packages/
rm -rf ./*/build

pushd metapackage_openmp
"$fpm" build --verbose
"$fpm" run --verbose
popd

pushd metapackage_stdlib
"$fpm" build --verbose
"$fpm" run --verbose
popd

pushd metapackage_minpack
"$fpm" build --verbose
"$fpm" run --verbose
popd

pushd metapackage_mpi
"$fpm" build --verbose 
"$fpm" run --verbose
popd

pushd metapackage_mpi_c
"$fpm" build --verbose 
"$fpm" run --verbose
popd

pushd metapackage_hdf5
"$fpm" build --verbose 
"$fpm" run --verbose
popd

# Cleanup
rm -rf ./*/build
