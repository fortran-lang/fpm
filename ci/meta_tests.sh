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
"$fpm" build
"$fpm" run 
popd

pushd metapackage_stdlib
"$fpm" build
"$fpm" run 
popd

pushd metapackage_mpi
"$fpm" build
"$fpm" run 
popd

# Cleanup
rm -rf ./*/build
