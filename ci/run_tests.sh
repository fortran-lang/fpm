#!/bin/bash

set -ex

cd fpm
fpm build
fpm run
fpm test
build/gfortran_debug/app/fpm
cd ../test/example_packages/hello_world
../../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/hello_world
