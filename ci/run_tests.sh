#!/bin/bash

set -ex

cd fpm
fpm build
fpm run
build/gfortran_debug/app/fpm
cd ../test/example_packages/hello_world
../../../fpm/build/gfortran_debug/app/fpm build
./hello_world
