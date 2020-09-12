#!/bin/bash

set -ex

cd fpm
fpm build
fpm run
fpm test
build/gfortran_debug/app/fpm

cd ../test/example_packages/hello_world
../../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/hello_world

cd ../hello_complex
../../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/say_Hello
./build/gfortran_debug/app/say_goodbye
./build/gfortran_debug/test/greet_test
./build/gfortran_debug/test/farewell_test