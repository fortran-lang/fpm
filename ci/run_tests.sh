#!/bin/bash

set -ex

cd fpm
fpm build
fpm run
rm -rf fpm_scratch_*/
fpm test
rm -rf fpm_scratch_*/
build/gfortran_debug/app/fpm

cd ../example_packages/hello_world
../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/hello_world

cd ../hello_fpm
../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/hello_fpm

cd ../circular_test
../../fpm/build/gfortran_debug/app/fpm build

cd ../circular_example
../../fpm/build/gfortran_debug/app/fpm build

cd ../hello_complex
../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/say_Hello
./build/gfortran_debug/app/say_goodbye
./build/gfortran_debug/test/greet_test
./build/gfortran_debug/test/farewell_test

cd ../hello_complex_2
../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/say_hello_world
./build/gfortran_debug/app/say_goodbye
./build/gfortran_debug/test/greet_test
./build/gfortran_debug/test/farewell_test

cd ../auto_discovery_off
../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/auto_discovery_off
./build/gfortran_debug/test/my_test
test ! -x ./build/gfortran_debug/app/unused
test ! -x ./build/gfortran_debug/test/unused_test

cd ../with_c
../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/with_c

cd ../submodules
../../fpm/build/gfortran_debug/app/fpm build

cd ../program_with_module
../../fpm/build/gfortran_debug/app/fpm build
./build/gfortran_debug/app/Program_with_module
