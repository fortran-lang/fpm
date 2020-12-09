#!/bin/bash
set -ex

cd $(dirname $0)/../fpm

fpm build $@

# Run fpm executable
fpm run $@
fpm run $@ -- --version
fpm run $@ -- --help

# Run tests
rm -rf fpm_scratch_*/
fpm test $@
rm -rf fpm_scratch_*/

# Build example packages
f_fpm_path="$(fpm run $@ --runner echo)"
cd ../example_packages/
rm -rf ./*/build

cd hello_world
"${f_fpm_path}" build
./build/gfortran_debug/app/hello_world
"${f_fpm_path}" run

cd ../hello_fpm
"${f_fpm_path}" build
./build/gfortran_debug/app/hello_fpm

cd ../circular_test
"${f_fpm_path}" build

cd ../circular_example
"${f_fpm_path}" build

cd ../hello_complex
"${f_fpm_path}" build
"${f_fpm_path}" test
./build/gfortran_debug/app/say_Hello
./build/gfortran_debug/app/say_goodbye
./build/gfortran_debug/test/greet_test
./build/gfortran_debug/test/farewell_test

cd ../hello_complex_2
"${f_fpm_path}" build
./build/gfortran_debug/app/say_hello_world
./build/gfortran_debug/app/say_goodbye
./build/gfortran_debug/test/greet_test
./build/gfortran_debug/test/farewell_test

cd ../with_examples
"${f_fpm_path}" build
./build/gfortran_debug/app/demo-prog

cd ../auto_discovery_off
"${f_fpm_path}" build
./build/gfortran_debug/app/auto_discovery_off
./build/gfortran_debug/test/my_test
test ! -x ./build/gfortran_debug/app/unused
test ! -x ./build/gfortran_debug/test/unused_test

cd ../with_c
"${f_fpm_path}" build
./build/gfortran_debug/app/with_c

cd ../submodules
"${f_fpm_path}" build

cd ../program_with_module
"${f_fpm_path}" build
./build/gfortran_debug/app/Program_with_module

cd ../link_external
"${f_fpm_path}" build
./build/gfortran_debug/app/link_external

cd ../link_executable
"${f_fpm_path}" build
./build/gfortran_debug/app/gomp_test

# Cleanup
rm -rf ./*/build
