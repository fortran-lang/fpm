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

f_fpm_path="$(fpm run $@ --runner echo)"

# Let fpm build itself
"${f_fpm_path}" build

# Install fpm into local directory
"${f_fpm_path}" install --prefix "$PWD/_dist" --no-rebuild

# Build example packages
cd ../example_packages/
rm -rf ./*/build

cd hello_world

"${f_fpm_path}" build
"${f_fpm_path}" run --target hello_world
"${f_fpm_path}" run

cd ../hello_fpm
"${f_fpm_path}" build
"${f_fpm_path}" run --target hello_fpm

cd ../circular_test
"${f_fpm_path}" build

cd ../circular_example
"${f_fpm_path}" build

cd ../hello_complex
"${f_fpm_path}" build
"${f_fpm_path}" test
"${f_fpm_path}" run --target say_Hello
"${f_fpm_path}" run --target say_goodbye
"${f_fpm_path}" test --target greet_test
"${f_fpm_path}" test --target farewell_test

cd ../hello_complex_2
"${f_fpm_path}" build
"${f_fpm_path}" run --target say_hello_world
"${f_fpm_path}" run --target say_goodbye
"${f_fpm_path}" test --target greet_test
"${f_fpm_path}" test --target farewell_test

cd ../with_examples
"${f_fpm_path}" build
"${f_fpm_path}" run --example --target demo-prog
"${f_fpm_path}" run --target demo-prog

cd ../auto_discovery_off
"${f_fpm_path}" build
"${f_fpm_path}" run --target auto_discovery_off
"${f_fpm_path}" test --target my_test
test ! -x ./build/gfortran_*/app/unused
test ! -x ./build/gfortran_*/test/unused_test

cd ../with_c
"${f_fpm_path}" build
"${f_fpm_path}" run --target with_c

cd ../submodules
"${f_fpm_path}" build

cd ../program_with_module
"${f_fpm_path}" build
"${f_fpm_path}" run --target Program_with_module

cd ../link_external
"${f_fpm_path}" build
"${f_fpm_path}" run --target link_external

cd ../link_executable
"${f_fpm_path}" build
"${f_fpm_path}" run --target gomp_test

cd ../fortran_includes
"${f_fpm_path}" build

cd ../c_includes
"${f_fpm_path}" build

cd ../c_header_only
"${f_fpm_path}" build

# Cleanup
rm -rf ./*/build
