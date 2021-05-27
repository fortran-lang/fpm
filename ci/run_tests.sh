#!/bin/bash
set -ex

cd "$(dirname $0)/.."

if [ "$1" ]; then
   fpm="$1"
else
   fpm=fpm
fi

# Build example packages
pushd example_packages/
rm -rf ./*/build

dir=hello_world
"$fpm" -C $dir build
"$fpm" -C $dir run --target hello_world
"$fpm" -C $dir run

dir=hello_fpm
"$fpm" -C $dir build
"$fpm" -C $dir run --target hello_fpm

dir=circular_test
"$fpm" -C $dir build

dir=circular_example
"$fpm" -C $dir build

dir=hello_complex
"$fpm" -C $dir build
"$fpm" -C $dir test
"$fpm" -C $dir run --target say_Hello
"$fpm" -C $dir run --target say_goodbye
"$fpm" -C $dir test --target greet_test
"$fpm" -C $dir test --target farewell_test

dir=hello_complex_2
"$fpm" -C $dir build
"$fpm" -C $dir run --target say_hello_world
"$fpm" -C $dir run --target say_goodbye
"$fpm" -C $dir test --target greet_test
"$fpm" -C $dir test --target farewell_test

dir=with_examples
"$fpm" -C $dir build
"$fpm" -C $dir run --example --target demo-prog
"$fpm" -C $dir run --target demo-prog

dir=auto_discovery_off
"$fpm" -C $dir build
"$fpm" -C $dir run --target auto_discovery_off
"$fpm" -C $dir test --target my_test
test ! -x $dir/build/gfortran_*/app/unused
test ! -x $dir/build/gfortran_*/test/unused_test

dir=with_c
"$fpm" -C $dir build
"$fpm" -C $dir run --target with_c

"$fpm" -C $dir build

dir=program_with_module
"$fpm" -C $dir build
"$fpm" -C $dir run --target Program_with_module

dir=link_executable
"$fpm" -C $dir build
"$fpm" -C $dir run --target gomp_test

dir=fortran_includes
"$fpm" -C $dir build

dir=c_includes
"$fpm" -C $dir build

dir=c_header_only
"$fpm" -C $dir build

# Cleanup
rm -rf ./*/build
