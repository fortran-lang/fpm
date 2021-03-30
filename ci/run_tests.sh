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

pushd hello_world
"$fpm" build
"$fpm" run --target hello_world
"$fpm" run
popd

pushd hello_fpm
"$fpm" build
"$fpm" run --target hello_fpm
popd

pushd circular_test
"$fpm" build
popd

pushd circular_example
"$fpm" build
popd

pushd hello_complex
"$fpm" build
"$fpm" test
"$fpm" run --target say_Hello
"$fpm" run --target say_goodbye
"$fpm" test --target greet_test
"$fpm" test --target farewell_test
popd

pushd hello_complex_2
"$fpm" build
"$fpm" run --target say_hello_world
"$fpm" run --target say_goodbye
"$fpm" test --target greet_test
"$fpm" test --target farewell_test
popd

pushd with_examples
"$fpm" build
"$fpm" run --example --target demo-prog
"$fpm" run --target demo-prog
popd

pushd auto_discovery_off
"$fpm" build
"$fpm" run --target auto_discovery_off
"$fpm" test --target my_test
test ! -x ./build/gfortran_*/app/unused
test ! -x ./build/gfortran_*/test/unused_test
popd

pushd with_c
"$fpm" build
"$fpm" run --target with_c
popd

pushd submodules
"$fpm" build
popd

pushd program_with_module
"$fpm" build
"$fpm" run --target Program_with_module
popd

pushd link_executable
"$fpm" build
"$fpm" run --target gomp_test
popd

pushd fortran_includes
"$fpm" build
popd

pushd c_includes
"$fpm" build
popd

pushd c_header_only
"$fpm" build
popd

# Cleanup
rm -rf ./*/build
