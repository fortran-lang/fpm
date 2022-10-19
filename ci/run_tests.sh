#!/usr/bin/env bash
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
"$fpm" -C $dir/app run

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

pushd tree_shake
"$fpm" build
"$fpm" run
"$fpm" test
test ! -e ./build/gfortran_*/tree_shake/src_farewell_m.f90.o
test ! -e ./build/gfortran_*/tree_shake/src_farewell_m.f90.o.log
popd

pushd submodule_tree_shake
"$fpm" run
test ! -e ./build/gfortran_*/submodule_tree_shake/src_parent_unused.f90.o
test ! -e ./build/gfortran_*/submodule_tree_shake/src_parent_unused.f90.o.log
test ! -e ./build/gfortran_*/submodule_tree_shake/src_child_unused.f90.o
test ! -e ./build/gfortran_*/submodule_tree_shake/src_child_unused.f90.o.log
popd

pushd version_file
"$fpm" build
"$fpm" run
popd

pushd with_c
"$fpm" build
"$fpm" run --target with_c
popd

pushd submodules
"$fpm" build
popd

pushd app_with_submodule
"$fpm" run --all
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

pushd c_main
"$fpm" run
popd

pushd c_main_preprocess
"$fpm" build --c-flag "-DVAL"
popd

pushd app_with_c
"$fpm" run
popd

pushd hello_fpm_path
"$fpm" run
popd

pushd preprocess_cpp
"$fpm" build
popd

pushd preprocess_cpp_c
"$fpm" run
popd

pushd preprocess_hello
"$fpm" build
popd

pushd fpm_test_exe_issues
"$fpm" build
popd

pushd cpp_files
"$fpm" test
popd

# Cleanup
rm -rf ./*/build
