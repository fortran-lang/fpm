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

pushd nonintrinsic
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

pushd many_examples

"$fpm" run --example --all
test -e demo1.txt
test -e demo2.txt
popd

# Test building individual targets 
pushd many_targets
cases=( "1" "2" "3" )
targets=( "run" "example" "test" )
cmdrun=( "run --target" "run --example" "test --target" )
for j in {0..2}
do
   for i in {0..2}
   do
      rm -f *.txt
      this=${cases[$i]}	
      others=${cases[@]/$this}
      filename=${targets[$j]}$this
      echo "$filename"
      "$fpm" ${cmdrun[$j]} $filename 
      test -e $filename.txt
      # non-i-th tests should not have run
      for k in ${others[@]}
      do
         test ! -e ${targets[$k]}$k.txt   
      done
   done
done

# Test building all targets and with runner
if [[ "$(which time)" ]]; then
targets=( "run" "run --example" "test" )
names=( "run" "example" "test" )
cmdrun=( " " " --runner time" ) 
for j in {0..2}
do
  for i in {0..1}
  do
    rm -f *.txt
    "$fpm" ${targets[$j]}${cmdrun[$i]} 
    # all targets should have run
    for k in ${cases[@]}
    do
       test -e ${names[$j]}$k.txt   
    done
  done
done
fi
popd 


pushd auto_discovery_off
"$fpm" build
"$fpm" run --target auto_discovery_off
"$fpm" test --target my_test
test ! -x ./build/gfortran_*/app/unused
test ! -x ./build/gfortran_*/test/unused_test
popd

pushd auto_with_nondefault_main
"$fpm" build
"$fpm" install --prefix=./installed
test -x ./installed/bin/non_default_name
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

pushd preprocess_cpp_deps
"$fpm" build
popd

pushd preprocess_cpp_suffix
"$fpm" run
popd

pushd preprocess_per_dependency
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

# Test Fortran features
for feature in free-form fixed-form implicit-typing implicit-external
do
  pushd $feature
  "$fpm" run
  popd
done

# Test app exit codes
pushd fpm_test_exit_code
"$fpm" build

# odd number -> success!
EXIT_CODE=0
"$fpm" run -- 1 || EXIT_CODE=$?
test $EXIT_CODE -eq 0

# even number -> error 3
EXIT_CODE=0
"$fpm" run -- 512 || EXIT_CODE=$?
test $EXIT_CODE -eq 3

# even number -> error 3
EXIT_CODE=0
"$fpm" run -- 0 || EXIT_CODE=$?
test $EXIT_CODE -eq 3

# not an integer -> error 2
EXIT_CODE=0
"$fpm" run -- 3.1415 || EXIT_CODE=$?
test $EXIT_CODE -eq 2

# not a number -> error 2
EXIT_CODE=0
"$fpm" run -- notanumber || EXIT_CODE=$?
test $EXIT_CODE -eq 2

# no arguments -> error 1
EXIT_CODE=0
"$fpm" run || EXIT_CODE=$?
test $EXIT_CODE -eq 1
popd

# test dependency priority
pushd dependency_priority

# first build should run OK
EXIT_CODE=0
"$fpm" run || EXIT_CODE=$?
test $EXIT_CODE -eq 0

"$fpm" build --verbose

# Build again, should update nothing
"$fpm" build --verbose > build.log
if [[ -n "$(grep Update build.log)" ]]; then
  echo "Some dependencies were updated that should not be";
  exit 1;
fi

# Request update --clean, should update all dependencies
"$fpm" update --clean --verbose > update.log
if [[ -z "$(grep Update update.log)" ]]; then
  echo "No updated dependencies after 'fpm update --clean'";
  exit 1;
fi

# Test that no files are lost during multiple `install`s
# including overwriting the same install
"$fpm" install --prefix a
"$fpm" install --prefix a
"$fpm" install --prefix a
"$fpm" install --prefix b
"$fpm" install --prefix c

popd

# Cleanup
rm -rf ./*/build
