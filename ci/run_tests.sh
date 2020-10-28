#!/bin/bash

get_abs_filename() {
  # $1 : relative filename
  filename=$1
  parentdir=$(dirname "${filename}")

  if [ -d "${filename}" ]; then
      echo "$(cd "${filename}" && pwd)"
  elif [ -d "${parentdir}" ]; then
    echo "$(cd "${parentdir}" && pwd)/$(basename "${filename}")"
  fi
}

set -ex

cd fpm
fpm build
fpm run
rm -rf fpm_scratch_*/
fpm test
rm -rf fpm_scratch_*/

f_fpm_path="$(get_abs_filename $(find build -regex 'build/.*/app/fpm'))"
"${f_fpm_path}"

cd ../example_packages/hello_world
"${f_fpm_path}" build
./build/gfortran_debug/app/hello_world

cd ../hello_fpm
"${f_fpm_path}" build
./build/gfortran_debug/app/hello_fpm

cd ../circular_test
"${f_fpm_path}" build

cd ../circular_example
"${f_fpm_path}" build

cd ../hello_complex
"${f_fpm_path}" build
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
