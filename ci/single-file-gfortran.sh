#!/usr/bin/env bash

output="${OUTPUT:-fpm-single-file.F90}"

args=("$@")
file=$(printf "%s\n" "${args[@]}" | grep -P '^.+\.[fF]90$')
if [ $? = 0 ]; then
  echo " + Appending source file '$file' to '${output}'"
  cat $file >> "${output}"
fi
exec gfortran "${args[@]}"
