#!/usr/bin/env bash

# Test "fpm generate --cmake" by generating cmake for each example package and
# then building with the generated CMakeLists.txt

set -exu

for dir in example_packages/*/ ; do

	pushd "$dir"

	../../build/bin/fpm generate --cmake
	cmake -B temp_cmake_build -S .
	cmake --build temp_cmake_build --parallel

	# Cleanup
	rm CMakeLists.txt
	rm -rf temp_cmake_build

	popd

done

# TODO: test fpm self-generation too

