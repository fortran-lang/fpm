#!/usr/bin/env bash

# Test "fpm generate --cmake" by generating cmake for each example package and
# then building with the generated CMakeLists.txt

#set -exu
set -u

failures=()

for dir in example_packages/*/ ; do

	# TODO
	[[ "$dir" == "example_packages/preprocess_hello/" ]] && continue
	[[ "$dir" == "example_packages/preprocess_hello_dependency/" ]] && continue
	[[ "$dir" == "example_packages/program_with_cpp_guarded_module/" ]] && continue
	[[ "$dir" == "example_packages/shared_app_only/" ]] && continue
	[[ "$dir" == "example_packages/static_app_only/" ]] && continue

	# These example(s) do not work with "fpm build" either, either intentionally
	# or a separate issue unrelated to cmake generation
	[[ "$dir" == "example_packages/features_with_dependency/" ]] && continue

	pushd "$dir"

	../../build/bin/fpm generate --cmake
	cmake -B temp_cmake_build -S .
	cmake --build temp_cmake_build --parallel
	if [[ $? -ne 0 ]] ; then
		failures+=("$dir")
	fi

	# Cleanup
	rm CMakeLists.txt
	rm -rf temp_cmake_build

	popd

done

echo "failures = "
echo "${failures[@]}"

# TODO: test fpm self-generation too:
#
#     fpm run -- generate --cmake
#     cmake -B build -S .
#     cmake --build build --parallel
#

