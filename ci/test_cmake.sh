#!/usr/bin/env bash

# Test "fpm generate --cmake" by generating cmake for each example package and
# then building with the generated CMakeLists.txt

#set -exu
set -u

build_failures=()
runtime_failures=()

for dir in example_packages/*/ ; do

	# These example(s) do not work with "fpm build" either, either intentionally
	# or a separate issue unrelated to cmake generation
	[[ "$dir" == "example_packages/features_with_dependency/" ]] && continue

	pushd "$dir"

	../../build/bin/fpm generate --cmake
	cmake -B temp_cmake_build -S .
	cmake --build temp_cmake_build --parallel
	if [[ $? -ne 0 ]] ; then
		build_failures+=("$dir")
	else
		# Find and run executables. Use maxdepth because dependencies like
		# test-drive have executable hooks not built by us that I don't want to
		# run
		exes=$(find temp_cmake_build -maxdepth 1 -type f -executable | grep -v '/CMakeFiles/')
		#exes=$(find temp_cmake_build -type f -executable | grep -v '/CMakeFiles/')
		for exe in $exes; do

			# TODO
			[[ "$dir" == "example_packages/metapackage_stdlib_extblas/" ]] && continue
			[[ "$dir" == "example_packages/preprocess_per_dependency/" ]] && continue

			#[[ "$dir" == "example_packages/dependency_priority/" ]] && continue  # hangs indefinitely
			[[ "$dir" == "example_packages/fpm_test_exit_code/" ]] && continue  # returns 1 on purpose

			if [[ -x "$exe" ]]; then
				"$exe"
				if [[ $? -ne 0 ]]; then
					runtime_failures+=("$dir : $(basename $exe)")
				fi
			fi
		done
	fi

	# Cleanup
	rm CMakeLists.txt
	rm -rf temp_cmake_build

	popd

done

echo "================================"
echo "Test Results Summary"
echo "================================"

if [[ ${#build_failures[@]} -eq 0 ]]; then
	echo "Build failures: none"
else
	echo "Build failures (${#build_failures[@]}):"
	printf '  %s\n' "${build_failures[@]}"
fi

echo ""

if [[ ${#runtime_failures[@]} -eq 0 ]]; then
	echo "Runtime failures: none"
else
	echo "Runtime failures (${#runtime_failures[@]}):"
	printf '  %s\n' "${runtime_failures[@]}"
fi

echo "================================"

total_failures=$((${#build_failures[@]} + ${#runtime_failures[@]}))
exit $total_failures

# TODO: test fpm self-generation too:
#
#     fpm run -- generate --cmake
#     cmake -B build -S .
#     cmake --build build --parallel
#

