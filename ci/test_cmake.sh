#!/usr/bin/env bash

# Test "fpm generate --cmake" by generating cmake for each example package and
# then building with the generated CMakeLists.txt

set -u

if [ $# -gt 0 ]; then
	fpm="$1"
else
	fpm=fpm
fi

build_failures=()
runtime_failures=()

for dir in example_packages/*/ ; do

	# These example(s) do not work with "fpm build" either, either intentionally
	# or a separate issue unrelated to cmake generation
	[[ "$dir" == "example_packages/features_with_dependency/" ]] && continue

	# These examples require external dependencies not available in CI/CD
	case "$dir" in
		example_packages/link_external/ | \
		example_packages/metapackage_blas/ | \
		example_packages/metapackage_hdf5/ | \
		example_packages/metapackage_mpi/ | \
		example_packages/metapackage_mpi_c/ | \
		example_packages/metapackage_mpi_cpp/ | \
		example_packages/metapackage_netcdf/ | \
		example_packages/metapackage_stdlib_extblas/)
			continue
			;;
	esac

	pushd "$dir"

	"$fpm" generate --cmake
	cmake -B temp_cmake_build -S .
	cmake --build temp_cmake_build --parallel
	if [[ $? -ne 0 ]] ; then
		build_failures+=("$dir")
	else
		# Find and run executables. Use maxdepth because dependencies like
		# test-drive have executable hooks not built by us that I don't want to
		# run
		exes=$(find temp_cmake_build -maxdepth 1 -type f -executable)
		for exe in $exes; do

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
	echo -e "Build failures: \033[0;32mnone\033[0m"
else
	echo -e "Build failures (\033[0;31m${#build_failures[@]}\033[0m):"
	printf '  %s\n' "${build_failures[@]}"
fi

echo ""

if [[ ${#runtime_failures[@]} -eq 0 ]]; then
	echo -e "Runtime failures: \033[0;32mnone\033[0m"
else
	echo -e "Runtime failures (\033[0;31m${#runtime_failures[@]}\033[0m):"
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

