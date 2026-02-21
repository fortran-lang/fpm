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

# Detect Windows/MSYS2 and set CMake generator accordingly
CMAKE_GENERATOR_FLAG=""
if [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "win32" ]] || [[ -n "${MSYSTEM:-}" ]]; then
	# On Windows with MSYS2/MinGW, use MinGW Makefiles generator for Fortran support
	CMAKE_GENERATOR_FLAG="-G \"MinGW Makefiles\""
fi

for dir in example_packages/*/ ; do

	# Skip examples that don't work with cmake generation
	case "$dir" in

		# Does not work with "fpm build" either (intentionally or separate issue)
		example_packages/features_with_dependency/ )
			continue
			;;

		# Require external dependencies not available in CI/CD
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
	if [[ $? -ne 0 ]] ; then
		build_failures+=("$dir (fpm generate failed)")
		popd
		continue
	fi
	if [[ -n "${CMAKE_GENERATOR_FLAG}" ]]; then
		eval cmake ${CMAKE_GENERATOR_FLAG} -B temp_cmake_build -S .
	else
		cmake -B temp_cmake_build -S .
	fi
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
	rm -f CMakeLists.txt
	rm -rf temp_cmake_build
	# Clean up generated CMakeLists.txt in dependency directories
	find build/dependencies -name CMakeLists.txt -delete 2>/dev/null || true

	popd

done

echo "================================"
echo "Testing fpm self-generation with CMake"
echo "================================"

# Ensure dependencies are fetched first
"$fpm" build
if [[ $? -ne 0 ]] ; then
    build_failures+=("fpm self-generation (fpm build failed)")
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
fi

# Generate CMakeLists.txt for fpm itself
"$fpm" generate --cmake
if [[ $? -ne 0 ]] ; then
    build_failures+=("fpm self-generation (generate --cmake failed)")
    # Cleanup and exit
    rm -f CMakeLists.txt
    find build/dependencies -name CMakeLists.txt -delete 2>/dev/null || true

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
fi

# Configure and build with CMake
if [[ -n "${CMAKE_GENERATOR_FLAG}" ]]; then
    eval cmake ${CMAKE_GENERATOR_FLAG} -B temp_self_cmake_build -S .
else
    cmake -B temp_self_cmake_build -S .
fi
cmake --build temp_self_cmake_build --parallel
if [[ $? -ne 0 ]] ; then
    build_failures+=("fpm self-generation (cmake build failed)")
    # Cleanup
    rm -f CMakeLists.txt
    rm -rf temp_self_cmake_build
    find build/dependencies -name CMakeLists.txt -delete 2>/dev/null || true

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
fi

# Run the CMake-built test suites
echo "Running CMake-built test suites..."
for test_exe in temp_self_cmake_build/fpm-test temp_self_cmake_build/cli-test ; do
    if [[ -x "$test_exe" ]]; then
        "$test_exe"
        if [[ $? -ne 0 ]]; then
            runtime_failures+=("fpm self-generation : $(basename $test_exe)")
        fi
    fi
done

# Cleanup
rm -f CMakeLists.txt
rm -rf temp_self_cmake_build
find build/dependencies -name CMakeLists.txt -delete 2>/dev/null || true

echo "Self-generation test completed"

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
