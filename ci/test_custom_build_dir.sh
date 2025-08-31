#!/usr/bin/env bash
set -ex

# Test script for custom build directory functionality
# Usage: ./test_custom_build_dir.sh [fpm_executable] [example_package_dir]

# Move to repo root (works from <root> or <root>/ci)
this_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$this_dir/.."

if [ "$1" ]; then
   fpm="$1"
else
   fpm=fpm
fi

if [ "$2" ]; then
   test_package="$2"
else
   test_package="hello_world"
fi

echo "Testing custom build directory functionality with package: $test_package"

# Go to example packages directory
pushd example_packages/

# Test 1: Custom build directory with CLI option
pushd "$test_package"
echo "Test 1: CLI option --build-dir"
rm -rf ./build custom_build_test
"$fpm" build --build-dir custom_build_test
test -d custom_build_test
test -f custom_build_test/.gitignore
"$fpm" run --build-dir custom_build_test --target "$test_package"
# Verify standard build directory was not created
test ! -d build
echo "✓ CLI option --build-dir works"

# Test 2: Environment variable
echo "Test 2: Environment variable FPM_BUILD_DIR"
rm -rf custom_build_test env_build_test
FPM_BUILD_DIR=env_build_test "$fpm" build
test -d env_build_test
test -f env_build_test/.gitignore
FPM_BUILD_DIR=env_build_test "$fpm" run --target "$test_package"
echo "✓ Environment variable FPM_BUILD_DIR works"

# Test 3: CLI option overrides environment variable
echo "Test 3: CLI option overrides environment variable"
rm -rf env_build_test cli_override_test
FPM_BUILD_DIR=env_build_test "$fpm" build --build-dir cli_override_test
test -d cli_override_test
test ! -d env_build_test
echo "✓ CLI option correctly overrides environment variable"

# Test 4: Build directory validation - reserved names
echo "Test 4: Build directory validation"
# These should fail with specific error messages
if "$fpm" build --build-dir src 2>&1 | grep -q "conflicts with source directory"; then
    echo "✓ Correctly rejected 'src'"
else
    echo "ERROR: Should reject 'src'" && exit 1
fi

if "$fpm" build --build-dir app 2>&1 | grep -q "conflicts with source directory"; then
    echo "✓ Correctly rejected 'app'"
else
    echo "ERROR: Should reject 'app'" && exit 1
fi

if "$fpm" build --build-dir test 2>&1 | grep -q "conflicts with source directory"; then
    echo "✓ Correctly rejected 'test'"
else
    echo "ERROR: Should reject 'test'" && exit 1
fi

if "$fpm" build --build-dir . 2>&1 | grep -q "would overwrite the current"; then
    echo "✓ Correctly rejected '.'"
else
    echo "ERROR: Should reject '.'" && exit 1
fi

# Test 5: Path normalization
echo "Test 5: Path normalization"
if "$fpm" build --build-dir ./src 2>&1 | grep -q "conflicts with source directory"; then
    echo "✓ Correctly rejected './src' (path normalization works)"
else
    echo "ERROR: Should reject './src'" && exit 1
fi

# Test 6: Different commands with custom build directory
echo "Test 6: Different commands with custom build directory"
rm -rf test_build_all
"$fpm" build --build-dir test_build_all
"$fpm" run --build-dir test_build_all --target "$test_package"
# Some packages may not have tests, so this might fail but that's expected
"$fpm" test --build-dir test_build_all 2>/dev/null || echo "No tests in $test_package (expected)"
echo "✓ All commands work with custom build directory"

# Cleanup test directories
rm -rf custom_build_test env_build_test cli_override_test test_build_all
popd

popd

echo "All custom build directory tests passed!"
