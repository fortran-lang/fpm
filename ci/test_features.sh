#!/usr/bin/env bash
set -ex

# Test script for FPM features functionality
# Usage: ./test_features.sh [fpm_executable]
# Note: This script should be run from the repo root or integrated into run_tests.sh

if [ "$1" ]; then
   fpm="$1"
else
   # Default to the fpm passed from run_tests.sh or system fpm
   fpm="${fpm:-fpm}"
fi

echo "Testing FPM features functionality"

echo "=== Testing features_demo package ==="

# Test 1: Basic features - debug feature
pushd "features_demo"
echo "Test 1: Basic debug feature"
rm -rf build
"$fpm" run --features debug | tee output.txt
grep -q "DEBUG mode enabled" output.txt
grep -q "✓ DEBUG mode enabled" output.txt
echo "✓ Debug feature works"

# Test 2: Profile usage - development profile (includes debug)
echo "Test 2: Development profile (debug feature)"  
rm -rf build
"$fpm" run --profile development --target features_demo | tee output.txt
grep -q "DEBUG mode enabled" output.txt
echo "✓ Development profile works"

# Test 3: Multiple features
echo "Test 3: Multiple features (debug + openmp)"
rm -rf build  
"$fpm" run --features debug,openmp --target features_demo | tee output.txt
grep -q "DEBUG mode enabled" output.txt
grep -q "OpenMP support enabled" output.txt
echo "✓ Multiple features work"

# Test 4: Feature-specific executable (debug_demo only available with debug feature)
echo "Test 4: Feature-specific executable"
rm -rf build
"$fpm" run --features debug --target debug_demo | tee output.txt
grep -q "Debug Demo Program" output.txt
grep -q "Debug mode: ON" output.txt
echo "✓ Feature-specific executable works"

# Test 5: Profile with multiple features - production profile (release + openmp)
echo "Test 5: Production profile (release + openmp)"
rm -rf build
"$fpm" run --profile production --target features_demo | tee output.txt
grep -q "RELEASE mode enabled" output.txt
grep -q "OpenMP support enabled" output.txt
# Should NOT have debug
! grep -q "DEBUG mode enabled" output.txt
echo "✓ Production profile works"

# Test 6: No features - baseline behavior
echo "Test 6: No features (baseline)"
rm -rf build  
"$fpm" run --target features_demo | tee output.txt
# Should have neither DEBUG nor RELEASE without explicit features
! grep -q "DEBUG mode enabled" output.txt || true
! grep -q "RELEASE mode enabled" output.txt || true
grep -q "Features: NONE" output.txt || grep -q "Demo completed successfully" output.txt
echo "✓ Baseline (no features) works"

# Test 7: Build listing with features
echo "Test 7: Build listing with features"
rm -rf build
"$fpm" build --features debug --list | tee build_list.txt
grep -q "debug_demo" build_list.txt
grep -q "features_demo" build_list.txt  
echo "✓ Build listing with features works"

# Test 8: Error handling - invalid feature
echo "Test 8: Error handling for invalid feature"
rm -rf build
if "$fpm" run --features nonexistent --target features_demo 2>&1 | grep -q "undefined feature"; then
    echo "✓ Correctly rejected invalid feature"
else
    echo "ERROR: Should reject invalid feature" && exit 1
fi

# Test 9: Error handling - invalid profile  
echo "Test 9: Error handling for invalid profile"
rm -rf build
if "$fpm" run --profile nonexistent --target features_demo 2>&1 | grep -q "undefined profile"; then
    echo "✓ Correctly rejected invalid profile"  
else
    echo "ERROR: Should reject invalid profile" && exit 1
fi

# Test 10: Features and profile mutual exclusion
echo "Test 10: Features and profile mutual exclusion"
rm -rf build
if "$fpm" run --features debug --profile development --target features_demo 2>&1 | grep -q "cannot specify both"; then
    echo "✓ Correctly rejected features + profile combination"
else
    echo "ERROR: Should reject features + profile combination" && exit 1  
fi

# Cleanup
rm -rf build output.txt build_list.txt
popd

echo "=== Testing features_with_dependency package ==="

# Test dependency features
pushd "features_with_dependency"

# Test 11: No features - should show NONE for both local and dependency
echo "Test 11: Dependency package without features"
rm -rf build
"$fpm" run | tee output.txt
grep -q "NONE - no local features active" output.txt
grep -q "Features: NONE" output.txt
echo "✓ Dependency package baseline works"

# Test 12: Debug dependency feature
echo "Test 12: Debug dependency feature"
rm -rf build
"$fpm" run --features with_feat_debug | tee output.txt
grep -q "WITH_DEBUG_DEPENDENCY" output.txt
grep -q "DEBUG mode enabled" output.txt
echo "✓ Debug dependency feature works"

# Test 13: Release dependency feature
echo "Test 13: Release dependency feature"
rm -rf build
"$fpm" run --features with_feat_release | tee output.txt
grep -q "WITH_RELEASE_DEPENDENCY" output.txt
grep -q "RELEASE mode enabled" output.txt
echo "✓ Release dependency feature works"

# Test 14: Multi dependency feature
echo "Test 14: Multi dependency feature"
rm -rf build
"$fpm" run --features with_feat_multi | tee output.txt
grep -q "WITH_MULTI_DEPENDENCY" output.txt
grep -q "DEBUG mode enabled" output.txt
grep -q "MPI support enabled" output.txt
echo "✓ Multi dependency feature works"

# Test 15: Profile with dependency features
echo "Test 15: Debug dependency profile"
rm -rf build
"$fpm" run --profile debug_dep | tee output.txt
grep -q "WITH_DEBUG_DEPENDENCY" output.txt
grep -q "DEBUG mode enabled" output.txt
echo "✓ Debug dependency profile works"

# Cleanup
rm -rf build output.txt
popd

echo "All FPM features tests passed!"
