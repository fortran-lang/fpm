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
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled"; exit 1; }
echo "✓ Debug feature works"

# Test 2: Profile usage - development profile (includes debug)
echo "Test 2: Development profile (debug feature)"  
rm -rf build
"$fpm" run --profile development --target features_demo | tee output.txt
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled in development profile"; exit 1; }
echo "✓ Development profile works"

# Test 3: Multiple features
echo "Test 3: Multiple features (debug + openmp)"
rm -rf build  
"$fpm" run --features debug,openmp --target features_demo | tee output.txt
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled with multiple features"; exit 1; }
grep -q "OpenMP support enabled" output.txt || { echo "ERROR: OpenMP not enabled with multiple features"; exit 1; }
echo "✓ Multiple features work"

# Test 4: Feature-specific executable (debug_demo only available with debug feature)
echo "Test 4: Feature-specific executable"
rm -rf build
"$fpm" run --features debug --target debug_demo | tee output.txt
grep -q "Debug Demo Program" output.txt || { echo "ERROR: Debug Demo Program not found"; exit 1; }
grep -q "Debug mode: ON" output.txt || { echo "ERROR: Debug mode not ON in debug_demo"; exit 1; }
echo "✓ Feature-specific executable works"

# Test 5: Profile with multiple features - production profile (release + openmp)
echo "Test 5: Production profile (release + openmp)"
rm -rf build
"$fpm" run --profile production --target features_demo | tee output.txt
grep -q "RELEASE mode enabled" output.txt || { echo "ERROR: RELEASE mode not enabled in production profile"; exit 1; }
grep -q "OpenMP support enabled" output.txt || { echo "ERROR: OpenMP not enabled in production profile"; exit 1; }
# Should NOT have debug
if grep -q "DEBUG mode enabled" output.txt; then
    echo "ERROR: DEBUG mode should not be enabled in production profile"
    exit 1
fi
echo "✓ Production profile works"

# Test 6: No features - baseline behavior
echo "Test 6: No features (baseline)"
rm -rf build  
"$fpm" run --target features_demo | tee output.txt
# Should have neither DEBUG nor RELEASE without explicit features
if grep -q "DEBUG mode enabled" output.txt; then
    echo "ERROR: DEBUG mode should not be enabled in baseline"
    exit 1
fi
if grep -q "RELEASE mode enabled" output.txt; then
    echo "ERROR: RELEASE mode should not be enabled in baseline"
    exit 1
fi
if ! grep -q "Features: NONE" output.txt && ! grep -q "Demo completed successfully" output.txt; then
    echo "ERROR: Expected baseline features output not found"
    exit 1
fi
echo "✓ Baseline (no features) works"

# Test 7: Error handling - invalid feature
echo "Test 7: Error handling for invalid feature"
rm -rf build
if ! "$fpm" run --features nonexistent --target features_demo > /dev/null 2>&1; then
    echo "Correctly rejected invalid feature"
else
    echo "ERROR: Should reject invalid feature" && exit 1
fi

# Test 8: Error handling - invalid profile  
echo "Test 8: Error handling for invalid profile"
rm -rf build
if ! "$fpm" run --profile nonexistent --target features_demo > /dev/null 2>&1; then
    echo "Correctly rejected invalid profile"  
else
    echo "ERROR: Should reject invalid profile" && exit 1
fi

# Test 9: Features and profile mutual exclusion
echo "Test 9: Features and profile mutual exclusion"
rm -rf build
if ! "$fpm" run --features debug --profile development --target features_demo > /dev/null 2>&1; then
    echo "Correctly rejected features + profile combination"
else
    echo "ERROR: Should reject features + profile combination" && exit 1  
fi

# Cleanup
rm -rf build output.txt build_list.txt
popd

echo "=== Testing features_with_dependency package ==="

# Test dependency features
pushd "features_with_dependency"

# Test 10: No features - should show NONE for both local and dependency
echo "Test 10: Dependency package without features"
rm -rf build
"$fpm" run | tee output.txt
grep -q "NONE - no local features active" output.txt || { echo "ERROR: Local features NONE message not found"; exit 1; }
grep -q "Features: NONE" output.txt || { echo "ERROR: Features NONE not found in dependency test"; exit 1; }
echo "✓ Dependency package baseline works"

# Test 11: Debug dependency feature
echo "Test 11: Debug dependency feature"
rm -rf build
"$fpm" run --features with_feat_debug | tee output.txt
grep -q "WITH_DEBUG_DEPENDENCY" output.txt || { echo "ERROR: WITH_DEBUG_DEPENDENCY not found"; exit 1; }
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled in dependency test"; exit 1; }
echo "✓ Debug dependency feature works"

# Test 12: Release dependency feature
echo "Test 12: Release dependency feature"
rm -rf build
"$fpm" run --features with_feat_release | tee output.txt
grep -q "WITH_RELEASE_DEPENDENCY" output.txt || { echo "ERROR: WITH_RELEASE_DEPENDENCY not found"; exit 1; }
grep -q "RELEASE mode enabled" output.txt || { echo "ERROR: RELEASE mode not enabled in dependency test"; exit 1; }
echo "✓ Release dependency feature works"

# Test 13: Multi dependency feature
echo "Test 13: Multi dependency feature"
rm -rf build
"$fpm" run --features with_feat_multi | tee output.txt
grep -q "WITH_MULTI_DEPENDENCY" output.txt || { echo "ERROR: WITH_MULTI_DEPENDENCY not found"; exit 1; }
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled in multi dependency test"; exit 1; }
grep -q "MPI support enabled" output.txt || { echo "ERROR: MPI support not enabled in multi dependency test"; exit 1; }
echo "✓ Multi dependency feature works"

# Test 14: Profile with dependency features
echo "Test 14: Debug dependency profile"
rm -rf build
"$fpm" run --profile debug_dep | tee output.txt
grep -q "WITH_DEBUG_DEPENDENCY" output.txt || { echo "ERROR: WITH_DEBUG_DEPENDENCY not found in profile test"; exit 1; }
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled in dependency profile test"; exit 1; }
echo "✓ Debug dependency profile works"

# Cleanup
rm -rf build output.txt
popd

echo "All FPM features tests passed!"
