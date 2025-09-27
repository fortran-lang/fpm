#!/usr/bin/env bash
set -exo pipefail

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
"$fpm" run --features debug > output.txt
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled"; exit 1; }
echo "✓ Debug feature works"

# Test 2: Profile usage - development profile (includes debug)
echo "Test 2: Development profile (debug feature)"
rm -rf build
"$fpm" run --profile development --target features_demo > output.txt
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled in development profile"; exit 1; }
echo "✓ Development profile works"

# Test 3: Multiple features
echo "Test 3: Multiple features (debug + openmp)"
rm -rf build
"$fpm" run --features debug,openmp --target features_demo > output.txt
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled with multiple features"; exit 1; }
grep -q "OpenMP support enabled" output.txt || { echo "ERROR: OpenMP not enabled with multiple features"; exit 1; }
echo "✓ Multiple features work"

# Test 4: Feature-specific executable (debug_demo only available with debug feature)
echo "Test 4: Feature-specific executable"
rm -rf build
"$fpm" run --features debug --target debug_demo > output.txt
grep -q "Debug Demo Program" output.txt || { echo "ERROR: Debug Demo Program not found"; exit 1; }
grep -q "Debug mode: ON" output.txt || { echo "ERROR: Debug mode not ON in debug_demo"; exit 1; }
echo "✓ Feature-specific executable works"

# Test 5: Profile with multiple features - production profile (release + openmp)
echo "Test 5: Production profile (release + openmp)"
rm -rf build
"$fpm" run --profile production --target features_demo > output.txt
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
"$fpm" run --target features_demo > output.txt
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

# RE-ENABLE AFTER MERGING fpm WITH CPP PARSING PR
# Test 10: No features - should show NONE for both local and dependency
# echo "Test 10: Dependency package without features"
rm -rf build
#"$fpm" run | tee output.txt
# grep -q "NONE - no local features active" output.txt || { echo "ERROR: Local features NONE message not found"; exit 1; }
# grep -q "Features: NONE" output.txt || { echo "ERROR: Features NONE not found in dependency test"; exit 1; }
# echo "✓ Dependency package baseline works"

# Test 11: Debug dependency feature
echo "Test 11: Debug dependency feature"
rm -rf build
"$fpm" run --features with_feat_debug > output.txt
grep -q "WITH_DEBUG_DEPENDENCY" output.txt || { echo "ERROR: WITH_DEBUG_DEPENDENCY not found"; exit 1; }
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled in dependency test"; exit 1; }
echo "✓ Debug dependency feature works"

# Test 12: Release dependency feature
echo "Test 12: Release dependency feature"
rm -rf build
"$fpm" run --features with_feat_release > output.txt
grep -q "WITH_RELEASE_DEPENDENCY" output.txt || { echo "ERROR: WITH_RELEASE_DEPENDENCY not found"; exit 1; }
grep -q "RELEASE mode enabled" output.txt || { echo "ERROR: RELEASE mode not enabled in dependency test"; exit 1; }
echo "✓ Release dependency feature works"

# Test 13: Multi dependency feature
echo "Test 13: Multi dependency feature"
rm -rf build
"$fpm" run --features with_feat_multi > output.txt
grep -q "WITH_MULTI_DEPENDENCY" output.txt || { echo "ERROR: WITH_MULTI_DEPENDENCY not found"; exit 1; }
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled in multi dependency test"; exit 1; }
grep -q "MPI support enabled" output.txt || { echo "ERROR: MPI support not enabled in multi dependency test"; exit 1; }
echo "✓ Multi dependency feature works"

# Test 14: Profile with dependency features
echo "Test 14: Debug dependency profile"
rm -rf build
"$fpm" run --profile debug_dep > output.txt
grep -q "WITH_DEBUG_DEPENDENCY" output.txt || { echo "ERROR: WITH_DEBUG_DEPENDENCY not found in profile test"; exit 1; }
grep -q "DEBUG mode enabled" output.txt || { echo "ERROR: DEBUG mode not enabled in dependency profile test"; exit 1; }
echo "✓ Debug dependency profile works"

# Cleanup
rm -rf build output.txt
popd

echo "=== Testing features_per_compiler package ==="

# Test features per compiler package
pushd "features_per_compiler"

# Test 15: Development profile (debug + verbose)
echo "Test 15: Features per compiler - development profile"
rm -rf build
if "$fpm" run --profile development > output.txt; then
    echo "✓ Exit code 0 (success) as expected"
else
    echo "ERROR: Expected exit code 0 but got non-zero exit code"
    exit 1
fi
grep -q "Features Per Compiler Demo" output.txt || { echo "ERROR: Features Per Compiler Demo not found"; exit 1; }
grep -q "✓ DEBUG: -g flag found" output.txt || { echo "ERROR: Debug feature not detected"; exit 1; }
grep -q "✓ VERBOSE: -v flag found" output.txt || { echo "ERROR: Verbose feature not detected"; exit 1; }
grep -q "✓ All compiler flag checks PASSED" output.txt || { echo "ERROR: Expected all checks to pass"; exit 1; }
# Check compiler-specific flags (will depend on detected compiler)
if grep -q "Detected compiler: gfortran" output.txt; then
    grep -q "✓ Debug: -Wall found" output.txt || { echo "ERROR: gfortran debug flag -Wall not found"; exit 1; }
    grep -q "✓ Debug: -fcheck=bounds found" output.txt || { echo "ERROR: gfortran debug flag -fcheck=bounds not found"; exit 1; }
fi
echo "✓ Development profile works"

# Test 16: Production profile (release + fast)
echo "Test 16: Features per compiler - production profile"
rm -rf build
if "$fpm" run --profile production > output.txt; then
    echo "✓ Exit code 0 (success) as expected"
else
    echo "ERROR: Expected exit code 0 but got non-zero exit code"
    exit 1
fi
grep -q "Features Per Compiler Demo" output.txt || { echo "ERROR: Features Per Compiler Demo not found"; exit 1; }
grep -q "✓ RELEASE: -O flags found" output.txt || { echo "ERROR: Release feature not detected"; exit 1; }
grep -q "✓ FAST: fast optimization flags found" output.txt || { echo "ERROR: Fast feature not detected"; exit 1; }
grep -q "✓ All compiler flag checks PASSED" output.txt || { echo "ERROR: Expected all checks to pass"; exit 1; }
# Check compiler-specific flags (will depend on detected compiler)
if grep -q "Detected compiler: gfortran" output.txt; then
    # Check for either -march=native or -mcpu (Apple Silicon uses -mcpu)
    if ! (grep -q "✓ Release: -march=native found" output.txt || grep -q "✓ Release: -mcpu found" output.txt); then
        echo "ERROR: gfortran release architecture flag (-march=native or -mcpu) not found"
        exit 1
    fi
    grep -q "✓ Fast: -ffast-math found" output.txt || { echo "ERROR: gfortran fast flag -ffast-math not found"; exit 1; }
fi
echo "✓ Production profile works"

# Test 17: Testing profile (debug + strict)
echo "Test 17: Features per compiler - testing profile"
rm -rf build
if "$fpm" run --profile testing > output.txt; then
    echo "✓ Exit code 0 (success) as expected"
else
    echo "ERROR: Expected exit code 0 but got non-zero exit code"
    exit 1
fi
grep -q "Features Per Compiler Demo" output.txt || { echo "ERROR: Features Per Compiler Demo not found"; exit 1; }
grep -q "✓ DEBUG: -g flag found" output.txt || { echo "ERROR: Debug feature not detected"; exit 1; }
grep -q "✓ STRICT: standard compliance flags found" output.txt || { echo "ERROR: Strict feature not detected"; exit 1; }
grep -q "✓ All compiler flag checks PASSED" output.txt || { echo "ERROR: Expected all checks to pass"; exit 1; }
# Check compiler-specific flags (will depend on detected compiler)
if grep -q "Detected compiler: gfortran" output.txt; then
    grep -q "✓ Strict: -Wpedantic found" output.txt || { echo "ERROR: gfortran strict flag -Wpedantic not found"; exit 1; }
fi
echo "✓ Testing profile works"

# Test 18: Individual features - debug only
echo "Test 18: Features per compiler - debug feature only"
rm -rf build
if "$fpm" run --features debug > output.txt; then
    echo "✓ Exit code 0 (success) as expected"
else
    echo "ERROR: Expected exit code 0 but got non-zero exit code"
    exit 1
fi
grep -q "Features Per Compiler Demo" output.txt || { echo "ERROR: Features Per Compiler Demo not found"; exit 1; }
grep -q "✓ DEBUG: -g flag found" output.txt || { echo "ERROR: Debug feature not detected"; exit 1; }
grep -q "✓ All compiler flag checks PASSED" output.txt || { echo "ERROR: Expected all checks to pass"; exit 1; }
# Should NOT have release or fast flags
if grep -q "✓ RELEASE: -O flags found" output.txt; then
    echo "ERROR: Release flags should not be present with debug only"
    exit 1
fi
echo "✓ Debug feature works"

# Test 19: Individual features - release only
echo "Test 19: Features per compiler - release feature only"
rm -rf build
if "$fpm" run --features release > output.txt; then
    echo "✓ Exit code 0 (success) as expected"
else
    echo "ERROR: Expected exit code 0 but got non-zero exit code"
    exit 1
fi
grep -q "Features Per Compiler Demo" output.txt || { echo "ERROR: Features Per Compiler Demo not found"; exit 1; }
grep -q "✓ RELEASE: -O flags found" output.txt || { echo "ERROR: Release feature not detected"; exit 1; }
grep -q "✓ All compiler flag checks PASSED" output.txt || { echo "ERROR: Expected all checks to pass"; exit 1; }
# Should NOT have debug flags
if grep -q "✓ DEBUG: -g flag found" output.txt; then
    echo "ERROR: Debug flags should not be present with release only"
    exit 1
fi
echo "✓ Release feature works"

# Test 20: No profile/features - baseline
echo "Test 20: Features per compiler - baseline (no profile)"
rm -rf build
if "$fpm" run > output.txt; then
    echo "✓ Exit code 0 (success) as expected"
else
    echo "ERROR: Expected exit code 0 but got non-zero exit code"
    exit 1
fi
grep -q "Features Per Compiler Demo" output.txt || { echo "ERROR: Features Per Compiler Demo not found"; exit 1; }
grep -q "✓ All compiler flag checks PASSED" output.txt || { echo "ERROR: Expected all checks to pass"; exit 1; }
# Should NOT have any feature flags in baseline
if grep -q "✓ DEBUG: -g flag found" output.txt; then
    echo "ERROR: Debug flags should not be present in baseline"
    exit 1
fi
if grep -q "✓ RELEASE: -O flags found" output.txt; then
    echo "ERROR: Release flags should not be present in baseline"
    exit 1
fi
echo "✓ Baseline (no profile) works"

# Cleanup
rm -rf build output.txt
popd

echo "All FPM features tests passed!"
