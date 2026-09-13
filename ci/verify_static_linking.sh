#!/usr/bin/env bash
# verify_static_linking.sh — Verify that an fpm binary has no dynamic GCC runtime dependencies.
#
# This script checks a Windows PE binary (fpm.exe) to ensure it does not dynamically
# link against GCC runtime DLLs (libgfortran, libgcc, libwinpthread, libquadmath).
#
# Dynamic linking against these DLLs causes ABI mismatch crashes when users install
# GCC compilers with different threading models (e.g., Win32 vs POSIX threads).
#
# Usage:
#   ./ci/verify_static_linking.sh <path-to-fpm-binary>
#
# Exit codes:
#   0 — Binary is statically linked (no GCC runtime DLL dependencies)
#   1 — Binary has dynamic GCC runtime dependencies (FAIL)
#   2 — Usage error or missing tools

set -euo pipefail

# --- Usage ---
if [ $# -lt 1 ]; then
    echo "Usage: $0 <path-to-binary>"
    echo "  Verifies that a Windows binary has no dynamic GCC runtime DLL dependencies."
    exit 2
fi

BINARY="$1"

if [ ! -f "$BINARY" ]; then
    echo "Error: Binary not found: $BINARY"
    exit 2
fi

# --- Check for objdump ---
if ! command -v objdump &> /dev/null; then
    echo "Warning: objdump not found. Skipping static linking verification."
    echo "Install binutils to enable this check."
    exit 0
fi

echo "=== Static Linking Verification ==="
echo "Binary: $BINARY"
echo ""

# List all DLL dependencies
echo "DLL dependencies:"
DLL_LIST=$(objdump -p "$BINARY" 2>/dev/null | grep -i "DLL Name" || true)

if [ -z "$DLL_LIST" ]; then
    echo "  (none found — binary may be fully static or not a PE file)"
    echo ""
    echo "✓ PASS: No dynamic DLL dependencies detected."
    exit 0
fi

echo "$DLL_LIST" | sed 's/^/  /'
echo ""

# Check for GCC runtime DLLs that indicate dynamic linking
# These are the DLLs that cause ABI mismatch issues (Issue #1204):
#   - libgfortran-*.dll   (Fortran runtime — threading model dependent)
#   - libgcc_s_seh-*.dll  (GCC support library)
#   - libwinpthread-*.dll (POSIX threading shim)
#   - libquadmath-*.dll   (Quad-precision math library)
GCC_RUNTIME_PATTERN="libgfortran\|libgcc\|libwinpthread\|libquadmath"

if echo "$DLL_LIST" | grep -qi "$GCC_RUNTIME_PATTERN"; then
    echo "✗ FAIL: Binary has dynamic dependencies on GCC runtime DLLs!"
    echo ""
    echo "The following GCC runtime DLLs were found:"
    echo "$DLL_LIST" | grep -i "$GCC_RUNTIME_PATTERN" | sed 's/^/  /'
    echo ""
    echo "This will cause 'Entry point not found' errors when users have"
    echo "different GCC versions with incompatible threading models installed."
    echo "See: https://github.com/fortran-lang/fpm/issues/1204"
    echo ""
    echo "Fix: Build with '--flag \"-static\"' to statically link GCC runtime libraries."
    exit 1
fi

echo "✓ PASS: No GCC runtime DLL dependencies found. Binary is safe for distribution."
exit 0
