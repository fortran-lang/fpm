#!/usr/bin/env bash
set -exo pipefail

# Test that incremental builds rebuild every target whose dependencies were rebuilt
# by an earlier run that did not rebuild the target itself (#1323)
# Usage: ./test_incremental_rebuild.sh [fpm_executable]

if [ "$1" ]; then
   fpm="$1"
else
   fpm=fpm
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Run a command, and check that it prints the expected value
expect() {
    local expected="$1"
    shift
    "$@" > output.txt
    local result
    result="$(tr -d ' \r' < output.txt)"
    if [ "$result" != "$expected" ]; then
        echo "ERROR: '$*' printed '$result', expected '$expected'"
        exit 1
    fi
}

# Build that must fail
build_fails() {
    if "$fpm" build; then
        echo "ERROR: build was expected to fail"
        exit 1
    fi
}

module_a() {
    printf 'module a\ncontains\n  integer function f()\n    f = %s\n  end function\nend module\n' "$1" > src/a.f90
}

app_main() {
    printf 'program main\n  use a\n  print *, f(%s\nend program\n' "$1" > app/main.f90
}

echo "Test 1: link targets after a build that failed before linking"
for lib_type in default static shared; do
    mkdir -p "$work/link_$lib_type/src" "$work/link_$lib_type/app"
    pushd "$work/link_$lib_type"
    printf 'name = "link_%s"\n' "$lib_type" > fpm.toml
    if [ "$lib_type" != default ]; then
        printf '[library]\ntype = "%s"\n' "$lib_type" >> fpm.toml
    fi
    module_a 1
    app_main ')'
    "$fpm" build
    expect 1 "$fpm" run

    # Change the module and break the app: the module is compiled, the app is not linked
    module_a 2
    app_main ''
    build_fails

    # Fix the app, back to its last built source
    app_main ')'
    "$fpm" build
    expect 2 "$fpm" run
    popd
done
echo "✓ Link targets are rebuilt after a failed build"

echo "Test 2: objects using a module recompiled by a failed build"
mkdir -p "$work/objects/src" "$work/objects/app"
pushd "$work/objects"
printf 'name = "objects"\n' > fpm.toml
module_k() {
    printf 'module k\n  integer, parameter :: n = %s\nend module\n' "$1" > src/k.f90
}
# Uses k, so that it builds after k and before main
module_b() {
    printf 'module b\n  use k\ncontains\n  integer function g()\n    g = 0*n\n  end function\nend module%s\n' "$1" > src/b.f90
}
module_k 1
module_b ''
printf 'program main\n  use k\n  use b\n  print *, n + g()\nend program\n' > app/main.f90
"$fpm" build
expect 1 "$fpm" run

# Change k and break b: k is compiled, the build stops before main
module_k 2
module_b ' x'
build_fails

# Fix b, back to its last built source: main must be compiled against the new k
module_b ''
"$fpm" build
expect 2 "$fpm" run
popd
echo "✓ Dependent objects are rebuilt after a failed build"

echo "Test 3: tests left out of a build that recompiled their dependencies"
mkdir -p "$work/tests/src" "$work/tests/test"
pushd "$work/tests"
printf 'name = "tests"\n' > fpm.toml
module_k 1
printf 'program check\n  use k\n  print *, n\nend program\n' > test/check.f90
"$fpm" build --tests
expect 1 "$fpm" test

# `fpm build` compiles k, but not the test
module_k 2
"$fpm" build
"$fpm" build --tests
expect 2 "$fpm" test
popd
echo "✓ Tests are rebuilt after a build without them"

echo "Test 4: up-to-date project is not rebuilt"
pushd "$work/tests"
"$fpm" build --tests 2>&1 | tee build.log
grep -q "Project is up to date" build.log
popd
echo "✓ Up-to-date project is not rebuilt"

echo "Test 5: sources whose included files changed"
mkdir -p "$work/include/src" "$work/include/include" "$work/include/app"
pushd "$work/include"
printf 'name = "include"\n' > fpm.toml
printf 'module a\ncontains\n  integer function f()\n    include "a.inc"\n  end function\nend module\n' > src/a.f90
printf 'program main\n  use a\n  print *, f()\nend program\n' > app/main.f90
# a.inc includes b.inc from the include directory
include_a() {
    printf 'include "b.inc"\nf = f + %s\n' "$1" > src/a.inc
}
include_b() {
    printf 'f = 10*%s\n' "$1" > include/b.inc
}
include_a 1
include_b 1
"$fpm" build
expect 11 "$fpm" run
include_a 2
"$fpm" build
expect 12 "$fpm" run
include_b 2
"$fpm" build
expect 22 "$fpm" run
rm include/b.inc
build_fails
popd
echo "✓ Sources are rebuilt after their included files changed"

# Whether the last build compiled a given source; Windows logs use backslashes
compiled() {
    tr '\\' / < build.log | grep -q -- "-c .*$1"
}

echo "Test 6: dependents of a module whose interface is unchanged"
mkdir -p "$work/interface/src" "$work/interface/app"
pushd "$work/interface"
printf 'name = "interface"\n' > fpm.toml
module_a 1
printf 'module b\n  use a\ncontains\n  integer function g()\n    g = f()\n  end function\nend module\n' > src/b.f90
printf 'program main\n  use b\n  print *, g()\nend program\n' > app/main.f90
"$fpm" build
expect 1 "$fpm" run

# Only a procedure body changes: gfortran leaves a.mod as it was, so b need not compile
module_a 2
"$fpm" build --verbose 2>&1 | tee build.log
compiled src/a.f90
case "${FPM_FC:-gfortran}" in
    *gfortran*)
        if compiled src/b.f90 || compiled app/main.f90; then
            echo "ERROR: dependents of a module with an unchanged interface were compiled"
            exit 1
        fi ;;
esac
expect 2 "$fpm" run

# The interface changes: b compiles again
printf 'module a\ncontains\n  integer function f(i)\n    integer, optional :: i\n    f = 3\n  end function\nend module\n' > src/a.f90
"$fpm" build --verbose 2>&1 | tee build.log
compiled src/b.f90
expect 3 "$fpm" run
popd
echo "✓ Dependents compile again only when a module interface changes"

echo "Test 7: submodules of a module whose private entities change"
mkdir -p "$work/submodule/src" "$work/submodule/app"
pushd "$work/submodule"
printf 'name = "submodule"\n' > fpm.toml
module_p() {
    printf 'module p\n  implicit none\n  %s, private :: v\n  interface\n    module integer function h()\n    end function\n  end interface\nend module\n' "$1" > src/p.f90
}
module_p integer
printf 'submodule (p) s\ncontains\n  module integer function h()\n    v = 1\n    h = int(v + v)\n  end function\nend submodule\n' > src/s.f90
printf 'program main\n  use p\n  print *, h()\nend program\n' > app/main.f90
"$fpm" build
expect 2 "$fpm" run

# p.mod is unchanged, but the submodule reads the private v through p.smod
module_p 'real(8)'
"$fpm" build --verbose 2>&1 | tee build.log
compiled src/s.f90
expect 2 "$fpm" run
popd
echo "✓ Submodules compile again when private entities of their ancestor change"

echo "All incremental rebuild tests passed"
