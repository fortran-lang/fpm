#!/usr/bin/env bash
set -exo pipefail

# Test that fpm processes running at the same time on one package wait for each
# other through the build directory lock, instead of building over each other (#957)
# Usage: ./test_concurrent_builds.sh [fpm_executable]

if [ "$1" ]; then
   fpm="$1"
else
   fpm=fpm
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Wait at most 120 s for a background process, and return its exit status,
# or 124 if it is still running
wait_for() {
    local pid="$1" i
    for i in $(seq 120); do
        kill -0 "$pid" 2>/dev/null || break
        sleep 1
    done
    if kill -0 "$pid" 2>/dev/null; then
        kill "$pid"
        return 124
    fi
    wait "$pid"
}

# Work on a copy of an example package, so that every build starts from scratch
cp -R "$(dirname "$0")/../example_packages/hello_complex" "$work/pkg"
rm -rf "$work/pkg/build"
pushd "$work/pkg"

echo "Test 1: builds of one package that start together all succeed"
pids=()
for i in 1 2 3 4; do
    "$fpm" build > "$work/build$i.log" 2>&1 &
    pids+=($!)
done
failed=0
for pid in "${pids[@]}"; do
    wait_for "$pid" || failed=1
done
if [ $failed -ne 0 ]; then
    cat "$work"/build*.log
    echo "ERROR: a build that ran at the same time as others failed"
    exit 1
fi
# They started together, so all but the first waited for the lock
grep -q "Waiting for another fpm process" "$work"/build*.log
"$fpm" run --target say_Hello > "$work/run.log"
test "$(tail -n 1 "$work/run.log" | tr -d ' \r')" = "Hello,World!"

echo "Test 2: programs that fpm run starts can run fpm on the same package"
# fpm releases the lock before it runs the programs: if it did not, the
# fpm build that the runner starts would wait forever
mkdir -p "$work/bin"
case "$fpm" in
    *.exe) cp "$fpm" "$work/bin/fpm.exe" ;;
    *)     cp "$fpm" "$work/bin/fpm" ;;
esac
PATH="$work/bin:$PATH" "$fpm" run --target say_Hello --runner "fpm build &&" > "$work/nested.log" 2>&1 &
status=0
wait_for $! || status=$?
cat "$work/nested.log"
if [ $status -eq 124 ]; then
    echo "ERROR: fpm run kept the build directory locked while its program ran"
    exit 1
fi
test $status -eq 0
! grep -q "Waiting for another fpm process" "$work/nested.log"
test "$(tail -n 1 "$work/nested.log" | tr -d ' \r')" = "Hello,World!"

echo "Test 3: builds that stop with an error leave the build directory unlocked"
cp source/greet_m.f90 "$work/greet_m.f90"
printf 'this is not fortran\n' >> source/greet_m.f90
pids=()
for i in 1 2; do
    "$fpm" build > "$work/fail$i.log" 2>&1 &
    pids+=($!)
done
for pid in "${pids[@]}"; do
    status=0
    wait_for "$pid" || status=$?
    if [ $status -eq 0 ] || [ $status -eq 124 ]; then
        cat "$work"/fail*.log
        echo "ERROR: a build of a broken source did not stop with an error"
        exit 1
    fi
done
cp "$work/greet_m.f90" source/greet_m.f90
"$fpm" build > "$work/fixed.log" 2>&1 || { cat "$work/fixed.log"; exit 1; }
! grep -q "Waiting for another fpm process" "$work/fixed.log"
popd

echo "All concurrent build tests passed"
