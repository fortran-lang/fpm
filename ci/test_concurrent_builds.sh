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

# A chain of modules, each using the one before: builds that do not wait for
# each other write the same module and object files at the same time
mkdir -p "$work/chain/src" "$work/chain/app"
pushd "$work/chain"
printf 'name = "chain"\n' > fpm.toml
printf 'module m1\ncontains\n  integer function f1()\n    f1 = 1\n  end function\nend module\n' > src/m1.f90
for i in $(seq 2 30); do
    printf 'module m%d\n  use m%d\ncontains\n  integer function f%d()\n    f%d = f%d() + 1\n  end function\nend module\n' \
        "$i" "$((i-1))" "$i" "$i" "$((i-1))" > "src/m$i.f90"
done
printf 'program main\n  use m30\n  print *, f30()\nend program\n' > app/main.f90

echo "Test 1: builds of one package that start together all succeed"
pids=()
for i in 1 2 3 4; do
    "$fpm" build > "$work/build$i.log" 2>&1 &
    pids+=($!)
done
failed=0
for pid in "${pids[@]}"; do
    wait "$pid" || failed=1
done
if [ $failed -ne 0 ]; then
    cat "$work"/build*.log
    echo "ERROR: a build that ran at the same time as others failed"
    exit 1
fi
# They started together, so all but the first waited for the lock
grep -q "Waiting for another fpm process" "$work"/build*.log
"$fpm" run > "$work/run.log"
test "$(tail -n 1 "$work/run.log" | tr -d ' \r')" = 30

echo "Test 2: programs that fpm run starts can run fpm on the same package"
# fpm releases the lock before it runs the programs: if it did not, the
# fpm build that the runner starts would wait forever
mkdir -p "$work/bin"
case "$fpm" in
    *.exe) cp "$fpm" "$work/bin/fpm.exe" ;;
    *)     cp "$fpm" "$work/bin/fpm" ;;
esac
PATH="$work/bin:$PATH" "$fpm" run --runner "fpm build &&" > "$work/nested.log" 2>&1 &
pid=$!
for i in $(seq 120); do
    kill -0 "$pid" 2>/dev/null || break
    sleep 1
done
if kill -0 "$pid" 2>/dev/null; then
    kill "$pid"
    cat "$work/nested.log"
    echo "ERROR: fpm run kept the build directory locked while its program ran"
    exit 1
fi
wait "$pid"
cat "$work/nested.log"
! grep -q "Waiting for another fpm process" "$work/nested.log"
test "$(tail -n 1 "$work/nested.log" | tr -d ' \r')" = 30
popd

echo "All concurrent build tests passed"
