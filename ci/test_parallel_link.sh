#!/usr/bin/env bash
set -eo pipefail

# Test that archives and executables made in parallel each get their own, intact
# command: an OpenMP fpm links all executables of a schedule region at once, and
# archives all dependencies at once
# Usage: ./test_parallel_link.sh [fpm_executable] [rounds]

if [ "$1" ]; then
   fpm="$1"
else
   fpm=fpm
fi
rounds="${2:-10}"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

ndeps=8
nexe=32

# New library package, quietly unless it fails
new_lib() {
    "$fpm" new "$1" --lib > "$work/new.log" 2>&1 || { cat "$work/new.log"; exit 1; }
}

# Dependencies, each archived on its own (library.type) in the same build directory
cd "$work"
new_lib par
cd par
mkdir app
printf 'library.type = "static"\n[dependencies]\n' >> fpm.toml
uses=""
for i in $(seq 1 $ndeps); do
    new_lib "dep$i"
    printf 'dep%s = { path = "dep%s" }\n' "$i" "$i" >> fpm.toml
    uses="$uses  use dep$i, only: hello$i => say_hello\n"
done
printf "module par\n${uses}contains\n  integer function f(k)\n    integer, intent(in) :: k\n    f = k\n  end function\nend module\n" > src/par.f90

# Executables with names of distinct lengths: a command that gets another one's
# length shows up as a mismatched object and output name
for k in $(seq 1 $nexe); do
    name="p$(printf "%${k}s" | tr ' ' 'x')"
    printf 'program %s\n  use par\n  print *, f(%s)\nend program\n' "$name" "$k" > "app/$name.f90"
done

"$fpm" build

for round in $(seq 1 "$rounds"); do
    # Remove every archive and executable, so they are all made again at once
    find build -type f \( -name '*.a' -o -name '*.lib' -o -path '*app*p*x' -o -path '*app*p*x.exe' \) -delete

    if ! "$fpm" build --verbose > build.log 2>&1; then
        cat build.log
        echo "ERROR: parallel build failed in round $round"
        exit 1
    fi

    found=$(find build -path '*app*' -type f \( -name 'p*x' -o -name 'p*x.exe' \) | wc -l)
    if [ $((found)) -ne $nexe ]; then
        cat build.log
        echo "ERROR: $((found)) of $nexe executables built in round $round"
        exit 1
    fi

    # Each echoed link command names its own object: `... app_<name>.f90.o ... -o .../<name>`.
    # Only whole echoed lines are checked: some compilers' runtimes wrap long output.
    bad=$(tr '\\' '/' < build.log | tr -d '\r' | awk '
        /^ \+ / && / -o [^ ]*\/app\/px+(\.exe)?$/ {
            out = $NF; sub(/.*\//, "", out); sub(/\.exe$/, "", out)
            if (index($0, "/app_" out ".f90.o ") == 0) print
        }')
    if [ -n "$bad" ]; then
        echo "$bad"
        echo "ERROR: garbled link command in round $round"
        exit 1
    fi
done

# Each executable prints the number of x in its name
for exe in $(find build -path '*app*' -type f \( -name 'p*x' -o -name 'p*x.exe' \)); do
    name="$(basename "$exe" .exe)"
    result="$("$exe" | tr -d ' \r')"
    if [ "$result" != "$(( ${#name} - 1 ))" ]; then
        echo "ERROR: $exe printed '$result'"
        exit 1
    fi
done

echo "Parallel links and archives built correctly in $rounds rounds"
