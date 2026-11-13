#!/usr/bin/env bash
set -euo pipefail

fpm="${1:-fpm}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cd "$work"
mkdir app src
printf 'name = "dotted_names"\n' > fpm.toml
printf 'module support\nend module support\n' > src/support.f90
printf 'program main\n  print *, "hello"\nend program main\n' > app/my.prog.f90

# Both the complete list and a selected target must retain interior dots.
for args in '--list' 'my.prog --list'; do
    "$fpm" run $args > output.txt 2>&1 || { cat output.txt; exit 1; }
    grep -Eq '(^|[[:space:]])my\.prog([[:space:]]|$)' output.txt
    if grep -Eq '(^|[[:space:]])my([[:space:]]|$)' output.txt; then
        cat output.txt
        exit 1
    fi
done

# A missing target prints the complete list through compact_list_all.
if "$fpm" run missing-target --list > output.txt 2>&1; then
    echo "An unknown target unexpectedly succeeded"
    exit 1
fi
grep -Eq '(^|[[:space:]])my\.prog([[:space:]]|$)' output.txt
if grep -Eq '(^|[[:space:]])my([[:space:]]|$)' output.txt; then
    cat output.txt
    exit 1
fi
