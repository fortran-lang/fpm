#!/usr/bin/env bash
set -exo pipefail

# Test that a target is built as soon as its own dependencies are, without waiting
# for unrelated targets that started before it (a slow compile must not hold back
# the rest of the build)
# Usage: ./test_build_schedule.sh [fpm_executable]

if [ "$1" ]; then
   fpm="$1"
else
   fpm=fpm
fi

case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*) echo "Skipped: the compiler wrapper is a shell script"; exit 0 ;;
esac

real_fc="$(command -v gfortran || true)"
if [ -z "$real_fc" ]; then
    echo "Skipped: needs gfortran"
    exit 0
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# gfortran that takes 5 s for slow.f90, and logs when each source starts and ends
mkdir "$work/bin"
cat > "$work/bin/gfortran" <<WRAPPER
#!/usr/bin/env bash
src=""
for arg in "\$@"; do
    case "\$arg" in *.f90) src="\$(basename "\$arg")" ;; esac
done
[ -n "\$src" ] && echo "start \$src" >> "$work/compile.log"
[ "\$src" = slow.f90 ] && sleep 5
"$real_fc" "\$@"
stat=\$?
[ -n "\$src" ] && echo "end \$src" >> "$work/compile.log"
exit \$stat
WRAPPER
chmod +x "$work/bin/gfortran"

# slow and a do not depend on anything, b depends on a only
mkdir -p "$work/schedule/src"
cd "$work/schedule"
printf 'name = "schedule"\n' > fpm.toml
printf 'module slow\nend module\n' > src/slow.f90
printf 'module a\nend module\n' > src/a.f90
printf 'module b\n  use a\nend module\n' > src/b.f90

FPM_FC="$work/bin/gfortran" "$fpm" build --jobs 2 2>&1 | tee build.log
if grep -q "ignored: this fpm was built without OpenMP" build.log; then
    echo "Skipped: this fpm builds one target at a time"
    exit 0
fi
cat "$work/compile.log"

# b must start while slow is still compiling
start_b=$(grep -n "^start b.f90" "$work/compile.log" | cut -d: -f1)
end_slow=$(grep -n "^end slow.f90" "$work/compile.log" | cut -d: -f1)
if [ "$start_b" -gt "$end_slow" ]; then
    echo "ERROR: b.f90 waited for slow.f90, which it does not depend on"
    exit 1
fi

echo "✓ Targets build as soon as their own dependencies are built"
