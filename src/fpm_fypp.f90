!># fypp preprocessing of `.fypp` sources
!>
!> When fypp is enabled for a package (`[preprocess.fypp]`), every `.fypp` file found in one of
!> its source directories is expanded by fypp into
!>
!>     <build_dir>/preprocess/<package>/<source-dir>/<relative path>.f90   (or .F90)
!>
!> *before* source parsing, so the generated sources take part in module dependency resolution
!> exactly like hand-written ones. The output directory mirrors the source tree and is owned by
!> fpm: outputs whose `.fypp` file disappeared are deleted.
!>
!> Expansion is incremental. A digest of the fypp command line, of the `.fypp` file and of every
!> file it (recursively) `#:include`s is stored next to each output as `<output>.digest`; files
!> whose digest did not change are not re-expanded. Stale files are expanded in parallel.
!>
!> The fypp executable is `fypp`, or the value of the `FPM_FYPP` environment variable.
module fpm_fypp
    use, intrinsic :: iso_fortran_env, only: int64
    use fpm_error, only: error_t, fatal_error
    use fpm_strings, only: string_t, lower, str_ends_with, fnv_1a, operator(.in.), add_strings
    use fpm_filesystem, only: list_files, join_path, dirname, basename, exists, is_dir, mkdir, &
                              read_lines, delete_file, canon_path
    use fpm_environment, only: get_env, os_is_unix
    use fpm_model, only: srcfile_t
    use fpm_manifest_preprocess, only: preprocess_config_t
    use fpm_sources, only: add_sources_from_dir, is_fypp_file
    implicit none
    private

    public :: preprocess_fypp_dir, fypp_output_suffix, glob_match

    character(*), parameter :: fypp_ext = ".fypp"
    character(*), parameter :: digest_ext = ".digest"

    !> Maximum `#:include` nesting followed when computing digests
    integer, parameter :: max_include_depth = 32

contains

!> Expand all `.fypp` files below `src_dir` into `out_dir`, then add the generated sources
!> to `sources` with the given `scope`.
subroutine preprocess_fypp_dir(sources, src_dir, out_dir, scope, preprocess, include_dirs, &
                               with_executables, verbose, error)
    !> Source list the generated files are appended to
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    !> Source directory to search (recursively) for `.fypp` files
    character(*), intent(in) :: src_dir
    !> Output directory, mirroring `src_dir`
    character(*), intent(in) :: out_dir
    !> Scope of the generated sources (FPM_SCOPE_*)
    integer, intent(in) :: scope
    !> Preprocessor configuration of the package (must have fypp enabled)
    type(preprocess_config_t), intent(in) :: preprocess
    !> Include directories, passed to fypp as -I
    type(string_t), intent(in) :: include_dirs(:)
    !> Whether generated `program`s are kept (executable scopes)
    logical, intent(in) :: with_executables
    !> Echo the fypp command lines
    logical, intent(in) :: verbose
    type(error_t), allocatable, intent(out) :: error

    type(string_t), allocatable :: files(:), inputs(:), outputs(:), commands(:)
    integer(int64), allocatable :: digests(:)
    logical, allocatable :: stale(:)
    integer, allocatable :: exitstat(:), cmdstat(:)
    character(:), allocatable :: fypp, flags, rel, failed
    integer :: i, n

    call list_files(src_dir, files, recurse=.true.)
    allocate(inputs(0))
    do i = 1, size(files)
        if (is_fypp_file(files(i)%s)) call add_strings(inputs, string_t(canon_path(files(i)%s)))
    end do
    n = size(inputs)

    allocate(outputs(n), commands(n), digests(n), stale(n), exitstat(n), cmdstat(n))

    if (n > 0) then
        fypp = get_env("FPM_FYPP", "fypp")
        flags = fypp_flags(preprocess, include_dirs)

        do i = 1, n
            rel = relative_path(inputs(i)%s, canon_path(src_dir))
            outputs(i)%s = canon_path(join_path(out_dir, strip_fypp_ext(rel) // "." // &
                                      fypp_output_suffix(rel, preprocess)))
            commands(i)%s = fypp // flags // " " // shell_quote(inputs(i)%s) // " " // &
                            shell_quote(outputs(i)%s)
            digests(i) = input_digest(commands(i)%s, inputs(i)%s, include_dirs)
            stale(i) = .not.(exists(outputs(i)%s) .and. digest_matches(outputs(i)%s, digests(i)))
        end do

        if (any(stale)) then

            call check_fypp_available(fypp, error)
            if (allocated(error)) return

            do i = 1, n
                if (.not.stale(i)) cycle
                call mkdir(dirname(outputs(i)%s), echo=.false.)
                call delete_file(outputs(i)%s // digest_ext)
                if (verbose) print '(a)', ' + ' // commands(i)%s
            end do

            exitstat = 0
            cmdstat = 0
            !$omp parallel do schedule(dynamic) default(shared) private(i)
            do i = 1, n
                if (stale(i)) call execute_command_line(commands(i)%s, exitstat=exitstat(i), &
                                                        cmdstat=cmdstat(i))
            end do
            !$omp end parallel do

            failed = ""
            do i = 1, n
                if (.not.stale(i)) cycle
                if (exitstat(i) == 0 .and. cmdstat(i) == 0) then
                    call write_digest(outputs(i)%s, digests(i))
                else
                    call delete_file(outputs(i)%s)
                    failed = failed // new_line("a") // "  " // inputs(i)%s
                end if
            end do

            if (len(failed) > 0) then
                call fatal_error(error, "fypp preprocessing failed for:" // failed)
                return
            end if

        end if
    end if

    ! Remove outputs left over from deleted/renamed .fypp files or a changed suffix
    if (is_dir(out_dir)) call remove_stale_outputs(out_dir, outputs)

    if (n == 0) return

    call add_sources_from_dir(sources, out_dir, scope, with_executables=with_executables, &
                              error=error, preprocess=preprocess, include_dirs=include_dirs)

end subroutine preprocess_fypp_dir

!> Suffix ("f90" or "F90") of the source generated from a `.fypp` file
!> (`rel_path`: path relative to its source directory)
function fypp_output_suffix(rel_path, preprocess) result(suffix)
    character(*), intent(in) :: rel_path
    type(preprocess_config_t), intent(in) :: preprocess
    character(:), allocatable :: suffix

    if (matches_any(rel_path, preprocess%force_cpp_preprocessing)) then
        suffix = "F90"
    else if (matches_any(rel_path, preprocess%skip_cpp_preprocessing)) then
        suffix = "f90"
    else if (allocated(preprocess%output_suffix)) then
        suffix = preprocess%output_suffix
    else
        suffix = "F90"
    end if

end function fypp_output_suffix

!> Whether a `.fypp` path matches any pattern, tried against the file name with and without
!> extension and against the relative path with and without extension
logical function matches_any(rel_path, patterns)
    character(*), intent(in) :: rel_path
    type(string_t), allocatable, intent(in) :: patterns(:)

    character(:), allocatable :: path, name
    integer :: i

    matches_any = .false.
    if (.not.allocated(patterns)) return

    path = canon_path(rel_path)
    name = basename(path)
    do i = 1, size(patterns)
        associate(p => patterns(i)%s)
            matches_any = glob_match(name, p) .or. glob_match(strip_fypp_ext(name), p) &
                     .or. glob_match(path, p) .or. glob_match(strip_fypp_ext(path), p)
        end associate
        if (matches_any) return
    end do

end function matches_any

!> Wildcard match: `*` matches any sequence (including empty), `?` any single character
pure logical function glob_match(string, pattern) result(match)
    character(*), intent(in) :: string, pattern

    integer :: s, p, star, mark

    s = 1; p = 1; star = 0; mark = 0
    do while (s <= len(string))
        if (p <= len(pattern)) then
            if (pattern(p:p) == "?" .or. pattern(p:p) == string(s:s)) then
                s = s + 1; p = p + 1
                cycle
            else if (pattern(p:p) == "*") then
                star = p; mark = s; p = p + 1
                cycle
            end if
        end if
        if (star > 0) then
            p = star + 1; mark = mark + 1; s = mark
        else
            match = .false.
            return
        end if
    end do
    do while (p <= len(pattern))
        if (pattern(p:p) /= "*") exit
        p = p + 1
    end do
    match = p > len(pattern)

end function glob_match

!> Command line flags shared by all fypp invocations of a package
function fypp_flags(preprocess, include_dirs) result(flags)
    type(preprocess_config_t), intent(in) :: preprocess
    type(string_t), intent(in) :: include_dirs(:)
    character(:), allocatable :: flags

    integer :: i

    flags = ""
    if (allocated(preprocess%macros)) then
        do i = 1, size(preprocess%macros)
            flags = flags // " -D" // shell_quote(preprocess%macros(i)%s)
        end do
    end if
    do i = 1, size(include_dirs)
        flags = flags // " -I" // shell_quote(canon_path(include_dirs(i)%s))
    end do

end function fypp_flags

!> Digest of everything the output of a fypp invocation depends on
function input_digest(command, fypp_file, include_dirs) result(digest)
    character(*), intent(in) :: command, fypp_file
    type(string_t), intent(in) :: include_dirs(:)
    integer(int64) :: digest

    type(string_t), allocatable :: visited(:)

    allocate(visited(0))
    digest = fnv_1a(command)
    call hash_file(fypp_file, include_dirs, digest, visited, 0)

end function input_digest

!> Fold a file and, recursively, the files it `#:include`s into a digest
recursive subroutine hash_file(file, include_dirs, digest, visited, depth)
    character(*), intent(in) :: file
    type(string_t), intent(in) :: include_dirs(:)
    integer(int64), intent(inout) :: digest
    type(string_t), allocatable, intent(inout) :: visited(:)
    integer, intent(in) :: depth

    type(string_t), allocatable :: lines(:)
    character(:), allocatable :: name, found
    integer :: i

    if (depth > max_include_depth) return

    lines = read_lines(file)
    if (size(lines) > 0) digest = fnv_1a(lines, digest)

    do i = 1, size(lines)
        name = include_target(lines(i)%s)
        if (len(name) == 0) cycle

        ! An unresolved include is reported by fypp itself
        found = resolve_include(name, dirname(file), include_dirs)
        if (len(found) == 0) cycle

        found = canon_path(found)
        if (found .in. visited) cycle
        call add_strings(visited, string_t(found))

        digest = fnv_1a(found, digest)
        call hash_file(found, include_dirs, digest, visited, depth + 1)
    end do

end subroutine hash_file

!> File named by a `#:include "file"` directive, or "" if the line is not one
function include_target(line) result(name)
    character(*), intent(in) :: line
    character(:), allocatable :: name

    character(:), allocatable :: rest
    character :: quote
    integer :: k

    name = ""
    rest = trim(adjustl(line))
    if (len(rest) < 10) return
    if (lower(rest(1:9)) /= "#:include") return

    rest = trim(adjustl(rest(10:)))
    if (len(rest) < 2) return
    quote = rest(1:1)
    if (quote /= '"' .and. quote /= "'") return

    k = index(rest(2:), quote)
    if (k > 1) name = rest(2:k)

end function include_target

!> Locate an included file: next to the including file first, then in the include directories
function resolve_include(name, current_dir, include_dirs) result(path)
    character(*), intent(in) :: name, current_dir
    type(string_t), intent(in) :: include_dirs(:)
    character(:), allocatable :: path

    integer :: i

    path = join_path(current_dir, name)
    if (exists(path)) return
    do i = 1, size(include_dirs)
        path = join_path(include_dirs(i)%s, name)
        if (exists(path)) return
    end do
    path = ""

end function resolve_include

!> Whether `<output>.digest` holds the expected digest
logical function digest_matches(output, digest)
    character(*), intent(in) :: output
    integer(int64), intent(in) :: digest

    integer(int64) :: cached
    integer :: unit, stat

    digest_matches = .false.
    if (.not.exists(output // digest_ext)) return

    open(newunit=unit, file=output // digest_ext, status="old", action="read", iostat=stat)
    if (stat /= 0) return
    read(unit, *, iostat=stat) cached
    close(unit)
    digest_matches = stat == 0 .and. cached == digest

end function digest_matches

subroutine write_digest(output, digest)
    character(*), intent(in) :: output
    integer(int64), intent(in) :: digest

    integer :: unit, stat

    open(newunit=unit, file=output // digest_ext, status="replace", action="write", iostat=stat)
    if (stat /= 0) return
    write(unit, '(i0)') digest
    close(unit)

end subroutine write_digest

!> Delete files in `out_dir` that are neither an expected output nor its digest
subroutine remove_stale_outputs(out_dir, outputs)
    character(*), intent(in) :: out_dir
    type(string_t), intent(in) :: outputs(:)

    type(string_t), allocatable :: files(:)
    character(:), allocatable :: file, owner
    integer :: i

    call list_files(out_dir, files, recurse=.true.)
    do i = 1, size(files)
        file = canon_path(files(i)%s)
        owner = file
        if (str_ends_with(file, digest_ext)) owner = file(1:len(file) - len(digest_ext))
        if (.not.(owner .in. outputs)) call delete_file(files(i)%s)
    end do

end subroutine remove_stale_outputs

!> Fail early with a clear message if fypp cannot be run
subroutine check_fypp_available(fypp, error)
    character(*), intent(in) :: fypp
    type(error_t), allocatable, intent(out) :: error

    integer :: stat, cmdstat
    character(len=256) :: cmdmsg

    ! Not via `run`: it stops with a compiler-specific message when a program is not found
    stat = 0
    cmdstat = 0
    if (os_is_unix()) then
        call execute_command_line(fypp // " --version >/dev/null 2>&1", exitstat=stat, &
                                  cmdstat=cmdstat, cmdmsg=cmdmsg)
    else
        call execute_command_line(fypp // " --version >NUL 2>&1", exitstat=stat, &
                                  cmdstat=cmdstat, cmdmsg=cmdmsg)
    end if
    if (stat /= 0 .or. cmdstat /= 0) then
        call fatal_error(error, "Cannot run fypp ('" // fypp // " --version' failed). " // &
            "fypp is required to preprocess .fypp sources: install it (e.g. 'pip install fypp') " // &
            "or point the FPM_FYPP environment variable to it.")
    end if

end subroutine check_fypp_available

!> `path` relative to `base` (both canonical); `path` itself if it is not below `base`
function relative_path(path, base) result(rel)
    character(*), intent(in) :: path, base
    character(:), allocatable :: rel

    rel = path
    if (base == "." .or. len(base) == 0) return
    if (len(path) > len(base) + 1) then
        if (path(1:len(base)) == base .and. path(len(base)+1:len(base)+1) == "/") &
            rel = path(len(base)+2:)
    end if

end function relative_path

!> Remove a trailing `.fypp` extension (any case)
function strip_fypp_ext(path) result(stem)
    character(*), intent(in) :: path
    character(:), allocatable :: stem

    stem = path
    if (str_ends_with(lower(path), fypp_ext)) stem = path(1:len(path) - len(fypp_ext))

end function strip_fypp_ext

!> Quote a command line argument if it contains blanks or shell metacharacters
function shell_quote(arg) result(quoted)
    character(*), intent(in) :: arg
    character(:), allocatable :: quoted

    if (scan(arg, " &|<>()^;$`!*?'") > 0 .and. index(arg, '"') == 0) then
        quoted = '"' // arg // '"'
    else
        quoted = arg
    end if

end function shell_quote

end module fpm_fypp
