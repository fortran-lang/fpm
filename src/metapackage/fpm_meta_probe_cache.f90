!># Probe cache for metapackages
!>
!> A metapackage probe compiles, links and runs a small program, and fpm resolves the
!> metapackages twice in every call -- once for the package, once more after its dependencies
!> are known -- so an unrecorded probe is paid twice per call. This module records the answers
!> in two layers:
!>
!> - **this process:** every answer, passed or failed, for the rest of the call;
!> - **disk**, in `<build-dir>/probe_cache.txt`: only the probes that PASSED, one key per line.
!>   A failed probe is never recorded, so a fixed environment is seen on the next call. A passed
!>   one goes stale only when the probe would now fail, and then the build itself fails loudly.
!>   `fpm clean`, or deleting the file, makes every probe run again.
!>
!> A key names what the answer depends on that fpm can see without running anything: the
!> probe, its flags, fpm's version, and the compiler command with the file it resolves to and
!> that file's size and modification time. A key whose compiler cannot be stamped (not found
!> on `PATH`, or a bootstrap build without the C helpers) is kept in this process only.
module fpm_meta_probe_cache
    use fpm_filesystem, only: join_path, exists, is_dir, mkdir, which, file_stamp, read_lines
    use fpm_strings, only: string_t
    use fpm_release, only: fpm_version, version_t
    implicit none
    private

    public :: probe_key, probe_lookup, probe_record, PROBE_CACHE_FILE, PROBE_CACHE_MAX_ENTRIES

    !> Name of the record in the build directory
    character(len=*), parameter :: PROBE_CACHE_FILE = "probe_cache.txt"

    !> Most keys the file keeps: adding one to a full file drops the oldest
    integer, parameter :: PROBE_CACHE_MAX_ENTRIES = 64

    !> The first line of the file, which says what it is
    character(len=*), parameter :: HEADER = &
        "# fpm: metapackage probes that passed; delete this file to run them again"

    !> This process's answers, passed or failed
    type(string_t), allocatable, save :: memo_keys(:)
    logical, allocatable, save :: memo_passed(:)

contains

    !> The key of probe `name` run with `flags` through compiler `command`, and whether it may be
    !> read from and written to disk: only when the compiler's file could be stamped.
    subroutine probe_key(name, flags, command, key, disk)
        !> What is probed, e.g. "openmp-fortran"
        character(len=*), intent(in) :: name
        !> The flags the probe passes
        character(len=*), intent(in) :: flags
        !> The compiler command that runs it
        character(len=*), intent(in) :: command
        !> The key
        character(len=:), allocatable, intent(out) :: key
        !> Whether the key may be looked up and stored on disk
        logical, intent(out) :: disk

        character(len=:), allocatable :: stamp

        stamp = compiler_stamp(command)
        disk = len(stamp) > 0
        key = name//'|'//trim(adjustl(flags))//'|fpm '//fpm_version_text()//'|'//trim(command)//'='//stamp

    end subroutine probe_key

    !> Looks `key` up: among this process's answers first, then -- when `disk` -- in the file
    !> under `build_dir`, which holds only passed probes.
    subroutine probe_lookup(build_dir, key, disk, found, passed)
        !> The build directory holding the file; empty means none
        character(len=*), intent(in) :: build_dir
        !> The key, from `probe_key`
        character(len=*), intent(in) :: key
        !> Whether the file may answer
        logical, intent(in) :: disk
        !> Whether an answer was found
        logical, intent(out) :: found
        !> The answer, when found
        logical, intent(out) :: passed

        type(string_t), allocatable :: lines(:)
        integer :: i

        found = .false.
        passed = .false.

        if (allocated(memo_keys)) then
            do i = 1, size(memo_keys)
                if (memo_keys(i)%s == key) then
                    found = .true.
                    passed = memo_passed(i)
                    return
                end if
            end do
        end if

        if (.not. disk .or. len_trim(build_dir) == 0) return
        if (.not. exists(join_path(build_dir, PROBE_CACHE_FILE))) return

        lines = read_lines(join_path(build_dir, PROBE_CACHE_FILE))
        do i = 1, size(lines)
            if (lines(i)%s == key) then
                found = .true.
                passed = .true.
                call remember(key, passed)
                return
            end if
        end do

    end subroutine probe_lookup

    !> Records a probe's answer: for this process always, and in the file under `build_dir`
    !> only when it passed and `disk` allows. A file that cannot be written is left alone: the
    !> record only saves time.
    subroutine probe_record(build_dir, key, disk, passed)
        !> The build directory holding the file; empty means none
        character(len=*), intent(in) :: build_dir
        !> The key, from `probe_key`
        character(len=*), intent(in) :: key
        !> Whether the file may be written
        logical, intent(in) :: disk
        !> The probe's answer
        logical, intent(in) :: passed

        type(string_t), allocatable :: lines(:)
        character(len=:), allocatable :: path
        integer :: i, first, nkeys, unit, stat

        call remember(key, passed)
        if (.not. passed .or. .not. disk .or. len_trim(build_dir) == 0) return

        path = join_path(build_dir, PROBE_CACHE_FILE)
        if (exists(path)) then
            lines = read_lines(path)
        else
            allocate (lines(0))
        end if

        ! Keys only: the header and anything else that is not one are rewritten or dropped
        nkeys = 0
        do i = 1, size(lines)
            if (lines(i)%s == key) return
            if (is_key(lines(i)%s)) nkeys = nkeys + 1
        end do

        if (.not. is_dir(build_dir)) call mkdir(build_dir)
        open (newunit=unit, file=path, status='replace', action='write', iostat=stat)
        if (stat /= 0) return

        write (unit, '(a)', iostat=stat) HEADER
        ! Keep the newest PROBE_CACHE_MAX_ENTRIES - 1 keys, then add this one
        first = nkeys - (PROBE_CACHE_MAX_ENTRIES - 1) + 1
        nkeys = 0
        do i = 1, size(lines)
            if (.not. is_key(lines(i)%s)) cycle
            nkeys = nkeys + 1
            if (nkeys >= first) write (unit, '(a)', iostat=stat) lines(i)%s
        end do
        write (unit, '(a)', iostat=stat) key
        close (unit)

    end subroutine probe_record

    !> Adds an answer to this process's list
    subroutine remember(key, passed)
        character(len=*), intent(in) :: key
        logical, intent(in) :: passed

        if (.not. allocated(memo_keys)) then
            allocate (memo_keys(0), memo_passed(0))
        end if
        memo_keys = [memo_keys, string_t(key)]
        memo_passed = [memo_passed, passed]

    end subroutine remember

    !> Whether a line of the file is a key, as opposed to the header or a blank line
    logical function is_key(line)
        character(len=*), intent(in) :: line

        is_key = len_trim(line) > 0
        if (is_key) is_key = line(1:1) /= '#'

    end function is_key

    !> The file a compiler command runs, with its size and modification time, as
    !> `"<path>|<size>:<mtime>"`; empty when either cannot be found
    function compiler_stamp(command) result(stamp)
        !> The command, possibly followed by arguments
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: stamp

        character(len=:), allocatable :: exe, path, fstamp
        integer :: blank

        stamp = ''
        exe = trim(adjustl(command))
        if (len(exe) == 0) return
        blank = index(exe, ' ')
        if (blank > 0) exe = exe(1:blank-1)

        if (scan(exe, '/\') > 0) then
            path = exe
        else
            path = which(exe)
        end if
        if (len(path) == 0) return

        fstamp = file_stamp(path)
        if (len(fstamp) == 0) return
        stamp = path//'|'//fstamp

    end function compiler_stamp

    !> fpm's own version, which keys the record: a new fpm may probe differently
    function fpm_version_text() result(text)
        character(len=:), allocatable :: text

        type(version_t) :: version

        version = fpm_version()
        text = version%s()

    end function fpm_version_text

end module fpm_meta_probe_cache
