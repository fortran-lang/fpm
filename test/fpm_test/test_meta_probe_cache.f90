!> Tests for the record of metapackage probes (`fpm_meta_probe_cache`)
module test_meta_probe_cache
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_meta_probe_cache, only: probe_key, probe_lookup, probe_record, &
                                    PROBE_CACHE_FILE, PROBE_CACHE_MAX_ENTRIES
    use fpm_filesystem, only: join_path, mkdir, get_temp_filename, read_lines, exists, os_delete_dir
    use fpm_strings, only: string_t
    use fpm_environment, only: os_is_unix
    implicit none
    private

    public :: collect_meta_probe_cache

contains

    !> Collect all exported unit tests
    subroutine collect_meta_probe_cache(tests)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest("probe-cache-round-trip", test_round_trip), &
            & new_unittest("probe-cache-failed-stays-in-process", test_failed_not_on_disk), &
            & new_unittest("probe-cache-key-follows-the-compiler", test_key_follows_compiler), &
            & new_unittest("probe-cache-unstamped-compiler", test_unstamped_compiler), &
            & new_unittest("probe-cache-bounded", test_bounded) &
            ]

    end subroutine collect_meta_probe_cache

    !> A fresh directory, and a stand-in compiler file inside it whose path the key stamps
    subroutine scratch(dir, compiler)
        character(:), allocatable, intent(out) :: dir, compiler
        integer :: unit

        dir = get_temp_filename()
        call mkdir(dir)
        compiler = join_path(dir, "fake-compiler")
        open (newunit=unit, file=compiler, status='replace', action='write')
        write (unit, '(a)') "#!/bin/sh"
        close (unit)

    end subroutine scratch

    !> How many times `key` is a line of the record in `dir`
    integer function count_in_file(dir, key) result(n)
        character(*), intent(in) :: dir, key
        type(string_t), allocatable :: lines(:)
        integer :: i

        n = 0
        if (.not. exists(join_path(dir, PROBE_CACHE_FILE))) return
        lines = read_lines(join_path(dir, PROBE_CACHE_FILE))
        do i = 1, size(lines)
            if (lines(i)%s == key) n = n + 1
        end do

    end function count_in_file

    !> A passed probe is written once and answered; a key written by an earlier call is read
    subroutine test_round_trip(error)
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: dir, compiler, key, earlier
        logical :: disk, found, passed
        integer :: unit

        call scratch(dir, compiler)

        call probe_key("round-trip", "-flag", compiler, key, disk)
        if (.not. disk) then
            call test_failed(error, "a compiler given by an existing path should be stamped")
            return
        end if

        call probe_lookup(dir, key, disk, found, passed)
        if (found) then
            call test_failed(error, "an empty record answered a probe")
            return
        end if

        call probe_record(dir, key, disk, .true.)
        call probe_record(dir, key, disk, .true.)
        if (count_in_file(dir, key) /= 1) then
            call test_failed(error, "a passed probe should be written to the record exactly once")
            return
        end if

        call probe_lookup(dir, key, disk, found, passed)
        if (.not. (found .and. passed)) then
            call test_failed(error, "a recorded probe was not answered as passed")
            return
        end if

        ! A key this process never saw, as an earlier fpm call left it in the file
        call probe_key("written-earlier", "-flag", compiler, earlier, disk)
        open (newunit=unit, file=join_path(dir, PROBE_CACHE_FILE), position='append', action='write')
        write (unit, '(a)') earlier
        close (unit)
        call probe_lookup(dir, earlier, disk, found, passed)
        if (.not. (found .and. passed)) then
            call test_failed(error, "a key recorded by an earlier call was not read from the file")
            return
        end if

        call os_delete_dir(os_is_unix(), dir)

    end subroutine test_round_trip

    !> A failed probe is answered for the rest of the call, and never written down
    subroutine test_failed_not_on_disk(error)
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: dir, compiler, key
        logical :: disk, found, passed

        call scratch(dir, compiler)
        call probe_key("fails", "-flag", compiler, key, disk)

        call probe_record(dir, key, disk, .false.)
        if (count_in_file(dir, key) /= 0) then
            call test_failed(error, "a failed probe was written to the record")
            return
        end if

        call probe_lookup(dir, key, disk, found, passed)
        if (.not. found .or. passed) then
            call test_failed(error, "a failed probe should be answered as failed for the rest of the call")
            return
        end if

        call os_delete_dir(os_is_unix(), dir)

    end subroutine test_failed_not_on_disk

    !> Rewriting the compiler file, the flags or the probe's name each give another key
    subroutine test_key_follows_compiler(error)
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: dir, compiler, before, after, other
        logical :: disk
        integer :: unit

        call scratch(dir, compiler)
        call probe_key("follows", "-flag", compiler, before, disk)

        call probe_key("follows", "-other-flag", compiler, other, disk)
        if (other == before) then
            call test_failed(error, "the flags should be part of the key")
            return
        end if

        call probe_key("follows-too", "-flag", compiler, other, disk)
        if (other == before) then
            call test_failed(error, "the probe's name should be part of the key")
            return
        end if

        ! A compiler of another size: an upgrade the key must not survive
        open (newunit=unit, file=compiler, status='replace', action='write')
        write (unit, '(a)') "#!/bin/sh"
        write (unit, '(a)') "exit 0"
        close (unit)
        call probe_key("follows", "-flag", compiler, after, disk)
        if (after == before) then
            call test_failed(error, "a changed compiler file should give another key")
            return
        end if

        call os_delete_dir(os_is_unix(), dir)

    end subroutine test_key_follows_compiler

    !> A compiler that cannot be found is probed in this process only, never from the file
    subroutine test_unstamped_compiler(error)
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: dir, compiler, key
        logical :: disk, found, passed

        call scratch(dir, compiler)
        call probe_key("unstamped", "-flag", "fpm-test-no-such-compiler-on-path", key, disk)
        if (disk) then
            call test_failed(error, "a compiler not found on PATH must not be recorded on disk")
            return
        end if

        call probe_record(dir, key, disk, .true.)
        if (count_in_file(dir, key) /= 0) then
            call test_failed(error, "an unstamped key was written to the record")
            return
        end if

        call probe_lookup(dir, key, disk, found, passed)
        if (.not. (found .and. passed)) then
            call test_failed(error, "an unstamped key should still be answered for the rest of the call")
            return
        end if

        call os_delete_dir(os_is_unix(), dir)

    end subroutine test_unstamped_compiler

    !> The record keeps the newest keys only
    subroutine test_bounded(error)
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: dir, compiler, key, first
        type(string_t), allocatable :: lines(:)
        character(len=16) :: name
        logical :: disk
        integer :: i, nkeys

        call scratch(dir, compiler)
        do i = 1, PROBE_CACHE_MAX_ENTRIES + 5
            write (name, '("bounded-",i0)') i
            call probe_key(trim(name), "-flag", compiler, key, disk)
            if (i == 1) first = key
            call probe_record(dir, key, disk, .true.)
        end do

        lines = read_lines(join_path(dir, PROBE_CACHE_FILE))
        nkeys = 0
        do i = 1, size(lines)
            if (len_trim(lines(i)%s) == 0) cycle
            if (lines(i)%s(1:1) == '#') cycle
            nkeys = nkeys + 1
        end do
        if (nkeys /= PROBE_CACHE_MAX_ENTRIES) then
            call test_failed(error, "the record should hold exactly its maximum of keys")
            return
        end if
        if (count_in_file(dir, key) /= 1) then
            call test_failed(error, "the newest key should be kept")
            return
        end if
        if (count_in_file(dir, first) /= 0) then
            call test_failed(error, "the oldest key should be dropped")
            return
        end if

        call os_delete_dir(os_is_unix(), dir)

    end subroutine test_bounded

end module test_meta_probe_cache
