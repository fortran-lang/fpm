!> Define tests for the `fpm_source_cache` module
module test_source_cache
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: get_temp_filename, exists, read_lines, delete_file
    use fpm_model, only: srcfile_t, FPM_UNIT_MODULE
    use fpm_source_cache, only: source_cache_t
    use fpm_source_parsing, only: parse_f_source
    use fpm_strings, only: string_t
    implicit none
    private

    public :: collect_source_cache

    character(len=*), parameter :: module_source(3) = [character(len=18) :: &
        & "module mod_a", " implicit none", "end module mod_a"]

contains

    !> Collect all exported unit tests
    subroutine collect_source_cache(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("store-lookup", test_store_lookup), &
            & new_unittest("miss-unseen", test_miss_unseen), &
            & new_unittest("miss-size-changed", test_miss_size_changed), &
            & new_unittest("miss-config-changed", test_miss_config_changed), &
            & new_unittest("miss-input-changed", test_miss_input_changed), &
            & new_unittest("miss-program-changed", test_miss_program_changed), &
            & new_unittest("persist-reload", test_persist_reload) &
            & ]

    end subroutine collect_source_cache


    !> A stored analysis is returned unchanged for an untouched source file
    subroutine test_store_lookup(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: path, cache_file
        type(source_cache_t) :: cache
        type(srcfile_t) :: source, restored
        logical :: hit

        call new_paths(path, cache_file)
        call write_lines(path, module_source)
        call parse_or_fail(path, source, error)
        if (allocated(error)) then
            call remove_files(path, cache_file)
            return
        end if

        call cache%init(cache_file)
        call cache%store(path, source)

        hit = cache%lookup(path, restored)
        if (.not. hit) then
            call test_failed(error, 'Unchanged source was not found in the cache')
            call remove_files(path, cache_file)
            return
        end if

        call check_module(restored, error)

        call remove_files(path, cache_file)

    end subroutine test_store_lookup


    !> A miss is reported when the cache holds no entry for the file
    subroutine test_miss_unseen(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: path, cache_file
        type(source_cache_t) :: cache
        type(srcfile_t) :: restored
        logical :: hit

        call new_paths(path, cache_file)
        call write_lines(path, module_source)

        call cache%init(cache_file)
        hit = cache%lookup(path, restored)
        if (hit) then
            call test_failed(error, 'An empty cache reported a hit')
        end if

        call remove_files(path, cache_file)

    end subroutine test_miss_unseen


    !> A miss is reported when the source file size changed
    subroutine test_miss_size_changed(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: path, cache_file
        type(source_cache_t) :: cache
        type(srcfile_t) :: source, restored
        logical :: hit

        call new_paths(path, cache_file)
        call write_lines(path, module_source)
        call parse_or_fail(path, source, error)
        if (allocated(error)) then
            call remove_files(path, cache_file)
            return
        end if

        call cache%init(cache_file)
        call cache%store(path, source)

        hit = cache%lookup(path, restored)
        if (.not. hit) then
            call test_failed(error, 'Unchanged source was not found in the cache')
            call remove_files(path, cache_file)
            return
        end if

        ! Same modification time is possible within one second, so the size
        ! alone has to invalidate the entry
        call write_lines(path, [module_source, " ! trailing comment"])
        hit = cache%lookup(path, restored)
        if (hit) then
            call test_failed(error, 'Changed source file still produced a cache hit')
        end if

        call remove_files(path, cache_file)

    end subroutine test_miss_size_changed


    !> A miss is reported when the preprocessing configuration changed
    subroutine test_miss_config_changed(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: path, cache_file
        type(source_cache_t) :: cache
        type(srcfile_t) :: source, restored
        logical :: hit

        call new_paths(path, cache_file)
        call write_lines(path, module_source)
        call parse_or_fail(path, source, error)
        if (allocated(error)) then
            call remove_files(path, cache_file)
            return
        end if

        call cache%init(cache_file)
        call cache%store(path, source, "cpp|FOO")

        hit = cache%lookup(path, restored, "cpp|FOO")
        if (.not. hit) then
            call test_failed(error, 'Entry with an identical configuration was not found')
            call remove_files(path, cache_file)
            return
        end if

        hit = cache%lookup(path, restored, "cpp")
        if (hit) then
            call test_failed(error, 'Entry was reused with a different macro set')
            call remove_files(path, cache_file)
            return
        end if

        hit = cache%lookup(path, restored)
        if (hit) then
            call test_failed(error, 'Preprocessed entry was reused without preprocessing')
        end if

        call remove_files(path, cache_file)

    end subroutine test_miss_config_changed


    !> A miss is reported when an inlined file changed
    subroutine test_miss_input_changed(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: path, cache_file, header
        type(source_cache_t) :: cache
        type(srcfile_t) :: source, restored
        type(string_t) :: inputs(1)
        logical :: hit

        call new_paths(path, cache_file)
        call write_lines(path, module_source)
        call parse_or_fail(path, source, error)
        if (allocated(error)) then
            call remove_files(path, cache_file)
            return
        end if

        header = path//".h"
        call write_lines(header, ["#define FOO 1"])
        inputs(1)%s = header

        call cache%init(cache_file)
        call cache%store(path, source, inputs=inputs)

        hit = cache%lookup(path, restored)
        if (.not. hit) then
            call test_failed(error, 'Entry with unchanged inputs was not found')
            call remove_files(path, cache_file, header)
            return
        end if

        call write_lines(header, ["#define FOO 1", "#define BAR 2"])
        hit = cache%lookup(path, restored)
        if (hit) then
            call test_failed(error, 'Entry was reused after an inlined file changed')
        end if

        call remove_files(path, cache_file, header)

    end subroutine test_miss_input_changed


    !> A miss is reported when the cache was written by another fpm build
    subroutine test_miss_program_changed(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: path, cache_file
        type(source_cache_t) :: cache, reloaded
        type(srcfile_t) :: source, restored
        logical :: hit

        call new_paths(path, cache_file)
        call write_lines(path, module_source)
        call parse_or_fail(path, source, error)
        if (allocated(error)) then
            call remove_files(path, cache_file)
            return
        end if

        call cache%init(cache_file)
        call cache%store(path, source)
        call cache%save()

        ! Where the running executable cannot be identified the cache skips
        ! the check altogether, so there is nothing to test
        if (.not. overwrite_fingerprint(cache_file)) then
            call remove_files(path, cache_file)
            return
        end if

        call reloaded%init(cache_file)
        hit = reloaded%lookup(path, restored)
        if (hit) then
            call test_failed(error, 'Cache written by another fpm build was reused')
        end if

        call remove_files(path, cache_file)

    end subroutine test_miss_program_changed


    !> Entries survive a save and are available to a reloaded cache
    subroutine test_persist_reload(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: path, cache_file
        type(source_cache_t) :: cache, reloaded
        type(srcfile_t) :: source, restored
        logical :: hit

        call new_paths(path, cache_file)
        call write_lines(path, module_source)
        call parse_or_fail(path, source, error)
        if (allocated(error)) then
            call remove_files(path, cache_file)
            return
        end if

        call cache%init(cache_file)
        call cache%store(path, source)
        call cache%save()

        if (.not. exists(cache_file)) then
            call test_failed(error, 'Cache file was not written')
            call remove_files(path, cache_file)
            return
        end if

        call reloaded%init(cache_file)
        hit = reloaded%lookup(path, restored)
        if (.not. hit) then
            call test_failed(error, 'Stored entry was not restored from the cache file')
            call remove_files(path, cache_file)
            return
        end if

        call check_module(restored, error)

        call remove_files(path, cache_file)

    end subroutine test_persist_reload


    !> Create a temporary source file path and a cache file path next to it
    subroutine new_paths(source_path, cache_path)

        character(:), allocatable, intent(out) :: source_path, cache_path

        source_path = get_temp_filename()
        cache_path = source_path//".cache.toml"

    end subroutine new_paths


    !> Delete the files created by a test, if they still exist
    subroutine remove_files(source_path, cache_path, header)

        character(*), intent(in) :: source_path, cache_path
        character(*), intent(in), optional :: header

        call delete_file(source_path)
        call delete_file(cache_path)
        if (present(header)) call delete_file(header)

    end subroutine remove_files


    !> Write lines to a file, replacing any previous content
    subroutine write_lines(path, lines)

        character(*), intent(in) :: path
        character(*), intent(in) :: lines(:)

        integer :: unit, i

        open(newunit=unit, file=path, status='replace', action='write')
        do i = 1, size(lines)
            write(unit, '(a)') lines(i)
        end do
        close(unit)

    end subroutine write_lines


    !> Rewrite the recorded program fingerprint, as if another fpm wrote it
    logical function overwrite_fingerprint(path) result(found)

        character(*), intent(in) :: path

        type(string_t), allocatable :: lines(:)
        integer :: i, unit

        found = .false.
        lines = read_lines(path)
        open(newunit=unit, file=path, status='replace', action='write')
        do i = 1, size(lines)
            if (index(lines(i)%s, 'fingerprint') > 0) then
                found = .true.
                write(unit, '(a)') 'fingerprint = "stale|1"'
            else
                write(unit, '(a)') lines(i)%s
            end if
        end do
        close(unit)

    end function overwrite_fingerprint


    !> Parse a source file, reporting unexpected parse failures
    subroutine parse_or_fail(path, source, error)

        character(*), intent(in) :: path
        type(srcfile_t), intent(out) :: source
        type(error_t), allocatable, intent(inout) :: error

        type(error_t), allocatable :: parse_error

        source = parse_f_source(path, parse_error)
        if (allocated(parse_error)) then
            call test_failed(error, 'Unexpected parse failure of '//path// &
                & ': '//parse_error%message)
        end if

    end subroutine parse_or_fail


    !> Check that a cached analysis is the parsed module
    subroutine check_module(source, error)

        type(srcfile_t), intent(in) :: source
        type(error_t), allocatable, intent(inout) :: error

        if (source%unit_type /= FPM_UNIT_MODULE) then
            call test_failed(error, 'Cached unit type is not FPM_UNIT_MODULE')
            return
        end if

        if (size(source%modules_provided) /= 1) then
            call test_failed(error, 'Cached modules-provided has an unexpected size')
            return
        end if

        if (source%modules_provided(1)%s /= 'mod_a') then
            call test_failed(error, 'Cached module name is not mod_a')
        end if

    end subroutine check_module

end module test_source_cache
