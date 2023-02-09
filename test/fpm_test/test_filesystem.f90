module fpm_test_filesystem
    use fpm_testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: canon_path, is_dir, mkdir, os_delete_dir, &
                              join_path
    use fpm_environment, only: OS_WINDOWS, get_os_type, os_is_unix
    implicit none
    private

    public :: collect_filesystem

contains


    !> Collect all exported unit tests
    subroutine collect_filesystem(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("canon-path", test_canon_path), &
            & new_unittest("create-delete-directory", test_mkdir_rmdir) &
            ]

    end subroutine collect_filesystem


    subroutine test_canon_path(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check_string(error, &
            & canon_path("git/project/src/origin"), "git/project/src/origin")
        if (allocated(error)) return

        call check_string(error,  &
            & canon_path("./project/src/origin"), "project/src/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("./project/src///origin/"), "project/src/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("../project/./src/origin/"), "../project/src/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/project//src/origin/"), "/project/src/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/project/src/../origin/"), "/project/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/project/src/../origin/.."), "/project")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/project/src//../origin/."), "/project/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("../project/src/./../origin/."), "../project/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("../project/src/../../../origin/."), "../../origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/../.."), "/")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("././././././/////a/b/.///././////.///c/../../../"), ".")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/./././././/////a/b/.///././////.///c/../../../"), "/")
        if (allocated(error)) return

    end subroutine test_canon_path


    !> Check a character variable against a reference value
    subroutine check_string(error, actual, expected)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Actual string value
        character(len=*), intent(in) :: actual

        !> Expected string value
        character(len=*), intent(in) :: expected

        if (actual /= expected) then
            call test_failed(error, &
                "Character value mismatch "//&
                "expected '"//expected//"' but got '"//actual//"'")
        end if

    end subroutine check_string


    subroutine test_mkdir_rmdir(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check_mkdir(error, join_path("tmpdir","subdir"))
        if (allocated(error)) return

        call check_rmdir(error, "tmpdir")
        if (allocated(error)) return

    end subroutine test_mkdir_rmdir


    !> Create a directory and verify its existence
    subroutine check_mkdir(error, path)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Directory path
        character(len=*), intent(in) :: path

        ! Directory shouldn't exist before it's created
        if (is_dir(path)) then
            call test_failed(error, &
                "Directory path "//path//" already exists before its creation")
            return
        end if

        ! Create directory
        call mkdir(path)

        ! Check that directory is indeed created
        if (.not.is_dir(path)) then
            call test_failed(error, &
                "Directory path "//path//" cannot be created")
        end if

      end subroutine check_mkdir


    !> Create a directory and verify its existence
    subroutine check_rmdir(error, path)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Directory path
        character(len=*), intent(in) :: path

        ! Directory should exist before it's deleted
        if (.not. is_dir(path)) then
            call test_failed(error, &
                "Directory path "//path//" doesn't exist before its deletion")
            return
        end if

        ! Delete directory
        call os_delete_dir(os_is_unix(),path)

        ! Check that directory is indeed deleted
        if (is_dir(path)) then
            call test_failed(error, &
                "Directory path "//path//" cannot be deleted")
        end if

      end subroutine check_rmdir


end module fpm_test_filesystem
