module test_filesystem
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: canon_path, is_dir, mkdir, os_delete_dir, &
                              join_path, is_absolute_path
    use fpm_environment, only: OS_WINDOWS, get_os_type, os_is_unix
    implicit none
    private

    public :: collect_filesystem

contains


    !> Collect all exported unit tests
    subroutine collect_filesystem(tests)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest("canon-path", test_canon_path), &
            & new_unittest("create-delete-directory", test_mkdir_rmdir), &
            & new_unittest("test-is-absolute-path", test_is_absolute_path) &
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

    subroutine test_is_absolute_path(error)
        type(error_t), allocatable, intent(out) :: error

        if (is_absolute_path('.', is_unix=.true.)) then
            call test_failed(error, "Relative path '.' isn't absolute")
            return
        end if
        
        if (is_absolute_path('abc', is_unix=.true.)) then
            call test_failed(error, "Relative path 'abc' isn't absolute")
            return
        end if
        
        if (.not. is_absolute_path('/', is_unix=.true.)) then
            call test_failed(error, "Path '/' is absolute")
            return
        end if
        
        if (.not. is_absolute_path('/abc', is_unix=.true.)) then
            call test_failed(error, "Path '/abc' is absolute")
            return
        end if
        
        if (.not. is_absolute_path('~/', is_unix=.true.)) then
            call test_failed(error, "Path '~/' is absolute")
            return
        end if
        
        if (.not. is_absolute_path('~/', is_unix=.true.)) then
            call test_failed(error, "Path '~/' is absolute")
            return
        end if
        
        if (is_absolute_path('abc', is_unix=.false.)) then
            call test_failed(error, "Relative path 'abc' isn't absolute")
            return
        end if
        
        if (is_absolute_path('..', is_unix=.false.)) then
            call test_failed(error, "Relative path '..' isn't absolute")
            return
        end if
        
        if (is_absolute_path('abc', is_unix=.false.)) then
            call test_failed(error, "Relative path 'abc' isn't absolute")
            return
        end if
        
        if (is_absolute_path('/', is_unix=.false.)) then
            call test_failed(error, "Path '/' isn't absolute on Windows")
            return
        end if
        
        if (is_absolute_path('c/', is_unix=.false.)) then
            call test_failed(error, "Path 'c/' isn't absolute")
            return
        end if
        
        if (.not. is_absolute_path('C:', is_unix=.false.)) then
            call test_failed(error, "Path 'C:' is absolute")
            return
        end if
        
        if (.not. is_absolute_path('x:', is_unix=.false.)) then
            call test_failed(error, "Path 'x:' is absolute")
            return
        end if
        
        if (.not. is_absolute_path('x:xyz', is_unix=.false.)) then
            call test_failed(error, "Path 'x:xyz' is absolute")
            return
        end if
        
        if (is_absolute_path('1:', is_unix=.false.)) then
            call test_failed(error, "Path '1:' isn't absolute")
            return
        end if

    end subroutine test_is_absolute_path

end module test_filesystem
