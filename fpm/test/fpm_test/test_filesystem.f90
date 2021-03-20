module test_filesystem
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: canon_path
    implicit none
    private

    public :: collect_filesystem

contains


    !> Collect all exported unit tests
    subroutine collect_filesystem(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("canon-path", test_canon_path) &
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
                "Character value missmatch "//&
                "expected '"//expected//"' but got '"//actual//"'")
        end if

    end subroutine check_string


end module test_filesystem
