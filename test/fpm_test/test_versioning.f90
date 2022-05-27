!> Test implementation of version data type
module test_versioning
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_versioning
    implicit none
    private

    public :: collect_versioning


contains


    !> Collect all exported unit tests
    subroutine collect_versioning(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("valid-version", test_valid_version), &
            & new_unittest("valid-equals", test_valid_equals), &
            & new_unittest("valid-notequals", test_valid_notequals), &
            & new_unittest("valid-compare", test_valid_compare), &
            & new_unittest("valid-match", test_valid_match), &
            & new_unittest("valid-string", test_valid_string), &
            & new_unittest("invalid-empty", test_invalid_empty, should_fail=.true.), &
            & new_unittest("invalid-version1", test_invalid_version1, should_fail=.true.), &
            & new_unittest("invalid-version2", test_invalid_version2, should_fail=.true.), &
            & new_unittest("invalid-version3", test_invalid_version3, should_fail=.true.), &
            & new_unittest("invalid-overflow", test_invalid_overflow, should_fail=.true.)]

    end subroutine collect_versioning


    !> Read valid version strings
    subroutine test_valid_version(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: version

        call new_version(version, "8.9.0", error)
        if (allocated(error)) return

        call new_version(version, "2020.10.003", error)

    end subroutine test_valid_version


    !> Compare versions for equality
    subroutine test_valid_equals(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: v1, v2
        type(version_t) :: varray(4)

        call new_version(v1, [1, 2, 0])
        call new_version(v2, [1, 2])

        if (.not. v1 == v2) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        if (.not. v2 == v1) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        call new_version(v1, [0, 9, 0])
        call new_version(v2, [0, 9])

        if (.not. v1==v2) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        if (.not. v2==v1) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        call new_version(v1, [2020])
        call new_version(v2, [2020, 0])

        if (.not. v1 == v2) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        if (.not. v2 == v1) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        call new_version(v1, [20, 1])
        call new_version(varray(1), [19])
        call new_version(varray(2), [18, 2])
        call new_version(varray(3), [20, 1])
        call new_version(varray(4), [1, 3, 1])

        if (.not. any(v1 == varray)) then
           call test_failed(error, "Version comparison failed")
           return
        end if

    end subroutine test_valid_equals


    !> Compare versions for mismatch
    subroutine test_valid_notequals(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: v1, v2
        type(version_t) :: varray(4)

        call new_version(v1, [2020, 3, 1])
        call new_version(v2, [2020, 3])

        if (.not. v1 /= v2) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        if (.not. v2 /= v1) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        call new_version(v1, [0, 9, 1])
        call new_version(v2, [0, 9])

        if (.not. v1/=v2) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        if (.not. v2/=v1) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        call new_version(v1, [2020])
        call new_version(v2, [0, 2020])

        if (.not. v2 /= v1) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        if (.not. v1 /= v2) then
           call test_failed(error, "Version comparison failed")
           return
        end if

        call new_version(v1, [20, 1])
        call new_version(varray(1), [19])
        call new_version(varray(2), [18, 2])
        call new_version(varray(3), [18, 1])
        call new_version(varray(4), [1, 3, 1])

        if (.not. any(v1 /= varray)) then
           call test_failed(error, "Version comparison failed")
           return
        end if

    end subroutine test_valid_notequals


    !> Relative comparison of versions
    subroutine test_valid_compare(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: v1, v2
        type(version_t) :: varray(4)

        call new_version(v1, [10])
        call new_version(v2, [1])

        if (.not. v1 > v2) then
           call test_failed(error, "Version comparison failed (gt)")
           return
        end if

        if (.not. v1 >= v2) then
           call test_failed(error, "Version comparison failed (ge)")
           return
        end if

        if (.not. v2 < v1) then
           call test_failed(error, "Version comparison failed (lt)")
           return
        end if

        if (.not. v2 <= v1) then
           call test_failed(error, "Version comparison failed (le)")
           return
        end if

        call new_version(v1, [1, 0, 8])
        call new_version(v2, [1, 0])

        if (.not. v1 > v2) then
           call test_failed(error, "Version comparison failed (gt)")
           return
        end if

        if (.not. v1 >= v2) then
           call test_failed(error, "Version comparison failed (ge)")
           return
        end if

        if (.not. v2 < v1) then
           call test_failed(error, "Version comparison failed (lt)")
           return
        end if

        if (.not. v2 <= v1) then
           call test_failed(error, "Version comparison failed (le)")
           return
        end if

        call new_version(v1, [1, 2])
        call new_version(v2, [1, 2, 0])

        if (v1 > v2) then
           call test_failed(error, "Version comparison failed (gt)")
           return
        end if

        if (.not. v1 >= v2) then
           call test_failed(error, "Version comparison failed (ge)")
           return
        end if

        if (v2 < v1) then
           call test_failed(error, "Version comparison failed (lt)")
           return
        end if

        if (.not. v2 <= v1) then
           call test_failed(error, "Version comparison failed (le)")
           return
        end if

        call new_version(v1, [20, 1])
        call new_version(varray(1), [19])
        call new_version(varray(2), [18, 2])
        call new_version(varray(3), [18, 1])
        call new_version(varray(4), [1, 3, 1])

        if (.not. all(v1 > varray)) then
           call test_failed(error, "Version comparison failed (gt)")
           return
        end if

    end subroutine test_valid_compare


    !> Semantic version matching
    subroutine test_valid_match(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: v1, v2

        call new_version(v1, [1, 1, 0])
        call new_version(v2, [1])

        if (.not. (v1 .match. v2)) then
           call test_failed(error, "Version comparison failed (match)")
           return
        end if

        if (v2 .match. v1) then
           call test_failed(error, "Version comparison failed (match)")
           return
        end if

        call new_version(v1, [0, 5, 8])
        call new_version(v2, [0, 5])

        if (.not. (v1 .match. v2)) then
           call test_failed(error, "Version comparison failed (match)")
           return
        end if

        if (v2 .match. v1) then
           call test_failed(error, "Version comparison failed (match)")
           return
        end if

        call new_version(v1, [1, 2])
        call new_version(v2, [1, 2, 0])

        if (.not. (v1 .match. v2)) then
           call test_failed(error, "Version comparison failed (match)")
           return
        end if

        if (.not. (v2 .match. v1)) then
           call test_failed(error, "Version comparison failed (match)")
           return
        end if

    end subroutine test_valid_match


    !> Test if version string is preserved
    subroutine test_valid_string(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=*), parameter :: str_in = "20.1.100"
        character(len=:), allocatable :: str_out
        type(version_t) :: version

        call new_version(version, str_in, error)
        if (allocated(error)) return
        call version%to_string(str_out)

        if (str_in /= str_out) then
           call test_failed(error, "Expected "//str_in//" but got "//str_out)
        end if

    end subroutine test_valid_string


    !> Empty string does not represent a version
    subroutine test_invalid_empty(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: version

        call new_version(version, "", error)

    end subroutine test_invalid_empty


    !> Version is invalid with trailing dots
    subroutine test_invalid_version1(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: version

        call new_version(version, "1.", error)

    end subroutine test_invalid_version1


    !> Version is invalid with multiple dots
    subroutine test_invalid_version2(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: version

        call new_version(version, "1..1", error)

    end subroutine test_invalid_version2


    !> Version is invalid if it is not a version
    subroutine test_invalid_version3(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: version

        call new_version(version, "one", error)

    end subroutine test_invalid_version3


    !> Check if overflows of the internal size constraint are handled gracefully
    subroutine test_invalid_overflow(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(version_t) :: version

        call new_version(version, "0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0", error)

    end subroutine test_invalid_overflow


end module test_versioning
