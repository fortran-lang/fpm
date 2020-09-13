!> Define some procedures to automate collecting and launching of tests
module testsuite
    use fpm_error, only : error_t, test_failed => fatal_error
    implicit none
    private

    public :: run_testsuite, new_unittest, test_failed
    public :: check_string
    public :: unittest_t, error_t


    abstract interface
        !> Entry point for tests
        subroutine test_interface(error)
            import :: error_t

            !> Error handling
            type(error_t), allocatable, intent(out) :: error

        end subroutine test_interface
    end interface


    !> Declaration of a unit test
    type :: unittest_t

        !> Name of the test
        character(len=:), allocatable :: name

        !> Entry point of the test
        procedure(test_interface), pointer, nopass :: test => null()

        !> Whether test is supposed to fail
        logical :: should_fail = .false.

    end type unittest_t


    abstract interface
        !> Collect all tests
        subroutine collect_interface(testsuite)
            import :: unittest_t

            !> Collection of tests
            type(unittest_t), allocatable, intent(out) :: testsuite(:)

        end subroutine collect_interface
    end interface


contains


    !> Driver for testsuite
    subroutine run_testsuite(collect, unit, stat)

        !> Collect tests
        procedure(collect_interface) :: collect

        !> Unit for IO
        integer, intent(in) :: unit

        !> Number of failed tests
        integer, intent(out) :: stat

        type(unittest_t), allocatable :: testsuite(:)
        character(len=*), parameter :: fmt = '("#", *(1x, a))'
        character(len=*), parameter :: indent = repeat(" ", 5) // repeat(".", 3)
        type(error_t), allocatable :: error
        integer :: ii

        stat = 0

        call collect(testsuite)

        do ii = 1, size(testsuite)
            write(unit, '("#", 3(1x, a), 1x, "(", i0, "/", i0, ")")') &
                & "Starting", testsuite(ii)%name, "...", ii, size(testsuite)
            call testsuite(ii)%test(error)
            if (allocated(error) .neqv. testsuite(ii)%should_fail) then
                if (testsuite(ii)%should_fail) then
                    write(unit, fmt) indent, testsuite(ii)%name, "[UNEXPECTED PASS]"
                else
                    write(unit, fmt) indent, testsuite(ii)%name, "[FAILED]"
                end if
                stat = stat + 1
            else
                if (testsuite(ii)%should_fail) then
                    write(unit, fmt) indent, testsuite(ii)%name, "[EXPECTED FAIL]"
                else
                    write(unit, fmt) indent, testsuite(ii)%name, "[PASSED]"
                end if
            end if
            if (allocated(error)) then
                write(unit, fmt) "Message:", error%message
            end if
        end do

    end subroutine run_testsuite


    !> Register a new unit test
    function new_unittest(name, test, should_fail) result(self)

        !> Name of the test
        character(len=*), intent(in) :: name

        !> Entry point for the test
        procedure(test_interface) :: test

        !> Whether test is supposed to error or not
        logical, intent(in), optional :: should_fail

        !> Newly registered test
        type(unittest_t) :: self

        self%name = name
        self%test => test
        if (present(should_fail)) self%should_fail = should_fail

    end function new_unittest


    !> Check a deferred length character variable against a reference value
    subroutine check_string(error, actual, expected, name)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Actual string value
        character(len=:), allocatable, intent(in) :: actual

        !> Expected string value
        character(len=*), intent(in) :: expected

        !> Name of the string to check
        character(len=*), intent(in) :: name

        if (.not.allocated(actual)) then
            call test_failed(error, name//" is not set correctly")
            return
        end if

        if (actual /= expected) then
            call test_failed(error, name//" is "//actual// &
                & " but should be "//expected)
        end if

    end subroutine check_string


end module testsuite
