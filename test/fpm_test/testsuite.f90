!> Define some procedures to automate collecting and launching of tests
module fpm_testsuite
    use fpm_error, only : error_t, test_failed => fatal_error
    implicit none
    private

    public :: run_testsuite, run_selected, new_unittest, new_testsuite, test_failed
    public :: select_test, select_suite
    public :: check_string
    public :: unittest_t, testsuite_t, error_t


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


    !> Collection of unit tests
    type :: testsuite_t

        !> Name of the testsuite
        character(len=:), allocatable :: name

        !> Entry point of the test
        procedure(collect_interface), pointer, nopass :: collect => null()

    end type testsuite_t


    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    character(len=*), parameter :: indent = repeat(" ", 5) // repeat(".", 3)


contains


    !> Driver for testsuite
    subroutine run_testsuite(collect, unit, stat)

        !> Collect tests
        procedure(collect_interface) :: collect

        !> Unit for IO
        integer, intent(in) :: unit

        !> Number of failed tests
        integer, intent(inout) :: stat

        type(unittest_t), allocatable :: testsuite(:)
        integer :: ii

        call collect(testsuite)

        do ii = 1, size(testsuite)
            write(unit, '("#", 3(1x, a), 1x, "(", i0, "/", i0, ")")') &
                & "Starting", testsuite(ii)%name, "...", ii, size(testsuite)
            call run_unittest(testsuite(ii), unit, stat)
        end do

    end subroutine run_testsuite


    !> Driver for selective testing
    subroutine run_selected(collect, name, unit, stat)

        !> Collect tests
        procedure(collect_interface) :: collect

        !> Name of the selected test
        character(len=*), intent(in) :: name

        !> Unit for IO
        integer, intent(in) :: unit

        !> Number of failed tests
        integer, intent(inout) :: stat

        type(unittest_t), allocatable :: testsuite(:)
        integer :: ii

        call collect(testsuite)

        ii = select_test(testsuite, name)

        if (ii > 0 .and. ii <= size(testsuite)) then
            call run_unittest(testsuite(ii), unit, stat)
        else
            write(unit, fmt) "Available tests:"
            do ii = 1, size(testsuite)
                write(unit, fmt) "-", testsuite(ii)%name
            end do
            stat = -huge(ii)
        end if

    end subroutine run_selected


    !> Run a selected unit test
    subroutine run_unittest(test, unit, stat)

        !> Unit test
        type(unittest_t), intent(in) :: test

        !> Unit for IO
        integer, intent(in) :: unit

        !> Number of failed tests
        integer, intent(inout) :: stat

        type(error_t), allocatable :: error

        call test%test(error)
        if (allocated(error) .neqv. test%should_fail) then
            if (test%should_fail) then
                write(unit, fmt) indent, test%name, "[UNEXPECTED PASS]"
            else
                write(unit, fmt) indent, test%name, "[FAILED]"
            end if
            stat = stat + 1
        else
            if (test%should_fail) then
                write(unit, fmt) indent, test%name, "[EXPECTED FAIL]"
            else
                write(unit, fmt) indent, test%name, "[PASSED]"
            end if
        end if
        if (allocated(error)) then
            write(unit, fmt) "Message:", error%message
        end if

    end subroutine run_unittest


    !> Select a unit test from all available tests
    function select_test(tests, name) result(pos)

        !> Name identifying the test suite
        character(len=*), intent(in) :: name

        !> Available unit tests
        type(unittest_t) :: tests(:)

        !> Selected test suite
        integer :: pos

        integer :: it

        pos = 0
        do it = 1, size(tests)
            if (name == tests(it)%name) then
                pos = it
                exit
            end if
        end do

    end function select_test


    !> Select a test suite from all available suites
    function select_suite(suites, name) result(pos)

        !> Name identifying the test suite
        character(len=*), intent(in) :: name

        !> Available test suites
        type(testsuite_t) :: suites(:)

        !> Selected test suite
        integer :: pos

        integer :: it

        pos = 0
        do it = 1, size(suites)
            if (name == suites(it)%name) then
                pos = it
                exit
            end if
        end do

    end function select_suite


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


    !> Register a new testsuite
    function new_testsuite(name, collect) result(self)

        !> Name of the testsuite
        character(len=*), intent(in) :: name

        !> Entry point to collect tests
        procedure(collect_interface) :: collect

        !> Newly registered testsuite
        type(testsuite_t) :: self

        self%name = name
        self%collect => collect

    end function new_testsuite


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


end module fpm_testsuite
