!> Define some procedures to automate collecting and launching of tests
module testsuite
    use fpm_error, only : error_t, fatal_error
    implicit none
    private

    !> Single precision real numbers
    integer, parameter :: sp = selected_real_kind(6)
    !> Double precision real numbers
    integer, parameter :: dp = selected_real_kind(15)
    !> Char length for integers
    integer, parameter :: i1 = selected_int_kind(2)
    !> Short length for integers
    integer, parameter :: i2 = selected_int_kind(4)
    !> Length of default integers
    integer, parameter :: i4 = selected_int_kind(9)
    !> Long length for integers
    integer, parameter :: i8 = selected_int_kind(18)

    public :: run_testsuite, run_selected, new_unittest, new_testsuite, test_failed
    public :: select_test, select_suite
    public :: check, check_string
    public :: unittest_t, testsuite_t, error_t


   interface check
      module procedure :: check_stat
      module procedure :: check_logical
      module procedure :: check_float_sp
      module procedure :: check_float_dp
      module procedure :: check_int_i1
      module procedure :: check_int_i2
      module procedure :: check_int_i4
      module procedure :: check_int_i8
      module procedure :: check_bool
      module procedure :: check_string
   end interface check


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


    subroutine check_stat(error, stat, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Status of operation
        integer, intent(in) :: stat

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        if (stat /= 0) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Non-zero exit code encountered", more)
            end if
        end if

    end subroutine check_stat


    subroutine check_logical(error, expression, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Result of logical operator
        logical, intent(in) :: expression

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        if (.not.expression) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Condition not fullfilled", more)
            end if
        end if

    end subroutine check_logical


    subroutine check_float_dp(error, actual, expected, message, more, thr, rel)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Found floating point value
        real(dp), intent(in) :: actual

        !> Expected floating point value
        real(dp), intent(in) :: expected

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        !> Allowed threshold for matching floating point values
        real(dp), intent(in), optional :: thr

        !> Check for relative errors instead
        logical, intent(in), optional :: rel

        logical :: relative
        real(dp) :: diff, threshold

        if (present(thr)) then
            threshold = thr
        else
            threshold = epsilon(expected)
        end if

        if (present(rel)) then
            relative = rel
        else
            relative = .false.
        end if

        if (relative) then
            diff = abs(actual - expected) / expected
        else
            diff = abs(actual - expected)
        end if

        if (diff > threshold) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Floating point value missmatch", more)
            end if
        end if

    end subroutine check_float_dp


    subroutine check_float_sp(error, actual, expected, message, more, thr, rel)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Found floating point value
        real(sp), intent(in) :: actual

        !> Expected floating point value
        real(sp), intent(in) :: expected

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        !> Allowed threshold for matching floating point values
        real(sp), intent(in), optional :: thr

        !> Check for relative errors instead
        logical, intent(in), optional :: rel

        logical :: relative
        real(sp) :: diff, threshold

        if (present(thr)) then
            threshold = thr
        else
            threshold = epsilon(expected)
        end if

        if (present(rel)) then
            relative = rel
        else
            relative = .false.
        end if

        if (relative) then
            diff = abs(actual - expected) / expected
        else
            diff = abs(actual - expected)
        end if

        if (diff > threshold) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Floating point value missmatch", more)
            end if
        end if

    end subroutine check_float_sp


    subroutine check_int_i1(error, actual, expected, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Found integer value
        integer(i1), intent(in) :: actual

        !> Expected integer value
        integer(i1), intent(in) :: expected

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        if (expected /= actual) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Integer value missmatch", more)
            end if
        end if

    end subroutine check_int_i1


    subroutine check_int_i2(error, actual, expected, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Found integer value
        integer(i2), intent(in) :: actual

        !> Expected integer value
        integer(i2), intent(in) :: expected

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        if (expected /= actual) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Integer value missmatch", more)
            end if
        end if

    end subroutine check_int_i2


    subroutine check_int_i4(error, actual, expected, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Found integer value
        integer(i4), intent(in) :: actual

        !> Expected integer value
        integer(i4), intent(in) :: expected

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        if (expected /= actual) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Integer value missmatch", more)
            end if
        end if

    end subroutine check_int_i4


    subroutine check_int_i8(error, actual, expected, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Found integer value
        integer(i8), intent(in) :: actual

        !> Expected integer value
        integer(i8), intent(in) :: expected

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        if (expected /= actual) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Integer value missmatch", more)
            end if
        end if

    end subroutine check_int_i8


    subroutine check_bool(error, actual, expected, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Found boolean value
        logical, intent(in) :: actual

        !> Expected boolean value
        logical, intent(in) :: expected

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        if (expected .neqv. actual) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Logical value missmatch", more)
            end if
        end if

    end subroutine check_bool


    subroutine check_string(error, actual, expected, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Found boolean value
        character(len=:), allocatable, intent(in) :: actual

        !> Expected boolean value
        character(len=*), intent(in) :: expected

        !> A detailed message describing the error
        character(len=*), intent(in), optional :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        if (.not.allocated(actual)) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Character variable not allocated", more)
            end if
            return
        end if

        if (expected /= actual) then
            if (present(message)) then
                call test_failed(error, message, more)
            else
                call test_failed(error, "Character value missmatch", more)
            end if
        end if

    end subroutine check_string


    subroutine test_failed(error, message, more)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> A detailed message describing the error
        character(len=*), intent(in) :: message

        !> Another line of error message
        character(len=*), intent(in), optional :: more

        allocate(error)

        if (present(more)) then
            error%message = message // new_line('a') // more
        else
            error%message = message
        end if

    end subroutine test_failed


end module testsuite
