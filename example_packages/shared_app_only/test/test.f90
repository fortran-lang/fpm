module test_shared_lib
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use shared_lib, only: test_something

    implicit none

    public :: collect


contains

    !> Collect all exported unit tests
    subroutine collect(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ new_unittest("shared_lib", test_shared) ]

    end subroutine collect

    subroutine test_shared(error)
        type(error_type), allocatable, intent(out) :: error
        
        call check(error, test_something(), 123, "Should be test_something==123")
        
    end subroutine test_shared 

end module test_shared_lib

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_shared_lib, only : collect
    implicit none
    integer :: stat
    type(testsuite_type), allocatable :: testsuite
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuite = new_testsuite("shared_lib", collect)

    write(error_unit, fmt) "Testing:", testsuite%name
    call run_testsuite(testsuite%collect, error_unit, stat)

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop 
    end if
end program tester
