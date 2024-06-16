!> Define tests for the `fpm_compiler` module
module test_compiler
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, &
        & check_string
    use fpm_environment, only : OS_WINDOWS, OS_LINUX
    use fpm_compiler   , only : compiler_t
    implicit none
    private

    public :: collect_compiler


contains

    !> Collect all exported unit tests
    subroutine collect_compiler(testsuite)
        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("check-fortran-source-runs", test_check_fortran_source_runs)]

    end subroutine collect_compiler
    
    subroutine test_check_fortran_source_runs(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error


    end subroutine test_check_fortran_source_runs    


end module test_compiler
