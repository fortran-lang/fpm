!> Define tests for the `fpm_compiler` module
module test_compiler
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, &
        & check_string
    use fpm_environment, only : OS_WINDOWS, OS_LINUX
    use fpm_compiler   , only : compiler_t, new_compiler
    use fpm_command_line, only: get_fpm_env
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
        
        character(:), allocatable :: fc,cc,cxx
        
        
        type(compiler_t) :: compiler

        !> Get default compiler
        fc  = get_fpm_env("FC", default="gfortran")
        cc  = get_fpm_env("CC", default=" ")
        cxx = get_fpm_env("CXX", default=" ")
        
        call new_compiler(compiler, fc, cc, cxx, echo=.false., verbose=.false.)

        if (compiler%is_unknown()) then
            call test_failed(error, "Cannot initialize Fortran compiler")
            return
        end if
        
        !> Test fortran-source runs
        if (.not.compiler%check_fortran_source_runs("print *, 'Hello world!'; end")) then 
            call test_failed(error, "Cannot run Fortran hello world")
            return
        end if        

    end subroutine test_check_fortran_source_runs    


end module test_compiler
