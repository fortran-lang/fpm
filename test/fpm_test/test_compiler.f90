!> Define tests for the `fpm_compiler` module
module test_compiler
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, &
        & check_string
    use fpm_environment, only : OS_WINDOWS, OS_LINUX
    use fpm_compiler   , only : compiler_t, new_compiler, tokenize_flags
    use fpm_strings    , only : string_t
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
            & new_unittest("check-fortran-source-runs", test_check_fortran_source_runs), &
            & new_unittest("tokenize-flags", test_tokenize_flags)]

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

        if (compiler%is_intel()) then
            print *, "TODO: test_check_fortran_source_runs fails for Intel compilers"
            return
        end if

        !> Test fortran-source runs
        if (.not.compiler%check_fortran_source_runs("print *, 'Hello world!'; end")) then
            call test_failed(error, "Cannot run Fortran hello world")
            return
        end if

        !> Test with invalid flags
        if (compiler%check_fortran_source_runs("print *, 'Hello world!'; end", &
                                               link_flags=" -some-really-invalid-link-flag")) then
            call test_failed(error, "Invalid link flags did not trigger an error")
            return
        end if
        if (compiler%check_fortran_source_runs("print *, 'Hello world!'; end", &
                                               compile_flags=" -certainly-not-a-build/flag")) then
            call test_failed(error, "Invalid compile flags did not trigger an error")
            return
        end if
        if (compiler%check_fortran_source_runs("print *, 'Hello world!'; end", &
                                               compile_flags=" -certainly-not-a-build/flag", &
                                               link_flags=" -some-really-invalid-link-flag")) then
            call test_failed(error, "Invalid build and link flags did not trigger an error")
            return
        end if

        !> Test the flag check wrapper
        if (compiler%check_flags_supported(compile_flags='-Werror=unknown-flag')) then
            call test_failed(error, "Invalid compile flags did not trigger an error")
            return
        end if
        if (compiler%check_flags_supported(link_flags='-Wl,--nonexistent-linker-option')) then
            call test_failed(error, "Invalid link flags did not trigger an error")
            return
        end if
        if (compiler%check_flags_supported(compile_flags='-Werror=unknown-flag', &
                                           link_flags='-Wl,--nonexistent-linker-option')) then
            call test_failed(error, "Invalid compile and link flags did not trigger an error")
            return
        end if

    end subroutine test_check_fortran_source_runs

    subroutine test_tokenize_flags(error)
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: flags
        type(string_t), allocatable :: tokens(:)
        integer :: i

        flags = '-I/path/to/include -I /test -I"/path/to/include with spaces" ' // &
            '-I "spaces here too" -L/path/to/lib -lmylib -O2 -g -Wall'
        call tokenize_flags(flags, tokens)

        do i = 1, size(tokens)
            print *, "Tokens ", i, ": ", tokens(i)%s
        end do

        if (tokens(1)%s /= '-I/path/to/include') then
            call test_failed(error, "Tokenization of flags failed: expected '-I/path/to/include'")
            return
        end if
        if (tokens(2)%s /= '-I/test') then
            call test_failed(error, "Tokenization of flags failed: expected '-I/test'")
            return
        end if
        if (tokens(3)%s /= '-I"/path/to/include with spaces"') then
            call test_failed(error, 'Tokenization of flags failed: expected -I"/path/to/include with spaces"')
            return
        end if

        if (size(tokens) /= 9) then
            call test_failed(error, "Tokenization of flags failed: expected 9 tokens")
            return
        end if

    end subroutine test_tokenize_flags

end module test_compiler
