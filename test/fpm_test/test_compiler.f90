!> Define tests for the `fpm_compiler` module
module test_compiler
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, &
        & check_string
    use fpm_environment, only : OS_WINDOWS, OS_LINUX
    use fpm_compiler   , only : compiler_t, new_compiler, tokenize_flags
    use fpm_strings    , only : string_t, operator(==)
    use fpm_command_line, only: get_fpm_env
    use fpm_compile_commands, only: compile_command_table_t
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
            & new_unittest("check-c-source-runs", test_check_c_source_runs), &
            & new_unittest("check-cxx-source-runs", test_check_cxx_source_runs), &
            & new_unittest("tokenize-flags", test_tokenize_flags), &
            & new_unittest("compile-commands-unix", test_register_compile_command_unix), &
            & new_unittest("compile-commands-windows", test_register_compile_command_windows), &
            & new_unittest("get-default-flags-pic", test_get_default_flags_pic)]

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
        if (compiler%check_flags_supported(compile_flags='-Werror=unknown-flag') &
            .and. .not. compiler%is_intel()) then  ! Intel will not trigger an error
            call test_failed(error, "Invalid compile flags did not trigger an error")
            return
        end if
        if (compiler%check_flags_supported(compile_flags='-not-a-compile-flag')) then
            call test_failed(error, "Invalid compile flags did not trigger an error")
            return
        end if
        if (compiler%check_flags_supported(link_flags='-Wl,--nonexistent-linker-option')) then
            call test_failed(error, "Invalid link flags did not trigger an error")
            return
        end if
        if (compiler%check_flags_supported(compile_flags='-Werror=eunknown-flag', &
                                           link_flags='-Wl,--nonexistent-linker-option')) then
            call test_failed(error, "Invalid compile and link flags did not trigger an error")
            return
        end if

    end subroutine test_check_fortran_source_runs

    subroutine test_check_c_source_runs(error)
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
            call test_failed(error, "Cannot initialize compiler")
            return
        end if

        !> Skip tests if no C compiler is available
        if (len_trim(compiler%cc) == 0) then
            return
        end if

        !> Test C source runs with simple hello world
        if (.not.compiler%check_c_source_runs( &
                '#include <stdio.h>' // new_line('a') // &
                'int main() { printf("Hello C world!"); return 0; }')) then
            call test_failed(error, "Cannot run C hello world with compiler "//compiler%cc)
            return
        end if

        !> Test with invalid source that should fail
        if (compiler%check_c_source_runs( &
                '#include <stdio.h>' // new_line('a') // &
                'int main() { return 1; }')) then  ! Returns error code 1
            call test_failed(error, "C program returning error code 1 did not fail")
            return
        end if

        !> Test with invalid flags
        if (compiler%check_c_source_runs( &
                '#include <stdio.h>' // new_line('a') // &
                'int main() { return 0; }', &
                compile_flags=" -invalid-c-flag")) then
            call test_failed(error, "Invalid C compile flags did not trigger an error")
            return
        end if

        !> Test the C flag check wrapper
        if (compiler%check_c_flags_supported(compile_flags='-not-a-c-flag')) then
            call test_failed(error, "Invalid C compile flags did not trigger an error")
            return
        end if

    end subroutine test_check_c_source_runs

    subroutine test_check_cxx_source_runs(error)
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
            call test_failed(error, "Cannot initialize compiler")
            return
        end if

        !> Skip tests if no C++ compiler is available or if it's set to a space
        if (len_trim(compiler%cxx) == 0 .or. trim(compiler%cxx) == " ") then
            return
        end if

        !> Test C++ source runs with simple hello world
        !> Only fail if we're sure the compiler is available
        if (.not.compiler%check_cxx_source_runs( &
                '#include <cstdio>' // new_line('a') // &
                'int main() { return 0; }')) then
            !> This might fail if C++ compiler is misconfigured, so just skip further tests
            return
        end if

        !> Test with invalid source that should fail
        if (compiler%check_cxx_source_runs( &
                '#include <cstdio>' // new_line('a') // &
                'int main() { return 1; }')) then  ! Returns error code 1
            call test_failed(error, "C++ program returning error code 1 did not fail")
            return
        end if

        !> Test with invalid flags
        if (compiler%check_cxx_source_runs( &
                '#include <cstdio>' // new_line('a') // &
                'int main() { return 0; }', &
                compile_flags=" -invalid-cxx-flag")) then
            call test_failed(error, "Invalid C++ compile flags did not trigger an error")
            return
        end if

        !> Test the C++ flag check wrapper
        if (compiler%check_cxx_flags_supported(compile_flags='-not-a-cxx-flag')) then
            call test_failed(error, "Invalid C++ compile flags did not trigger an error")
            return
        end if

    end subroutine test_check_cxx_source_runs

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

    subroutine test_register_compile_command_unix(error)
        type(error_t), allocatable, intent(out) :: error

        type(compile_command_table_t) :: table
        type(string_t), allocatable :: expected(:)
        integer :: i

        call table%register('gfortran -c -I/usr/include -O2 -Wall main.f90', OS_LINUX, error)
        if (allocated(error)) return

        if (size(table%command) /= 1) then
            call test_failed(error, "Expected 1 command registered")
            return
        end if

        associate(c => table%command(1))
            ! Expect these arguments in order
            expected = [ string_t('gfortran'), string_t('-c'), &
                         string_t('-I/usr/include'), string_t('-O2'), &
                         string_t('-Wall'), string_t('main.f90') ]

            if (.not. c%arguments == expected) then
                do i = 1, size(c%arguments)
                    print *, "Argument", i, ":", c%arguments(i)%s
                end do
                call test_failed(error, "Unix compile command arguments did not match expected tokens")
                return
            end if
        end associate
    end subroutine test_register_compile_command_unix

    subroutine test_register_compile_command_windows(error)
        type(error_t), allocatable, intent(out) :: error

        type(compile_command_table_t) :: table
        type(string_t), allocatable :: expected(:)
        integer :: i

        call table%register('ifort /c /I"C:\Program Files\Libs" /O2 /W4 main.f90', OS_WINDOWS, error)
        if (allocated(error)) return

        if (size(table%command) /= 1) then
            call test_failed(error, "Expected 1 command registered")
            return
        end if

        associate(c => table%command(1))
            ! Expected Windows-style tokens
            expected = [ string_t('ifort'), string_t('/c'), &
                         string_t('/IC:\Program Files\Libs'), string_t('/O2'), &
                         string_t('/W4'), string_t('main.f90') ]

            if (.not. c%arguments == expected) then
                do i = 1, size(c%arguments)
                    print *, "Argument", i, ":", c%arguments(i)%s
                end do
                call test_failed(error, "Windows compile command arguments did not match expected tokens")
                return
            end if
        end associate
    end subroutine test_register_compile_command_windows

    subroutine test_get_default_flags_pic(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(compiler_t) :: compiler
        character(:), allocatable :: flags
        integer :: first_pos, second_pos

        !> Create a gfortran compiler
        call new_compiler(compiler, "gfortran", "gcc", "g++", echo=.false., verbose=.false.)

        if (compiler%is_unknown()) then
            call test_failed(error, "Cannot initialize gfortran compiler for PIC test")
            return
        end if

        !> Test that -fPIC is included in release flags
        flags = compiler%get_default_flags(release=.true.)
        first_pos = index(flags, "-fPIC")
        if (first_pos == 0) then
            call test_failed(error, "Release flags should contain -fPIC, got: "//flags)
            return
        end if

        !> Test that -fPIC is not duplicated in release flags
        second_pos = index(flags(first_pos+5:), "-fPIC")
        if (second_pos /= 0) then
            call test_failed(error, "Release flags should not contain duplicate -fPIC, got: "//flags)
            return
        end if

        !> Test that -fPIC is included in debug flags
        flags = compiler%get_default_flags(release=.false.)
        first_pos = index(flags, "-fPIC")
        if (first_pos == 0) then
            call test_failed(error, "Debug flags should contain -fPIC, got: "//flags)
            return
        end if

        !> Test that -fPIC is not duplicated in debug flags
        second_pos = index(flags(first_pos+5:), "-fPIC")
        if (second_pos /= 0) then
            call test_failed(error, "Debug flags should not contain duplicate -fPIC, got: "//flags)
            return
        end if

    end subroutine test_get_default_flags_pic


end module test_compiler
