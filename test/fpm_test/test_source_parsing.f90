!> Define tests for the `fpm_sources` module (parsing routines)
module test_source_parsing
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: get_temp_filename
    use fpm_source_parsing, only: parse_f_source, parse_c_source, parse_use_statement
    use fpm_model, only: srcfile_t, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                         FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
                         FPM_UNIT_CPPSOURCE, FPM_UNIT_NAME
    use fpm_strings, only: operator(.in.), lower, string_t
    use fpm_error, only: file_parse_error, fatal_error
    use fpm_manifest_preprocess, only: preprocess_config_t
    implicit none
    private

    public :: collect_source_parsing

contains

    !> Collect all exported unit tests
    subroutine collect_source_parsing(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("modules-used", test_modules_used), &
            & new_unittest("intrinsic-modules-used", test_intrinsic_modules_used), &
            & new_unittest("nonintrinsic-modules-used", test_nonintrinsic_modules_used), &
            & new_unittest("include-stmt", test_include_stmt), &
            & new_unittest("program", test_program), &
            & new_unittest("program-noheader", test_program_noheader), &
            & new_unittest("program-noheader-2", test_program_noheader_2), &
            & new_unittest("module", test_module), &
            & new_unittest("module-with-subprogram", test_module_with_subprogram), &
            & new_unittest("module-with-c-api", test_module_with_c_api), &
            & new_unittest("module-with-abstract-interface",test_module_with_abstract_interface), &
            & new_unittest("module-end-stmt", test_module_end_stmt), &
            & new_unittest("program-with-module", test_program_with_module), &
            & new_unittest("program-with-abstract-interface", test_program_with_abstract_interface), &
            & new_unittest("submodule", test_submodule), &
            & new_unittest("submodule-ancestor", test_submodule_ancestor), &
            & new_unittest("subprogram", test_subprogram), &
            & new_unittest("csource", test_csource), &
            & new_unittest("invalid-use-stmt", &
                           test_invalid_use_stmt, should_fail=.true.), &
            & new_unittest("invalid-include-stmt", &
                           test_invalid_include_stmt, should_fail=.true.), &
            & new_unittest("invalid-module", &
                           test_invalid_module, should_fail=.true.), &
            & new_unittest("invalid-submodule", &
                           test_invalid_submodule, should_fail=.true.), &
            & new_unittest("use-statement",test_use_statement), &
            & new_unittest("conditional-compilation", test_conditional_compilation), &
            & new_unittest("conditional-compilation-elif", test_conditional_compilation_elif), &
            & new_unittest("conditional-compilation-elif-else", test_conditional_compilation_elif_else), &
            & new_unittest("conditional-compilation_ifdef_else", test_conditional_compilation_ifdef_else), &
            & new_unittest("conditional-if-defined", test_conditional_if_defined), &
            & new_unittest("conditional-macro-comparison", test_conditional_macro_comparison), &
            & new_unittest("define-without-trailing-space", test_define_no_trailing_space), &
            & new_unittest("macro-case-sensitivity", test_macro_case_sensitivity), &
            & new_unittest("if-macro-truthy", test_if_macro_truthy), &
            & new_unittest("if-macro-truthy-from-define", test_if_macro_truthy_from_define) &
            ]

    end subroutine collect_source_parsing


    !> Check parsing of module 'USE' statements
    subroutine test_modules_used(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'program test', &
            & ' use module_one', &
            & ' use :: module_two', &
            & ' use module_three, only: a, b, c', &
            & ' use :: module_four, only: a => b', &
            & '! use module_not_used', &
            & ' implicit none', &
            & 'end program test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_PROGRAM')
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        if (size(f_source%modules_used) /= 4) then
            call test_failed(error,'Incorrect number of modules_used - expecting four')
            return
        end if

        if (.not.('module_one' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        if (.not.('module_two' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        if (.not.('module_three' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        if (.not.('module_four' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        if ('module_not_used' .in. f_source%modules_used) then
            call test_failed(error,'Commented module found in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_modules_used


    !> Check that intrinsic modules are properly ignore
    subroutine test_intrinsic_modules_used(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'program test', &
            & ' use iso_c_binding', &
            & ' use iso_fortran_env', &
            & ' use ieee_arithmetic', &
            & ' use ieee_exceptions', &
            & ' use ieee_features', &
            & ' implicit none', &
            & 'end program test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        if (size(f_source%modules_used) /= 0) then
            call test_failed(error,'Incorrect number of modules_used - expecting zero')
            return
        end if

        if ('iso_c_binding' .in. f_source%modules_used) then
            call test_failed(error,'Intrinsic module found in modules_used')
            return
        end if

        if ('iso_fortran_env' .in. f_source%modules_used) then
            call test_failed(error,'Intrinsic module found in modules_used')
            return
        end if

        if ('ieee_arithmetic' .in. f_source%modules_used) then
            call test_failed(error,'Intrinsic module found in modules_used')
            return
        end if

        if ('ieee_exceptions' .in. f_source%modules_used) then
            call test_failed(error,'Intrinsic module found in modules_used')
            return
        end if

        if ('ieee_features' .in. f_source%modules_used) then
            call test_failed(error,'Intrinsic module found in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_intrinsic_modules_used


    !> Check that intrinsic module names are not ignored if declared non_intrinsic
    subroutine test_nonintrinsic_modules_used(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'program test', &
            & ' use, non_intrinsic :: iso_c_binding', &
            & ' use,     intrinsic :: iso_fortran_env', &
            & ' use, non_intrinsic :: ieee_arithmetic', &
            & ' use, non_intrinsic :: ieee_exceptions', &
            & ' use, non_intrinsic :: ieee_features', &
            & ' use, non_intrinsic :: my_module', &
            & ' implicit none', &
            & 'end program test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        if (size(f_source%modules_used) /= 5) then
            call test_failed(error,'Incorrect number of modules_used - expecting five')
            return
        end if

        if (.not. ('iso_c_binding' .in. f_source%modules_used)) then
            call test_failed(error,'Non-Intrinsic module found in modules_used')
            return
        end if

        if ('iso_fortran_env' .in. f_source%modules_used) then
            call test_failed(error,'Intrinsic module found in modules_used')
            return
        end if

        if (.not. ('ieee_arithmetic' .in. f_source%modules_used)) then
            call test_failed(error,'Non-Intrinsic module not found in modules_used')
            return
        end if

        if (.not. ('ieee_exceptions' .in. f_source%modules_used)) then
            call test_failed(error,'Non-Intrinsic module not found in modules_used')
            return
        end if

        if (.not. ('ieee_features' .in. f_source%modules_used)) then
            call test_failed(error,'Non-Intrinsic module not found in modules_used')
            return
        end if

        if (.not. ('my_module' .in. f_source%modules_used)) then
            call test_failed(error,'Non-Intrinsic module not found in modules_used')
            return
        end if

    end subroutine test_nonintrinsic_modules_used

    !> Check parsing of include statements
    subroutine test_include_stmt(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'program test', &
            & ' implicit none', &
            & ' include  "included_file.f90"', &
            & ' character(*) :: include_comments', &
            & ' include_comments = "some comments"', &
            & ' contains ', &
            & '  include"second_include.f90"', &
            & 'end program test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        if (size(f_source%modules_used) /= 0) then
            call test_failed(error,'Incorrect number of modules_used - expecting zero')
            return
        end if

        if (size(f_source%include_dependencies) /= 2) then
            call test_failed(error,'Incorrect number of include_dependencies - expecting two')
            return
        end if

        if (.not.('included_file.f90' .in. f_source%include_dependencies)) then
            call test_failed(error,'Missing include file in include_dependencies')
            return
        end if

        if (.not.('second_include.f90' .in. f_source%include_dependencies)) then
            call test_failed(error,'Missing include file in include_dependencies')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_include_stmt

    !> Try to parse a simple fortran program
    subroutine test_program(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'program  my_program', &
            & 'use module_one', &
            & 'implicit none', &
            & 'integer :: module', &
            & 'module = 1', &
            & 'module= 1', &
            & 'module =1', &
            & 'module (i) =1', &
            & 'contains', &
            & 'subroutine f()', &
            & 'end subroutine f', &
            & 'end program my_program'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_PROGRAM')
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        if (size(f_source%modules_used) /= 1) then
            call test_failed(error,'Incorrect number of modules_used - expecting one')
            return
        end if

        if (.not.('module_one' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_program

    !> Try to parse a simple fortran program with no "program" header
    subroutine test_program_noheader(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'use program_one', &
            & 'implicit none', &
            & 'integer :: module, program', &
            & 'module = 1', &
            & 'module= 1', &
            & 'module =1', &
            & 'module (i) =1', &
            & 'program = 123', &
            & 'contains', &
            & 'subroutine f()', &
            & 'end subroutine f', &
            & 'end program'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_PROGRAM, found '//&
                                   FPM_UNIT_NAME(f_source%unit_type))
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        if (size(f_source%modules_used) /= 1) then
            call test_failed(error,'Incorrect number of modules_used - expecting one')
            return
        end if

        if (.not.('program_one' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_program_noheader

    !> Try to parse a simple fortran program with no "program" header
    subroutine test_program_noheader_2(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'print *, "Hello World"', &
            & 'end program'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_PROGRAM, found '//&
                                   FPM_UNIT_NAME(f_source%unit_type))
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_program_noheader_2

    !> Try to parse fortran module
    subroutine test_module(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & '#define preprocesor_line_outside', &
            & 'module  my_mod ! A trailing comment', &
            & 'use module_one', &
            & 'interface', &
            & '  module subroutine f() bind(C)', &
            & 'end interface', &
            & 'integer :: program', &
            & 'program = 1', &
            & 'program= 1', &
            & 'program =1', &
            & 'program (i) =1', &
            & 'contains', &
            & 'module subroutine&', &
            & ' e()', &
            & ' integer, parameter :: c = 1', &
            & ' integer :: & ', &
            & '       bind(c)', &
            & ' bind(c) = 1', &
            & 'end subroutine e', &
            & 'module subroutine f()', &
            & 'end subroutine f', &
            & 'module function g()', &
            & 'end function g', &
            & 'module integer function h()', &
            & 'end function h', &
            & 'module real function i()', &
            & 'string = " &', &
            & 'module name"', &
            & 'string = " &', &
            & 'module name !"', &
            & 'end function i', &
            & 'end module test', &
            & '! A trailing comment outside of module'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_MODULE) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_MODULE')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error,'Unexpected modules_provided - expecting one')
            return
        end if

        if (size(f_source%modules_used) /= 1) then
            call test_failed(error,'Incorrect number of modules_used - expecting one')
            return
        end if

        if (.not.('my_mod' .in. f_source%modules_provided)) then
            call test_failed(error,'Missing module in modules_provided')
            return
        end if

        if (.not.('module_one' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_module


    !> Try to parse fortran module with subroutine outside of module
    !>  (this should be detected as FPM_UNIT_SUBPROGRAM not FPM_UNIT_MODULE)
    subroutine test_module_with_subprogram(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'module  my_mod', &
            & 'contains', &
            & 'module subroutine f()', &
            & 'end subroutine f', &
            & 'module function g()', &
            & 'end function g', &
            & 'end module test',&
            & 'function h()', &
            & 'end function'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_SUBPROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_SUBPROGRAM')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error,'Unexpected modules_provided - expecting one')
            return
        end if

        if (size(f_source%modules_used) /= 0) then
            call test_failed(error,'Incorrect number of modules_used - expecting zero')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_module_with_subprogram


    !> Try to parse fortran modules without the full end module statement
    !>  This should be detected as FPM_UNIT_SUBPROGRAM not FPM_UNIT_MODULE
    !>  because we cannot guarantee if non-module subprograms are present
    subroutine test_module_end_stmt(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'module mod1', &
            & 'contains', &
            & 'module subroutine f()', &
            & 'end subroutine f', &
            & 'module function g()', &
            & 'end function g', &
            & 'end', &
            & 'module mod2', &
            & 'contains', &
            & 'module subroutine f()', &
            & 'end subroutine f', &
            & 'module function g()', &
            & 'end function g', &
            & 'end module mod2'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_SUBPROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_SUBPROGRAM')
            return
        end if

        if (size(f_source%modules_provided) /= 2) then
            call test_failed(error,'Unexpected modules_provided - expecting two')
            return
        end if

        if (size(f_source%modules_used) /= 0) then
            call test_failed(error,'Incorrect number of modules_used - expecting zero')
            return
        end if

        if (.not.('mod1' .in. f_source%modules_provided)) then
            call test_failed(error,'Missing module in modules_provided')
            return
        end if

        if (.not.('mod2' .in. f_source%modules_provided)) then
            call test_failed(error,'Missing module in modules_provided')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_module_end_stmt


    !> Try to parse fortran module with exported C-API via bind(c)
    !>  (this should be detected as FPM_UNIT_SUBPROGRAM not FPM_UNIT_MODULE to prevent pruning)
    subroutine test_module_with_c_api(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'module  my_mod', &
            & 'contains', &
            & 'subroutine f() &', &
            & '           bind(C)', &
            & 'end subroutine f', &
            & 'module function g()', &
            & 'end function g', &
            & 'end module test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_SUBPROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_SUBPROGRAM')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error,'Unexpected modules_provided - expecting one')
            return
        end if

        if (size(f_source%modules_used) /= 0) then
            call test_failed(error,'Incorrect number of modules_used - expecting zero')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_module_with_c_api

    !> Check parsing of module exporting an abstract interface
    !>   See also https://github.com/fortran-lang/fpm/issues/1073
    subroutine test_module_with_abstract_interface(error)
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t) :: f_source

        allocate(temp_file,source=get_temp_filename())
        open(file=temp_file,newunit=unit)
        write(unit, '(A)') &
        & 'module foo', &
        & 'abstract interface', &
        & '   subroutine bar1()', &
        & '   end subroutine', &
        & '   subroutine bar2() bind(c)', &
        & '   end subroutine', &
        & 'end interface', &
        & 'end module foo'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) return
        if (f_source%unit_type /= FPM_UNIT_MODULE) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_MODULE')
            return
        end if
        call f_source%test_serialization('srcfile_t: serialization', error)
    end subroutine test_module_with_abstract_interface


    !> Try to parse combined fortran module and program
    !>  Check that parsed unit type is FPM_UNIT_PROGRAM
    subroutine test_program_with_module(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'module my_mod', &
            & 'use module_one', &
            & 'interface', &
            & '  module subroutine f()', &
            & 'end interface', &
            & 'contains', &
            & 'module procedure f()', &
            & 'end procedure f', &
            & 'end module test', &
            & 'program my_program', &
            & 'use my_mod', &
            & 'implicit none', &
            & 'end my_program'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_PROGRAM')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error,'Unexpected modules_provided - expecting one')
            return
        end if

        if (.not.('my_mod' .in. f_source%modules_provided)) then
            call test_failed(error,'Missing module in modules_provided')
            return
        end if

        if (.not.('module_one' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        if (.not.('my_mod' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_program_with_module

    !> Check parsing of interfaces within program unit
    !>   See also https://github.com/fortran-lang/fpm/issues/1073
    subroutine test_program_with_abstract_interface(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'program my_program', &
            & 'implicit none', &
            & 'abstract interface', &
            & '   function cmpfunc(a,b) bind(c)', &
            & '     use, intrinsic :: iso_c_binding', &
            & '     type(c_ptr), intent(in), value :: a, b', &
            & '     integer(c_int) :: cmpfunc', &
            & '   end function', &
            & 'end interface', &
            & 'interface', &
            & '   subroutine qsort(ptr,count,size,comp) bind(c,name="qsort")', &
            & '     use, intrinsic :: iso_c_binding', &
            & '     type(c_ptr), value :: ptr', &
            & '     integer(c_size_t), value :: count, size', &
            & '     type(c_funptr), value :: comp', &
            & 'end interface', &
            & 'end program my_program'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_PROGRAM')
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        ! Intrinsic modules are not counted in `modules_used` (!)
        if (size(f_source%modules_used) /= 0) then
            call test_failed(error,'Incorrect number of modules_used - expecting zero')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_program_with_abstract_interface

    !> Try to parse fortran submodule for ancestry
    subroutine test_submodule(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'submodule (parent) child', &
            & 'use module_one', &
            & 'end submodule test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_SUBMODULE) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_SUBMODULE')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error,'Unexpected modules_provided - expecting one')
            return
        end if

        if (size(f_source%modules_used) /= 2) then
            call test_failed(error,'Incorrect number of modules_used - expecting two')
            return
        end if

        if (.not.('child' .in. f_source%modules_provided)) then
            call test_failed(error,'Missing module in modules_provided')
            return
        end if

        if (.not.('module_one' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        if (.not.('parent' .in. f_source%modules_used)) then
            call test_failed(error,'Missing parent module in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_submodule


    !> Try to parse fortran multi-level submodule for ancestry
    subroutine test_submodule_ancestor(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'submodule (ancestor:parent) child', &
            & 'use module_one', &
            & 'end submodule test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_SUBMODULE) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_SUBMODULE')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error,'Unexpected modules_provided - expecting one')
            return
        end if

        if (size(f_source%modules_used) /= 2) then
            call test_failed(error,'Incorrect number of modules_used - expecting two')
            return
        end if

        if (.not.('child' .in. f_source%modules_provided)) then
            call test_failed(error,'Missing module in modules_provided')
            return
        end if

        if (.not.('module_one' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        if (.not.('parent' .in. f_source%modules_used)) then
            call test_failed(error,'Missing parent module in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_submodule_ancestor


    !> Try to parse standard fortran sub-program (non-module) source
    subroutine test_subprogram(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'subroutine my_sub(a)', &
            & ' use module_one', &
            & ' integer, intent(in) :: a', &
            & 'end subroutine my_sub'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_SUBPROGRAM) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_SUBPROGRAM')
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        if (size(f_source%modules_used) /= 1) then
            call test_failed(error,'Incorrect number of modules_used - expecting one')
            return
        end if

        if (.not.('module_one' .in. f_source%modules_used)) then
            call test_failed(error,'Missing module in modules_used')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_subprogram


    !> Try to parse standard c source for includes
    subroutine test_csource(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())
        temp_file = temp_file//'.c'

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & '#include "proto.h"', &
            & 'void c_func(int a) {', &
            & ' #include "function_body.c"', &
            & ' return', &
            & '}'
        close(unit)

        f_source = parse_c_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_CSOURCE) then
            call test_failed(error,'Wrong unit type detected - expecting FPM_UNIT_CSOURCE')
            return
        end if

        if (size(f_source%modules_provided) /= 0) then
            call test_failed(error,'Unexpected modules_provided - expecting zero')
            return
        end if

        if (size(f_source%modules_used) /= 0) then
            call test_failed(error,'Incorrect number of modules_used - expecting zero')
            return
        end if

        if (size(f_source%include_dependencies) /= 2) then
            call test_failed(error,'Incorrect number of include_dependencies - expecting two')
            return
        end if

        if (allocated(f_source%link_libraries)) then
            call test_failed(error,'Unexpected link_libraries - expecting unallocated')
            return
        end if

        if (size(f_source%parent_modules) /= 0) then
            call test_failed(error,'Incorrect number of parent_modules - expecting zero')
            return
        end if

        if (.not.('proto.h' .in. f_source%include_dependencies)) then
            call test_failed(error,'Missing file in include_dependencies')
            return
        end if

        if (.not.('function_body.c' .in. f_source%include_dependencies)) then
            call test_failed(error,'Missing file in include_dependencies')
            return
        end if

        call f_source%test_serialization('srcfile_t: serialization', error)

    end subroutine test_csource

    !> Try to parse fortran program with invalid use statement
    subroutine test_invalid_use_stmt(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'program test', &
            & 'use module_one', &
            & 'use :: ', &
            & 'end program test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

    end subroutine test_invalid_use_stmt


    !> Try to parse fortran program with invalid use statement
    subroutine test_invalid_include_stmt(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'program test', &
            & ' include "', &
            & 'end program test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

    end subroutine test_invalid_include_stmt


    !> Try to parse incorrect fortran module syntax
    subroutine test_invalid_module(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'module ::my_mod', &
            & 'end module test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

    end subroutine test_invalid_module


    !> Try to parse incorrect fortran submodule syntax
    subroutine test_invalid_submodule(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'submodule :: child', &
            & 'end submodule test'
        close(unit)

        f_source = parse_f_source(temp_file,error)
        if (allocated(error)) then
            return
        end if

        write(*,*) '"',f_source%modules_used(1)%s,'"'

    end subroutine test_invalid_submodule

    !> Parse several USE statements
    subroutine test_use_statement(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(*), parameter :: filename='test_use_statement'
        character(:), allocatable :: line,module_name

        logical :: used,is_intrinsic

        line = 'use, intrinsic:: iso_fortran_env'
        call parse_use_statement(filename,0,line,used,is_intrinsic,module_name,error)
        if (allocated(error)) return

        if (.not. (used .and. &
                   is_intrinsic .and. &
                   module_name=='iso_fortran_env' .and. &
                   used)) then
          call fatal_error(error,'USE statement failed parsing <'//line//'>')
          return
        endif

        line = 'use, non_intrinsic :: iso_fortran_env'
        call parse_use_statement(filename,0,line,used,is_intrinsic,module_name,error)
        if (allocated(error)) return

        if (.not. (used .and. &
                   (.not.is_intrinsic) .and. &
                   module_name=='iso_fortran_env' .and. &
                   used)) then
          call fatal_error(error,'USE statement failed parsing <'//line//'>')
          return
        endif

        line = 'use, non_intrinsic :: my_fortran_module'
        call parse_use_statement(filename,0,line,used,is_intrinsic,module_name,error)
        if (allocated(error)) return

        if (.not. (used .and. &
                   (.not.is_intrinsic) .and. &
                   module_name=='my_fortran_module' .and. &
                   used)) then
          call fatal_error(error,'USE statement failed parsing <'//line//'>')
          return
        endif

        line = 'use, intrinsic :: my_fortran_module'
        call parse_use_statement(filename,0,line,used,is_intrinsic,module_name,error)

        ! This is not an intrinsic module: should detect an error
        if (.not. allocated(error)) then
          call fatal_error(error,'Did not catch invalid intrinsic module in <'//line//'>')
          return
        else
           deallocate(error)
        endif

    end subroutine test_use_statement

    !> Test conditional compilation parsing with CPP preprocessing
    subroutine test_conditional_compilation(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: f_source
        character(:), allocatable :: temp_file
        integer :: unit
        type(preprocess_config_t) :: cpp_config

        ! Test 1: Without preprocessing, should include dependencies from #ifdef blocks
        temp_file = get_temp_filename()

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            'module test_mod', &
            '#ifdef SOME_FEATURE', &
            '  use nonexistent_module', &
            '#endif', &
            '  implicit none', &
            'contains', &
            '  subroutine test_sub()', &
            '    print *, "test"', &
            '  end subroutine', &
            'end module test_mod'
        close(unit)

        ! Parse without preprocessing - should detect the use statement
        f_source = parse_f_source(temp_file, error)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 1) then
            call test_failed(error, 'Expected 1 module dependency without preprocessing, got different count')
            return
        end if

        if (f_source%modules_used(1)%s /= 'nonexistent_module') then
            call test_failed(error, 'Expected nonexistent_module, got: '//f_source%modules_used(1)%s)
            return
        end if

        ! Test 2: With preprocessing enabled, should skip dependencies from #ifdef blocks
        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 0) then
            call test_failed(error, 'Expected 0 module dependencies with preprocessing, got some dependencies')
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_MODULE) then
            call test_failed(error, 'Expected module unit type')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error, 'Expected 1 provided module')
            return
        end if

        if (f_source%modules_provided(1)%s /= 'test_mod') then
            call test_failed(error, 'Expected test_mod, got: '//f_source%modules_provided(1)%s)
            return
        end if

    end subroutine test_conditional_compilation
    !> Test conditional compilation parsing with CPP preprocessing
    subroutine test_conditional_compilation_elif(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: f_source
        character(:), allocatable :: temp_file
        integer :: unit
        type(preprocess_config_t) :: cpp_config

        ! Test 1: Without preprocessing, should include dependencies from #ifdef blocks
        temp_file = get_temp_filename()

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            'module test_mod', &
            '#ifdef SOME_FEATURE', &
            '  use nonexistent_module', &
            '#elif defined(ANOTHER_FEATURE)', &
            ' use another_nonexistent_module', &
            '#endif', &
            '  implicit none', &
            'contains', &
            '  subroutine test_sub()', &
            '    print *, "test"', &
            '  end subroutine', &
            'end module test_mod'
        close(unit)

        ! Parse without preprocessing - should detect the use statement
        f_source = parse_f_source(temp_file, error)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 2) then
            call test_failed(error, 'Expected 2 module dependency without preprocessing, got different count')
            return
        end if

        if (f_source%modules_used(1)%s /= 'nonexistent_module') then
            call test_failed(error, 'Expected nonexistent_module, got: '//f_source%modules_used(1)%s)
            return
        end if

        if (f_source%modules_used(2)%s /= 'another_nonexistent_module') then
            call test_failed(error, 'Expected another_nonexistent_module, got: '//f_source%modules_used(2)%s)
            return
        end if

        ! Test 2: With preprocessing enabled, should skip dependencies from #ifdef blocks
        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 0) then
            call test_failed(error, 'Expected 0 module dependencies with preprocessing, got some dependencies')
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_MODULE) then
            call test_failed(error, 'Expected module unit type')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error, 'Expected 1 provided module')
            return
        end if

        if (f_source%modules_provided(1)%s /= 'test_mod') then
            call test_failed(error, 'Expected test_mod, got: '//f_source%modules_provided(1)%s)
            return
        end if

    end subroutine test_conditional_compilation_elif
    !> Test conditional compilation parsing with CPP preprocessing
    subroutine test_conditional_compilation_elif_else(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: f_source
        character(:), allocatable :: temp_file
        integer :: unit
        type(preprocess_config_t) :: cpp_config

        ! Test 1: Without preprocessing, should include dependencies from #ifdef blocks
        temp_file = get_temp_filename()

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            'module test_mod', &
            '#ifdef SOME_FEATURE', &
            '  use nonexistent_module', &
            '#elif defined(ANOTHER_FEATURE)', &
            ' use another_nonexistent_module', &
            '#else',&
            ' use a_third_module',&
            '#endif', &
            '  implicit none', &
            'contains', &
            '  subroutine test_sub()', &
            '    print *, "test"', &
            '  end subroutine', &
            'end module test_mod'
        close(unit)

        ! Parse without preprocessing - should detect the use statement
        f_source = parse_f_source(temp_file, error)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 3) then
            call test_failed(error, 'Expected 3 module dependency without preprocessing, got different count')
            return
        end if

        if (f_source%modules_used(1)%s /= 'nonexistent_module') then
            call test_failed(error, 'Expected nonexistent_module, got: '//f_source%modules_used(1)%s)
            return
        end if
        if (f_source%modules_used(2)%s /= 'another_nonexistent_module') then
            call test_failed(error, 'Expected another_nonexistent_module, got: '//f_source%modules_used(2)%s)
            return
        end if
        if (f_source%modules_used(3)%s /= 'a_third_module') then
            call test_failed(error, 'Expected a_third_module, got: '//f_source%modules_used(3)%s)
            return
        end if

        ! Test 2: With preprocessing enabled, should skip dependencies from #ifdef blocks
        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 1) then
            call test_failed(error, 'Expected 1 module dependencies with preprocessing, got some dependencies')
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_MODULE) then
            call test_failed(error, 'Expected module unit type')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error, 'Expected 1 provided module')
            return
        end if

        if (f_source%modules_provided(1)%s /= 'test_mod') then
            call test_failed(error, 'Expected test_mod, got: '//f_source%modules_provided(1)%s)
            return
        end if

    end subroutine test_conditional_compilation_elif_else

   subroutine test_conditional_compilation_ifdef_else(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: f_source
        character(:), allocatable :: temp_file
        integer :: unit
        type(preprocess_config_t) :: cpp_config

        ! Test 1: Without preprocessing, should include dependencies from #ifdef blocks
        temp_file = get_temp_filename()

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            'module test_mod', &
            '#ifdef SOME_FEATURE', &
            '  use nonexistent_module', &
            '#else',&
            ' use a_second_module',&
            '#endif', &
            '  implicit none', &
            'contains', &
            '  subroutine test_sub()', &
            '    print *, "test"', &
            '  end subroutine', &
            'end module test_mod'
        close(unit)

        ! Parse without preprocessing - should detect the use statement
        f_source = parse_f_source(temp_file, error)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 2) then
            call test_failed(error, 'Expected 2 module dependency without preprocessing, got different count')
            return
        end if

        if (f_source%modules_used(1)%s /= 'nonexistent_module') then
            call test_failed(error, 'Expected nonexistent_module, got: '//f_source%modules_used(1)%s)
            return
        end if

        if (f_source%modules_used(2)%s /= 'a_second_module') then
            call test_failed(error, 'Expected a_second_module, got: '//f_source%modules_used(2)%s)
            return
        end if

        ! Test 2: With preprocessing enabled, should skip dependencies from #ifdef blocks
        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 1) then
            call test_failed(error, 'Expected 1 module dependencies with preprocessing, got some dependencies')
            return
        end if

        if (f_source%unit_type /= FPM_UNIT_MODULE) then
            call test_failed(error, 'Expected module unit type')
            return
        end if

        if (size(f_source%modules_provided) /= 1) then
            call test_failed(error, 'Expected 1 provided module')
            return
        end if

        if (f_source%modules_provided(1)%s /= 'test_mod') then
            call test_failed(error, 'Expected test_mod, got: '//f_source%modules_provided(1)%s)
            return
        end if

    end subroutine test_conditional_compilation_ifdef_else

    !> Test conditional compilation parsing with #if defined() syntax
    subroutine test_conditional_if_defined(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: f_source
        character(:), allocatable :: temp_file
        integer :: unit
        type(preprocess_config_t) :: cpp_config

        temp_file = get_temp_filename()

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            'module test_mod', &
            '#if defined(SOME_FEATURE)', &
            '  use some_module', &
            '#endif', &
            '#if !defined(OTHER_FEATURE)', &
            '  use other_module', &
            '#endif', &
            '#if SIMPLE_MACRO', &
            '  use third_module', &
            '#endif', &
            '#ifdef FOURTH_FEATURE', &
            '  use fourth_module', &
            '#endif', &
            '#ifndef FIFTH_FEATURE', &
            '  use fifth_module', &
            '  #ifdef NESTED_FEATURE', &
            '    use nested_module', &
            '  #endif', &
            '#endif', &
            '  implicit none', &
            'end module test_mod'
        close(unit)

        ! Without preprocessing - should detect all dependencies (including nested)
        f_source = parse_f_source(temp_file, error)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 6) then
            call test_failed(error, 'Expected 6 module dependencies without preprocessing')
            return
        end if

        ! With preprocessing - should skip all dependencies (no macros defined)
        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"
        
        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        ! Should find 2 dependencies: !defined(OTHER_FEATURE) and #ifndef FIFTH_FEATURE are both true
        ! The nested #ifdef NESTED_FEATURE should be inactive since NESTED_FEATURE is not defined
        if (size(f_source%modules_used) /= 2) then
            if (size(f_source%modules_used) > 0) then
                call test_failed(error, 'Expected 2 module dependencies with preprocessing, got ' // &
                               f_source%modules_used(1)%s // ' (and others)')
            else
                call test_failed(error, 'Expected 2 module dependencies with preprocessing, got 0')
            end if
            return
        end if

        ! Test with some macros defined - should find active dependencies
        call cpp_config%new([string_t('SOME_FEATURE'), string_t('SIMPLE_MACRO')])
        cpp_config%name = "cpp"
        
        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return
        
        if (.not.('some_module' .in. f_source%modules_used)) then ! some_module
            call test_failed(error, 'Expected "some_module" dependency with SOME_FEATURE and SIMPLE_MACRO defined')            
            return
        end if    
        if (.not.('other_module' .in. f_source%modules_used)) then ! other_module
            call test_failed(error, 'Expected "other_module" dependency with SOME_FEATURE and SIMPLE_MACRO defined')            
            return
        end if   
        if (.not.('third_module' .in. f_source%modules_used)) then ! third_module
            call test_failed(error, 'Expected "third_module" dependency with SOME_FEATURE and SIMPLE_MACRO defined')            
            return
        end if     
        if (.not.('fifth_module' .in. f_source%modules_used)) then ! fifth_module
            call test_failed(error, 'Expected "fifth_module" dependency with SOME_FEATURE and SIMPLE_MACRO defined')            
            return
        end if         
        
        if (size(f_source%modules_used) /= 4) then ! all modules
            call test_failed(error, 'Expected 4 module dependencies with SOME_FEATURE and SIMPLE_MACRO defined')
            return
        end if                          
        
        ! Test nested condition: define outer but not inner macro
        call cpp_config%new([string_t('FIFTH_FEATURE')])  ! This makes #ifndef FIFTH_FEATURE inactive
        cpp_config%name = "cpp"
        
        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 1) then
            if (size(f_source%modules_used) > 0) then
                call test_failed(error, 'Expected 1 module dependency with FIFTH_FEATURE defined, got: ' // &
                               f_source%modules_used(2)%s // ' (and maybe others)')
            else
                call test_failed(error, 'Expected 1 module dependency with FIFTH_FEATURE defined, got 0')
            end if
            return
        end if
        
        ! Test nested condition: define both outer condition (negative) and inner (positive)
        call cpp_config%new([string_t('NESTED_FEATURE')])  ! FIFTH_FEATURE not defined, NESTED_FEATURE defined
        cpp_config%name = "cpp"
        
        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (size(f_source%modules_used) /= 3) then
            call test_failed(error, 'Expected 3 module dependencies with nested conditions active')
            return
        end if

    end subroutine test_conditional_if_defined

    !> Test conditional compilation with #if MACRO == VALUE and #if MACRO != VALUE
    subroutine test_conditional_macro_comparison(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: f_source
        character(:), allocatable :: temp_file
        integer :: unit
        type(preprocess_config_t) :: cpp_config

        temp_file = get_temp_filename()

        ! Test file with macro value comparisons and #define
        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            'module test_mod', &
            '#define LOCAL_MACRO 1', &
            '#if EXTERNAL_MACRO == 1', &
            '  use external_eq_module', &
            '#endif', &
            '#if EXTERNAL_MACRO != 1', &
            '  use external_neq_module', &
            '#endif', &
            '#if LOCAL_MACRO == 1', &
            '  use local_eq_module', &
            '#endif', &
            '#if LOCAL_MACRO != 1', &
            '  use local_neq_module', &
            '#endif', &
            '#if UNDEFINED_MACRO == 1', &
            '  use undefined_module', &
            '#endif', &
            '  implicit none', &
            'end module test_mod'
        close(unit)

        ! Test 1: With CPP enabled but no external macros defined
        ! LOCAL_MACRO is defined via #define, EXTERNAL_MACRO is not
        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        ! Should find: local_eq_module (LOCAL_MACRO==1 is true)
        !              external_neq_module (EXTERNAL_MACRO!=1 is true since undefined)
        ! Should NOT find: external_eq_module, local_neq_module, undefined_module
        if (.not.('local_eq_module' .in. f_source%modules_used)) then
            call test_failed(error, 'Expected local_eq_module with #define LOCAL_MACRO 1')
            return
        end if

        if ('local_neq_module' .in. f_source%modules_used) then
            call test_failed(error, 'Should not find local_neq_module when LOCAL_MACRO==1')
            return
        end if

        if ('external_eq_module' .in. f_source%modules_used) then
            call test_failed(error, 'Should not find external_eq_module when EXTERNAL_MACRO is undefined')
            return
        end if

        if (.not.('external_neq_module' .in. f_source%modules_used)) then
            call test_failed(error, 'Expected external_neq_module (UNDEFINED != 1 should be true)')
            return
        end if

        if ('undefined_module' .in. f_source%modules_used) then
            call test_failed(error, 'Should not find undefined_module when UNDEFINED_MACRO is undefined')
            return
        end if

        ! Test 2: With external macro defined to matching value
        call cpp_config%new([string_t('EXTERNAL_MACRO=1')])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (.not.('external_eq_module' .in. f_source%modules_used)) then
            call test_failed(error, 'Expected external_eq_module with EXTERNAL_MACRO=1')
            return
        end if

        if ('external_neq_module' .in. f_source%modules_used) then
            call test_failed(error, 'Should not find external_neq_module when EXTERNAL_MACRO==1')
            return
        end if

        ! Test 3: With external macro defined to non-matching value
        call cpp_config%new([string_t('EXTERNAL_MACRO=2')])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if ('external_eq_module' .in. f_source%modules_used) then
            call test_failed(error, 'Should not find external_eq_module when EXTERNAL_MACRO==2')
            return
        end if

        if (.not.('external_neq_module' .in. f_source%modules_used)) then
            call test_failed(error, 'Expected external_neq_module with EXTERNAL_MACRO=2 (!=1)')
            return
        end if

    end subroutine test_conditional_macro_comparison

    !> Test #define directive without trailing space (issue #1222)
    !> This tests for out-of-bounds access when parsing #define at end of line
    subroutine test_define_no_trailing_space(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source
        type(preprocess_config_t) :: cpp_config

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        ! Note: #define SIMPLE_FLAG has no trailing space - this triggered
        ! an out-of-bounds error before the fix
        write(unit, '(a)') &
            & '#define SIMPLE_FLAG', &
            & '#ifdef SIMPLE_FLAG', &
            & '  use flag_module', &
            & '#endif', &
            & 'program test', &
            & '  implicit none', &
            & 'end program test'
        close(unit)

        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
            call test_failed(error, 'Wrong unit type detected - expecting FPM_UNIT_PROGRAM')
            return
        end if

        if (.not.('flag_module' .in. f_source%modules_used)) then
            call test_failed(error, 'Expected flag_module with #define SIMPLE_FLAG (no trailing space)')
            return
        end if

    end subroutine test_define_no_trailing_space

    !> Test that CPP macros are case-sensitive (per CPP standard)
    subroutine test_macro_case_sensitivity(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source
        type(preprocess_config_t) :: cpp_config

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & '! Test #ifdef (uses macro_in_list)', &
            & '#ifdef MY_MACRO', &
            & '  use ifdef_uppercase_module', &
            & '#endif', &
            & '#ifdef my_macro', &
            & '  use ifdef_lowercase_module', &
            & '#endif', &
            & '#ifdef My_Macro', &
            & '  use ifdef_mixedcase_module', &
            & '#endif', &
            & '! Test #if MACRO (uses macro_is_truthy -> get_macro_value)', &
            & '#if MY_MACRO', &
            & '  use if_uppercase_module', &
            & '#endif', &
            & '#if my_macro', &
            & '  use if_lowercase_module', &
            & '#endif', &
            & '#if My_Macro', &
            & '  use if_mixedcase_module', &
            & '#endif', &
            & 'program test', &
            & '  implicit none', &
            & 'end program test'
        close(unit)

        ! Test 1: Define MY_MACRO=1 (uppercase) - should only match uppercase for both #ifdef and #if
        call cpp_config%new([string_t('MY_MACRO=1')])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        ! #ifdef tests (macro_in_list)
        if (.not.('ifdef_uppercase_module' .in. f_source%modules_used)) then
            call test_failed(error, '#ifdef: Expected ifdef_uppercase_module with MY_MACRO defined')
            return
        end if
        if ('ifdef_lowercase_module' .in. f_source%modules_used) then
            call test_failed(error, '#ifdef: Should not find ifdef_lowercase_module - case-sensitive')
            return
        end if
        if ('ifdef_mixedcase_module' .in. f_source%modules_used) then
            call test_failed(error, '#ifdef: Should not find ifdef_mixedcase_module - case-sensitive')
            return
        end if

        ! #if tests (macro_is_truthy -> get_macro_value)
        if (.not.('if_uppercase_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if: Expected if_uppercase_module with MY_MACRO=1 defined')
            return
        end if
        if ('if_lowercase_module' .in. f_source%modules_used) then
            call test_failed(error, '#if: Should not find if_lowercase_module - case-sensitive')
            return
        end if
        if ('if_mixedcase_module' .in. f_source%modules_used) then
            call test_failed(error, '#if: Should not find if_mixedcase_module - case-sensitive')
            return
        end if

        ! Test 2: Define my_macro=1 (lowercase) - should only match lowercase
        call cpp_config%new([string_t('my_macro=1')])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        ! #ifdef tests
        if ('ifdef_uppercase_module' .in. f_source%modules_used) then
            call test_failed(error, '#ifdef: Should not find ifdef_uppercase_module - case-sensitive')
            return
        end if
        if (.not.('ifdef_lowercase_module' .in. f_source%modules_used)) then
            call test_failed(error, '#ifdef: Expected ifdef_lowercase_module with my_macro defined')
            return
        end if

        ! #if tests
        if ('if_uppercase_module' .in. f_source%modules_used) then
            call test_failed(error, '#if: Should not find if_uppercase_module - case-sensitive')
            return
        end if
        if (.not.('if_lowercase_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if: Expected if_lowercase_module with my_macro=1 defined')
            return
        end if

    end subroutine test_macro_case_sensitivity

    !> Test #if MACRO truthy evaluation per CPP semantics
    !> Per CPP: undefined=0, MACRO=0 is false, MACRO=1 is true, MACRO=non-zero is true
    subroutine test_if_macro_truthy(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source
        type(preprocess_config_t) :: cpp_config

        allocate(temp_file, source=get_temp_filename())

        ! Test file with #if MACRO (not #ifdef, not #if defined())
        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & '#if UNDEFINED_MACRO', &
            & '  use undefined_module', &
            & '#endif', &
            & '#if ZERO_MACRO', &
            & '  use zero_module', &
            & '#endif', &
            & '#if ONE_MACRO', &
            & '  use one_module', &
            & '#endif', &
            & '#if NONZERO_MACRO', &
            & '  use nonzero_module', &
            & '#endif', &
            & '#if EMPTY_MACRO', &
            & '  use empty_module', &
            & '#endif', &
            & 'program test', &
            & '  implicit none', &
            & 'end program test'
        close(unit)

        ! Test 1: No macros defined - only UNDEFINED should be tested (evaluates to 0/false)
        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if ('undefined_module' .in. f_source%modules_used) then
            call test_failed(error, 'Undefined macro should evaluate to false in #if')
            return
        end if

        ! Test 2: ZERO_MACRO=0 should be false, ONE_MACRO=1 should be true
        call cpp_config%new([string_t('ZERO_MACRO=0'), string_t('ONE_MACRO=1')])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if ('zero_module' .in. f_source%modules_used) then
            call test_failed(error, 'MACRO=0 should evaluate to false in #if')
            return
        end if

        if (.not.('one_module' .in. f_source%modules_used)) then
            call test_failed(error, 'MACRO=1 should evaluate to true in #if')
            return
        end if

        ! Test 3: Non-zero values should be truthy
        call cpp_config%new([string_t('NONZERO_MACRO=42')])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (.not.('nonzero_module' .in. f_source%modules_used)) then
            call test_failed(error, 'MACRO=42 (non-zero) should evaluate to true in #if')
            return
        end if

        ! Test 4: Empty macro (defined without value) - true, like -DMACRO on command line
        call cpp_config%new([string_t('EMPTY_MACRO')])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        if (.not.('empty_module' .in. f_source%modules_used)) then
            call test_failed(error, 'Empty macro should evaluate to true in #if (like -DMACRO)')
            return
        end if

    end subroutine test_if_macro_truthy

    !> Test all CPP conditional formats for macros defined inside source via #define
    !> Formats: #if MACRO, #ifdef MACRO, #if defined(MACRO), #if defined MACRO, #if MACRO == val
    subroutine test_if_macro_truthy_from_define(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit
        character(:), allocatable :: temp_file
        type(srcfile_t), allocatable :: f_source
        type(preprocess_config_t) :: cpp_config

        allocate(temp_file, source=get_temp_filename())

        ! Test file with #define inside source, then various conditional formats
        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & '#define ZERO_MACRO 0', &
            & '#define ONE_MACRO 1', &
            & '#define NONZERO_MACRO 42', &
            & '#define EMPTY_MACRO', &
            & '', &
            & '! Test #if MACRO format', &
            & '#if ZERO_MACRO', &
            & '  use if_zero_module', &
            & '#endif', &
            & '#if ONE_MACRO', &
            & '  use if_one_module', &
            & '#endif', &
            & '#if NONZERO_MACRO', &
            & '  use if_nonzero_module', &
            & '#endif', &
            & '#if EMPTY_MACRO', &
            & '  use if_empty_module', &
            & '#endif', &
            & '#if UNDEFINED_MACRO', &
            & '  use if_undefined_module', &
            & '#endif', &
            & '', &
            & '! Test #ifdef MACRO format', &
            & '#ifdef ZERO_MACRO', &
            & '  use ifdef_zero_module', &
            & '#endif', &
            & '#ifdef ONE_MACRO', &
            & '  use ifdef_one_module', &
            & '#endif', &
            & '#ifdef EMPTY_MACRO', &
            & '  use ifdef_empty_module', &
            & '#endif', &
            & '#ifdef UNDEFINED_MACRO', &
            & '  use ifdef_undefined_module', &
            & '#endif', &
            & '', &
            & '! Test #if defined(MACRO) format (with parentheses)', &
            & '#if defined(ZERO_MACRO)', &
            & '  use defined_paren_zero_module', &
            & '#endif', &
            & '#if defined(UNDEFINED_MACRO)', &
            & '  use defined_paren_undefined_module', &
            & '#endif', &
            & '#if !defined(UNDEFINED_MACRO)', &
            & '  use not_defined_paren_module', &
            & '#endif', &
            & '', &
            & '! Test #if defined MACRO format (without parentheses)', &
            & '#if defined ONE_MACRO', &
            & '  use defined_noparen_one_module', &
            & '#endif', &
            & '#if !defined UNDEFINED_MACRO', &
            & '  use not_defined_noparen_module', &
            & '#endif', &
            & '', &
            & '! Test #if MACRO == value format', &
            & '#if ZERO_MACRO == 0', &
            & '  use eq_zero_module', &
            & '#endif', &
            & '#if ONE_MACRO == 1', &
            & '  use eq_one_module', &
            & '#endif', &
            & '#if NONZERO_MACRO == 42', &
            & '  use eq_nonzero_module', &
            & '#endif', &
            & '#if ONE_MACRO != 0', &
            & '  use neq_one_module', &
            & '#endif', &
            & '#if ZERO_MACRO != 1', &
            & '  use neq_zero_module', &
            & '#endif', &
            & '', &
            & 'program test', &
            & '  implicit none', &
            & 'end program test'
        close(unit)

        ! Enable CPP but don't pass any external macros
        call cpp_config%new([string_t::])
        cpp_config%name = "cpp"

        f_source = parse_f_source(temp_file, error, preprocess=cpp_config)
        if (allocated(error)) return

        ! === #if MACRO tests ===
        if ('if_zero_module' .in. f_source%modules_used) then
            call test_failed(error, '#if MACRO: MACRO=0 should be false')
            return
        end if
        if (.not.('if_one_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if MACRO: MACRO=1 should be true')
            return
        end if
        if (.not.('if_nonzero_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if MACRO: MACRO=42 should be true')
            return
        end if
        if ('if_empty_module' .in. f_source%modules_used) then
            call test_failed(error, '#if MACRO: empty macro from #define should be false (CPP error)')
            return
        end if
        if ('if_undefined_module' .in. f_source%modules_used) then
            call test_failed(error, '#if MACRO: undefined should be false')
            return
        end if

        ! === #ifdef MACRO tests ===
        if (.not.('ifdef_zero_module' .in. f_source%modules_used)) then
            call test_failed(error, '#ifdef MACRO: MACRO=0 should be true (defined)')
            return
        end if
        if (.not.('ifdef_one_module' .in. f_source%modules_used)) then
            call test_failed(error, '#ifdef MACRO: MACRO=1 should be true')
            return
        end if
        if (.not.('ifdef_empty_module' .in. f_source%modules_used)) then
            call test_failed(error, '#ifdef MACRO: empty macro should be true')
            return
        end if
        if ('ifdef_undefined_module' .in. f_source%modules_used) then
            call test_failed(error, '#ifdef MACRO: undefined should be false')
            return
        end if

        ! === #if defined(MACRO) tests ===
        if (.not.('defined_paren_zero_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if defined(MACRO): MACRO=0 should be true (defined)')
            return
        end if
        if ('defined_paren_undefined_module' .in. f_source%modules_used) then
            call test_failed(error, '#if defined(MACRO): undefined should be false')
            return
        end if
        if (.not.('not_defined_paren_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if !defined(MACRO): undefined should be true')
            return
        end if

        ! === #if defined MACRO tests (no parentheses) ===
        if (.not.('defined_noparen_one_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if defined MACRO: MACRO=1 should be true')
            return
        end if
        if (.not.('not_defined_noparen_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if !defined MACRO: undefined should be true')
            return
        end if

        ! === #if MACRO == value tests ===
        if (.not.('eq_zero_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if MACRO == 0: should be true when MACRO=0')
            return
        end if
        if (.not.('eq_one_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if MACRO == 1: should be true when MACRO=1')
            return
        end if
        if (.not.('eq_nonzero_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if MACRO == 42: should be true when MACRO=42')
            return
        end if
        if (.not.('neq_one_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if MACRO != 0: should be true when MACRO=1')
            return
        end if
        if (.not.('neq_zero_module' .in. f_source%modules_used)) then
            call test_failed(error, '#if MACRO != 1: should be true when MACRO=0')
            return
        end if

    end subroutine test_if_macro_truthy_from_define


end module test_source_parsing
