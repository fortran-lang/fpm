!> Define tests for the `fpm_sources` module (parsing routines)
module test_source_parsing
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: get_temp_filename
    use fpm_source_parsing, only: parse_f_source, parse_c_source, parse_use_statement
    use fpm_model, only: srcfile_t, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                         FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
                         FPM_UNIT_CPPSOURCE, FPM_UNIT_NAME
    use fpm_strings, only: operator(.in.), lower
    use fpm_error, only: file_parse_error, fatal_error
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
            & new_unittest("use-statement",test_use_statement) &
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


end module test_source_parsing
