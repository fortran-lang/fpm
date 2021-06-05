!> Define tests for the `fpm_sources` module (parsing routines)
module test_source_parsing
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: get_temp_filename
    use fpm_source_parsing, only: parse_f_source, parse_c_source
    use fpm_model, only: srcfile_t, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                         FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE
    use fpm_strings, only: operator(.in.)
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
            & new_unittest("include-stmt", test_include_stmt), &
            & new_unittest("program", test_program), &
            & new_unittest("module", test_module), &
            & new_unittest("program-with-module", test_program_with_module), &
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
                           test_invalid_submodule, should_fail=.true.) &
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

    end subroutine test_intrinsic_modules_used


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

    end subroutine test_program


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
            & 'module  my_mod ! A trailing comment', &
            & 'use module_one', &
            & 'interface', &
            & '  module subroutine f()', &
            & 'end interface', &
            & 'integer :: program', &
            & 'program = 1', &
            & 'program= 1', &
            & 'program =1', &
            & 'program (i) =1', &
            & 'contains', &
            & 'module subroutine&', &
            & ' e()', &
            & 'end subroutine e', &
            & 'module subroutine f()', &
            & 'end subroutine f', &
            & 'module function g()', &
            & 'end function g', &
            & 'module integer function h()', &
            & 'end function h()', &
            & 'module real function i()', &
            & 'end function i()', &
            & 'end module test'
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

    end subroutine test_module


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

    end subroutine test_program_with_module


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

        if (.not.('proto.h' .in. f_source%include_dependencies)) then
            call test_failed(error,'Missing file in include_dependencies')
            return
        end if

        if (.not.('function_body.c' .in. f_source%include_dependencies)) then
            call test_failed(error,'Missing file in include_dependencies')
            return
        end if

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



end module test_source_parsing
