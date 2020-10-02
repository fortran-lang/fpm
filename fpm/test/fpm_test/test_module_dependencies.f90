!> Define tests for the `fpm_sources` module (module dependency checking)
module test_module_dependencies
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_sources, only: resolve_module_dependencies
    use fpm_model, only: srcfile_t, srcfile_ptr, &
                FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
                FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
                FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST
    use fpm_strings, only: string_t
    implicit none
    private

    public :: collect_module_dependencies

    interface operator(.in.)
        module procedure srcfile_in
    end interface

contains


    !> Collect all exported unit tests
    subroutine collect_module_dependencies(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("library-module-use", test_library_module_use), &
            & new_unittest("program-module-use", test_program_module_use), &
            & new_unittest("program-with-module", test_program_with_module), &
            & new_unittest("program-own-module-use", test_program_own_module_use), &
            & new_unittest("missing-library-use", &
                            test_missing_library_use, should_fail=.true.), &
            & new_unittest("missing-program-use", &
                            test_missing_program_use, should_fail=.true.), &
            & new_unittest("invalid-library-use", &
                            test_invalid_library_use, should_fail=.true.), &
            & new_unittest("invalid-own-module-use", &
                            test_invalid_own_module_use, should_fail=.true.) &
            ]
            
    end subroutine collect_module_dependencies


    !> Check library module using another library module
    subroutine test_library_module_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)

        sources(1) = new_test_module(file_name="src/my_mod_1.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_1')])
        
        sources(2) = new_test_module(file_name="src/my_mod_2.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_2')], &
                                    uses=[string_t('my_mod_1')])

        call resolve_module_dependencies(sources,error)

        if (allocated(error)) then
            return
        end if

        if (size(sources(1)%file_dependencies)>0) then
            call test_failed(error,'Incorrect number of file_dependencies - expecting zero')
            return
        end if

        if (size(sources(2)%file_dependencies) /= 1) then
            call test_failed(error,'Incorrect number of file_dependencies - expecting one')
            return
        end if

        if (.not.(sources(1) .in. sources(2)%file_dependencies)) then
            call test_failed(error,'Missing file in file_dependencies')
            return
        end if
        
    end subroutine test_library_module_use


    !> Check program using a library module
    subroutine test_program_module_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i
        type(srcfile_t) :: sources(3)

        sources(1) = new_test_module(file_name="src/my_mod_1.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_1')])
        
        sources(2) = new_test_program(file_name="app/my_program.f90", &
                                    scope=FPM_SCOPE_APP, &
                                    uses=[string_t('my_mod_1')])

        sources(3) = new_test_program(file_name="test/my_test.f90", &
                                    scope=FPM_SCOPE_TEST, &
                                    uses=[string_t('my_mod_1')])

        call resolve_module_dependencies(sources,error)

        if (allocated(error)) then
            return
        end if

        if (size(sources(1)%file_dependencies)>0) then
            call test_failed(error,'Incorrect number of file_dependencies - expecting zero')
            return
        end if

        do i=2,3

            if (size(sources(i)%file_dependencies) /= 1) then
                call test_failed(error,'Incorrect number of file_dependencies - expecting one')
                return
            end if

            if (.not.(sources(1) .in. sources(i)%file_dependencies)) then
                call test_failed(error,'Missing file in file_dependencies')
                return
            end if

        end do
        
    end subroutine test_program_module_use


    !> Check program with module in single source file
    !>  (Resulting source object should not include itself as a file dependency)
    subroutine test_program_with_module(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i
        type(srcfile_t) :: sources(1)

        sources(1) = new_test_module(file_name="app/my_program.f90", &
                                    scope = FPM_SCOPE_APP, &
                                    provides=[string_t('app_mod')], &
                                    uses=[string_t('app_mod')])

        call resolve_module_dependencies(sources,error)

        if (allocated(error)) then
            return
        end if

        if (size(sources(1)%file_dependencies)>0) then
            call test_failed(error,'Incorrect number of file_dependencies - expecting zero')
            return
        end if
        
    end subroutine test_program_with_module

    
    !> Check program using a module in same directory
    subroutine test_program_own_module_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)

        sources(1) = new_test_module(file_name="app/app_mod.f90", &
                                    scope = FPM_SCOPE_APP, &
                                    provides=[string_t('app_mod')])
        
        sources(2) = new_test_program(file_name="app/my_program.f90", &
                                    scope=FPM_SCOPE_APP, &
                                    uses=[string_t('app_mod')])

        call resolve_module_dependencies(sources,error)

        if (allocated(error)) then
            return
        end if

        if (size(sources(1)%file_dependencies)>0) then
            call test_failed(error,'Incorrect number of file_dependencies - expecting zero')
            return
        end if

        if (size(sources(2)%file_dependencies) /= 1) then
            call test_failed(error,'Incorrect number of file_dependencies - expecting one')
            return
        end if

        if (.not.(sources(1) .in. sources(2)%file_dependencies)) then
            call test_failed(error,'Missing file in file_dependencies')
            return
        end if
        
    end subroutine test_program_own_module_use


    !> Check missing library module dependency
    subroutine test_missing_library_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)

        sources(1) = new_test_module(file_name="src/my_mod_1.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_1')])
        
        sources(2) = new_test_module(file_name="src/my_mod_2.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_2')], &
                                    uses=[string_t('my_mod_3')])

        call resolve_module_dependencies(sources,error)
        
    end subroutine test_missing_library_use


    !> Check missing program module dependency
    subroutine test_missing_program_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)

        sources(1) = new_test_module(file_name="src/my_mod_1.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_1')])

        sources(2) = new_test_program(file_name="app/my_program.f90", &
                                    scope=FPM_SCOPE_APP, &
                                    uses=[string_t('my_mod_2')])

        call resolve_module_dependencies(sources,error)
        
    end subroutine test_missing_program_use


    !> Check library module using a non-library module
    subroutine test_invalid_library_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)

        sources(1) = new_test_module(file_name="app/app_mod.f90", &
                                    scope = FPM_SCOPE_APP, &
                                    provides=[string_t('app_mod')])
        
        sources(2) = new_test_module(file_name="src/my_mod.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod')], &
                                    uses=[string_t('app_mod')])

        call resolve_module_dependencies(sources,error)
        
    end subroutine test_invalid_library_use


    !> Check program using a non-library module in a different directory
    subroutine test_invalid_own_module_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)

        sources(1) = new_test_module(file_name="app/subdir/app_mod.f90", &
                                    scope = FPM_SCOPE_APP, &
                                    provides=[string_t('app_mod')])
        
        sources(2) = new_test_program(file_name="app/my_program.f90", &
                                    scope=FPM_SCOPE_APP, &
                                    uses=[string_t('app_mod')])

        call resolve_module_dependencies(sources,error)
        
    end subroutine test_invalid_own_module_use


    !> Helper to create a new srcfile_t for a module
    function new_test_module(file_name, scope, uses, provides) result(src)
        character(*), intent(in) :: file_name
        integer, intent(in) :: scope
        type(string_t), intent(in), optional :: uses(:)
        type(string_t), intent(in), optional :: provides(:)
        type(srcfile_t) :: src

        src%file_name = file_name
        src%unit_scope = scope
        src%unit_type = FPM_UNIT_MODULE

        if (present(provides)) then
            src%modules_provided = provides
        else
            allocate(src%modules_provided(0))
        end if

        if (present(uses)) then
            src%modules_used = uses
        else
            allocate(src%modules_used(0))
        end if

        allocate(src%include_dependencies(0))

    end function new_test_module


    !> Helper to create a new srcfile_t for a program
    function new_test_program(file_name, scope, uses) result(src)
        character(*), intent(in) :: file_name
        integer, intent(in) :: scope
        type(string_t), intent(in), optional :: uses(:)
        type(srcfile_t) :: src

        src%file_name = file_name
        src%unit_scope = scope
        src%unit_type = FPM_UNIT_PROGRAM

        if (present(uses)) then
            src%modules_used = uses
        else
            allocate(src%modules_used(0))
        end if

        allocate(src%modules_provided(0))
        allocate(src%include_dependencies(0))

    end function new_test_program


    !> Helper to check if a srcfile is in a list of srcfile_ptr
    logical function srcfile_in(needle,haystack)
        type(srcfile_t), intent(in), target :: needle
        type(srcfile_ptr), intent(in) :: haystack(:)

        integer :: i

        srcfile_in = .false.
        do i=1,size(haystack)
            
            if (associated(haystack(i)%ptr,needle)) then
                srcfile_in = .true.
                return
            end if

        end do

    end function srcfile_in

end module test_module_dependencies
