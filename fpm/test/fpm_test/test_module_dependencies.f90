!> Define tests for the `fpm_sources` module (module dependency checking)
module test_module_dependencies
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_targets, only: targets_from_sources, resolve_module_dependencies
    use fpm_model, only: fpm_model_t, srcfile_t, build_target_t, build_target_ptr, &
                FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
                FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
                FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST, &
                FPM_TARGET_EXECUTABLE, FPM_TARGET_OBJECT, FPM_TARGET_ARCHIVE
    use fpm_strings, only: string_t
    implicit none
    private

    public :: collect_module_dependencies, operator(.in.)

    interface operator(.in.)
        module procedure target_in
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
        type(fpm_model_t) :: model

        model%output_directory = ''

        sources(1) = new_test_source(FPM_UNIT_MODULE,file_name="src/my_mod_1.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_1')])
        
        sources(2) = new_test_source(FPM_UNIT_MODULE,file_name="src/my_mod_2.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_2')], &
                                    uses=[string_t('my_mod_1')])

        call targets_from_sources(model,sources)
        call resolve_module_dependencies(model%targets,error)

        if (allocated(error)) then
            return
        end if

        if (size(model%targets) /= 3) then
            call test_failed(error,'Incorrect number of model%targets - expecting three')
            return
        end if

        call check_target(model%targets(1)%ptr,type=FPM_TARGET_ARCHIVE,n_depends=2, &
                          deps = [model%targets(2),model%targets(3)],error=error)
        
        if (allocated(error)) return


        call check_target(model%targets(2)%ptr,type=FPM_TARGET_OBJECT,n_depends=0, &
                          source=sources(1),error=error)
        
        if (allocated(error)) return
        

        call check_target(model%targets(3)%ptr,type=FPM_TARGET_OBJECT,n_depends=1, &
                          deps=[model%targets(2)],source=sources(2),error=error)
        
        if (allocated(error)) return
        
    end subroutine test_library_module_use


    !> Check a program using a library module
    !>  Each program generates two model%targets: object file and executable
    !> 
    subroutine test_program_module_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call test_scope(FPM_SCOPE_APP,error)
        if (allocated(error)) return

        call test_scope(FPM_SCOPE_TEST,error)
        if (allocated(error)) return

    contains

    subroutine test_scope(exe_scope,error)
        integer, intent(in) :: exe_scope
        type(error_t), allocatable, intent(out) :: error

        integer :: i
        type(srcfile_t) :: sources(3)
        type(fpm_model_t) :: model
        character(:), allocatable :: scope_str

        model%output_directory = ''

        scope_str = merge('FPM_SCOPE_APP ','FPM_SCOPE_TEST',exe_scope==FPM_SCOPE_APP)//' - '

        sources(1) = new_test_source(FPM_UNIT_MODULE,file_name="src/my_mod_1.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_1')])
        
        sources(2) = new_test_source(FPM_UNIT_PROGRAM,file_name="app/my_program.f90", &
                                    scope=exe_scope, &
                                    uses=[string_t('my_mod_1')])

        call targets_from_sources(model,sources)
        call resolve_module_dependencies(model%targets,error)

        if (allocated(error)) then
            return
        end if

        if (size(model%targets) /= 4) then
            call test_failed(error,scope_str//'Incorrect number of model%targets - expecting three')
            return
        end if

        call check_target(model%targets(1)%ptr,type=FPM_TARGET_ARCHIVE,n_depends=1, &
                          deps=[model%targets(2)],error=error)
        
        if (allocated(error)) return

        call check_target(model%targets(2)%ptr,type=FPM_TARGET_OBJECT,n_depends=0, &
                            source=sources(1),error=error)

        if (allocated(error)) return

        call check_target(model%targets(3)%ptr,type=FPM_TARGET_OBJECT,n_depends=1, &
                            deps=[model%targets(2)],source=sources(2),error=error)

        if (allocated(error)) return

        call check_target(model%targets(4)%ptr,type=FPM_TARGET_EXECUTABLE,n_depends=2, &
                            deps=[model%targets(1),model%targets(3)],error=error)

        if (allocated(error)) return

    end subroutine test_scope

    end subroutine test_program_module_use


    !> Check program with module in single source file
    !>  (Resulting target should not include itself as a dependency)
    subroutine test_program_with_module(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i
        type(srcfile_t) :: sources(1)
        type(fpm_model_t) :: model

        model%output_directory = ''

        sources(1) = new_test_source(FPM_UNIT_PROGRAM,file_name="app/my_program.f90", &
                                    scope = FPM_SCOPE_APP, &
                                    provides=[string_t('app_mod')], &
                                    uses=[string_t('app_mod')])

        call targets_from_sources(model,sources)
        call resolve_module_dependencies(model%targets,error)

        if (allocated(error)) then
            return
        end if

        if (size(model%targets) /= 2) then
            write(*,*) size(model%targets)
            call test_failed(error,'Incorrect number of model%targets - expecting two')
            return
        end if

        call check_target(model%targets(1)%ptr,type=FPM_TARGET_OBJECT,n_depends=0, &
                          source=sources(1),error=error)
        
        if (allocated(error)) return

        call check_target(model%targets(2)%ptr,type=FPM_TARGET_EXECUTABLE,n_depends=1, &
                          deps=[model%targets(1)],error=error)
        
        if (allocated(error)) return
        
    end subroutine test_program_with_module

    
    !> Check program using a module in same directory
    subroutine test_program_own_module_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call test_scope(FPM_SCOPE_APP,error)
        if (allocated(error)) return

        call test_scope(FPM_SCOPE_TEST,error)
        if (allocated(error)) return

    contains

    subroutine test_scope(exe_scope,error)
        integer, intent(in) :: exe_scope
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)
        type(fpm_model_t) :: model
        character(:), allocatable :: scope_str

        model%output_directory = ''

        scope_str = merge('FPM_SCOPE_APP ','FPM_SCOPE_TEST',exe_scope==FPM_SCOPE_APP)//' - '

        sources(1) = new_test_source(FPM_UNIT_MODULE,file_name="app/app_mod.f90", &
                                    scope = exe_scope, &
                                    provides=[string_t('app_mod')])
        
        sources(2) = new_test_source(FPM_UNIT_PROGRAM,file_name="app/my_program.f90", &
                                    scope=exe_scope, &
                                    uses=[string_t('app_mod')])

        call targets_from_sources(model,sources)
        call resolve_module_dependencies(model%targets,error)

        if (allocated(error)) then
            return
        end if

        if (size(model%targets) /= 3) then
            call test_failed(error,scope_str//'Incorrect number of model%targets - expecting three')
            return
        end if


        call check_target(model%targets(1)%ptr,type=FPM_TARGET_OBJECT,n_depends=0, &
                          source=sources(1),error=error)
        
        if (allocated(error)) return

        call check_target(model%targets(2)%ptr,type=FPM_TARGET_OBJECT,n_depends=1, &
                          source=sources(2),deps=[model%targets(1)],error=error)
        
        if (allocated(error)) return

        call check_target(model%targets(3)%ptr,type=FPM_TARGET_EXECUTABLE,n_depends=1, &
                           deps=[model%targets(2)],error=error)

        if (allocated(error)) return
        
    end subroutine test_scope
    end subroutine test_program_own_module_use


    !> Check missing library module dependency
    subroutine test_missing_library_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)
        type(fpm_model_t) :: model

        model%output_directory = ''

        sources(1) = new_test_source(FPM_UNIT_MODULE,file_name="src/my_mod_1.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_1')])
        
        sources(2) = new_test_source(FPM_UNIT_MODULE,file_name="src/my_mod_2.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_2')], &
                                    uses=[string_t('my_mod_3')])

        call targets_from_sources(model,sources)
        call resolve_module_dependencies(model%targets,error)
        
    end subroutine test_missing_library_use


    !> Check missing program module dependency
    subroutine test_missing_program_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)
        type(fpm_model_t) :: model

        model%output_directory = ''

        sources(1) = new_test_source(FPM_UNIT_MODULE,file_name="src/my_mod_1.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod_1')])

        sources(2) = new_test_source(FPM_UNIT_PROGRAM,file_name="app/my_program.f90", &
                                    scope=FPM_SCOPE_APP, &
                                    uses=[string_t('my_mod_2')])

        call targets_from_sources(model,sources)
        call resolve_module_dependencies(model%targets,error)
        
    end subroutine test_missing_program_use


    !> Check library module using a non-library module
    subroutine test_invalid_library_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)
        type(fpm_model_t) :: model

        model%output_directory = ''

        sources(1) = new_test_source(FPM_UNIT_MODULE,file_name="app/app_mod.f90", &
                                    scope = FPM_SCOPE_APP, &
                                    provides=[string_t('app_mod')])
        
        sources(2) = new_test_source(FPM_UNIT_MODULE,file_name="src/my_mod.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[string_t('my_mod')], &
                                    uses=[string_t('app_mod')])

        call targets_from_sources(model,sources)
        call resolve_module_dependencies(model%targets,error)
        
    end subroutine test_invalid_library_use


    !> Check program using a non-library module in a different directory
    subroutine test_invalid_own_module_use(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(srcfile_t) :: sources(2)
        type(fpm_model_t) :: model

        model%output_directory = ''

        sources(1) = new_test_source(FPM_UNIT_MODULE,file_name="app/subdir/app_mod.f90", &
                                    scope = FPM_SCOPE_APP, &
                                    provides=[string_t('app_mod')])
        
        sources(2) = new_test_source(FPM_UNIT_PROGRAM,file_name="app/my_program.f90", &
                                    scope=FPM_SCOPE_APP, &
                                    uses=[string_t('app_mod')])

        call targets_from_sources(model,sources)
        call resolve_module_dependencies(model%targets,error)
        
    end subroutine test_invalid_own_module_use


    !> Helper to create a new srcfile_t
    function new_test_source(type,file_name, scope, uses, provides) result(src)
        integer, intent(in) :: type
        character(*), intent(in) :: file_name
        integer, intent(in) :: scope
        type(string_t), intent(in), optional :: uses(:)
        type(string_t), intent(in), optional :: provides(:)
        type(srcfile_t) :: src

        src%file_name = file_name
        src%unit_scope = scope
        src%unit_type = type

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

    end function new_test_source


    !> Helper to check an expected output target
    subroutine check_target(target,type,n_depends,deps,source,error)
        type(build_target_t), intent(in) :: target
        integer, intent(in) :: type
        integer, intent(in) :: n_depends
        type(srcfile_t), intent(in), optional :: source
        type(build_target_ptr), intent(in), optional :: deps(:)
        type(error_t), intent(out), allocatable :: error

        integer :: i

        if (target%target_type /= type) then
            call test_failed(error,'Unexpected target_type for target "'//target%output_file//'"')
            return
        end if

        if (size(target%dependencies) /= n_depends) then
            call test_failed(error,'Wrong number of dependencies for target "'//target%output_file//'"')
            return
        end if

        if (present(deps)) then

            do i=1,size(deps)

                if (.not.(deps(i)%ptr .in. target%dependencies)) then
                    call test_failed(error,'Missing dependency ('//deps(i)%ptr%output_file//&
                                            ') for target "'//target%output_file//'"')
                    return
                end if

            end do

        end if

        if (present(source)) then

            if (allocated(target%source)) then
                if (target%source%file_name /= source%file_name) then
                    call test_failed(error,'Incorrect source ('//target%source%file_name//') for target "'//&
                                            target%output_file//'"'//new_line('a')//' expected "'//source%file_name//'"')
                    return
                end if

            else
                call test_failed(error,'Expecting source for target "'//target%output_file//'" but none found')
                return
            end if

        else

            if (allocated(target%source)) then
                call test_failed(error,'Found source ('//target%source%file_name//') for target "'//&
                                           target%output_file//'" but none expected')
                return
            end if

        end if

    end subroutine check_target


    !> Helper to check if a build target is in a list of build_target_ptr
    logical function target_in(needle,haystack)
        type(build_target_t), intent(in), target :: needle
        type(build_target_ptr), intent(in) :: haystack(:)

        integer :: i

        target_in = .false.
        do i=1,size(haystack)
            
            if (associated(haystack(i)%ptr,needle)) then
                target_in = .true.
                return
            end if

        end do

    end function target_in
    

end module test_module_dependencies
