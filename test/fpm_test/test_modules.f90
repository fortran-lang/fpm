!> Define tests for the `fpm_module` module (namespaced and non-namespaced modules)
module test_modules
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_targets, only: targets_from_sources, resolve_module_dependencies, &
                            resolve_target_linking, build_target_t, build_target_ptr, &
                            FPM_TARGET_EXECUTABLE, FPM_TARGET_OBJECT, FPM_TARGET_ARCHIVE
    use fpm_model, only: fpm_model_t, srcfile_t,  &
                FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
                FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
                FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST
    use fpm_module, only: module_t
    use fpm_strings, only: string_t, operator(.in.)
    use fpm, only: check_modules_for_duplicates
    implicit none
    private

    public :: collect_namespace_testing

    interface operator(.in.)
        module procedure target_in
    end interface


contains


    !> Collect all exported unit tests
    subroutine collect_namespace_testing(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("same-file-same-package-different-modules", &
                           same_file_same_package_different_modules, should_fail=.false.) &
            ]

    end subroutine collect_namespace_testing

    !> Test two different modules with same file name, same project, different folders
    subroutine same_file_same_package_different_modules(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(fpm_model_t) :: model
        type(build_target_ptr), allocatable :: targets(:)

        allocate(model%external_modules(0))
        allocate(model%packages(1))
        allocate(model%packages(1)%sources(2))

        model%packages(1)%sources(1) = dummy_source(FPM_UNIT_MODULE,file_name="src/a/my_mod.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[module_t('my_mod_1')])

        model%packages(1)%sources(2) = dummy_source(FPM_UNIT_MODULE,file_name="src/b/my_mod.f90", &
                                    scope = FPM_SCOPE_LIB, &
                                    provides=[module_t('my_mod_2')], &
                                    uses=[module_t('my_mod_1')])

        call targets_from_sources(targets,model,.false.,error)
        if (allocated(error)) return

        if (allocated(error)) then
            return
        end if
        if (size(targets) /= 3) then
            call test_failed(error,'Incorrect number of targets - expecting three')
            return
        end if

        call check_target(targets(1)%ptr,type=FPM_TARGET_ARCHIVE,n_depends=2, &
                          deps = [targets(2),targets(3)], &
                          links = targets(2:3), error=error)

        if (allocated(error)) return


        call check_target(targets(2)%ptr,type=FPM_TARGET_OBJECT,n_depends=0, &
                          source=model%packages(1)%sources(1),error=error)

        if (allocated(error)) return


        call check_target(targets(3)%ptr,type=FPM_TARGET_OBJECT,n_depends=1, &
                          deps=[targets(2)],source=model%packages(1)%sources(2),error=error)

        if (allocated(error)) return

    end subroutine same_file_same_package_different_modules

    !> Helper to create a new srcfile_t
    type(srcfile_t) function dummy_source(type,file_name, scope, uses, provides) result(src)
        integer, intent(in) :: type
        character(*), intent(in) :: file_name
        integer, intent(in) :: scope
        type(module_t), intent(in), optional :: uses(:)
        type(module_t), intent(in), optional :: provides(:)

        src%file_name  = file_name
        src%unit_scope = scope
        src%unit_type  = type

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

    end function dummy_source

    !> Helper to check an expected output target
    subroutine check_target(target,type,n_depends,deps,links,source,error)
        type(build_target_t), intent(in) :: target
        integer, intent(in) :: type
        integer, intent(in) :: n_depends
        type(srcfile_t), intent(in), optional :: source
        type(build_target_ptr), intent(in), optional :: deps(:)
        type(build_target_ptr), intent(in), optional :: links(:)
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

        if (present(links)) then

            do i=1,size(links)

                if (.not.(links(i)%ptr%output_file .in. target%link_objects)) then
                    call test_failed(error,'Missing object ('//links(i)%ptr%output_file//&
                                    ') for executable "'//target%output_file//'"')
                    return
                end if

            end do

            if (size(links) > size(target%link_objects)) then

                call test_failed(error,'There are missing link objects for target "'&
                                 //target%output_file//'"')
                return

            elseif (size(links) < size(target%link_objects)) then

                call test_failed(error,'There are more link objects than expected for target "'&
                                 //target%output_file//'"')
                return

            end if

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

end module test_modules
