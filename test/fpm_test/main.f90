!> Driver for unit testing
program fpm_testing
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testsuite, only : run_testsuite, new_testsuite, testsuite_t, &
        & select_suite, run_selected
    use fpm_test_toml, only : collect_toml
    use fpm_test_manifest, only : collect_manifest
    use fpm_test_filesystem, only : collect_filesystem
    use fpm_test_source_parsing, only : collect_source_parsing
    use fpm_test_module_dependencies, only : collect_module_dependencies
    use fpm_test_package_dependencies, only : collect_package_dependencies
    use fpm_test_backend, only: collect_backend
    use fpm_test_installer, only : collect_installer
    use fpm_test_versioning, only : collect_versioning
    implicit none
    integer :: stat, is
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_t), allocatable :: suite(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    suite = [ &
        & new_testsuite("fpm_toml", collect_toml), &
        & new_testsuite("fpm_manifest", collect_manifest), &
        & new_testsuite("fpm_filesystem", collect_filesystem), &
        & new_testsuite("fpm_source_parsing", collect_source_parsing), &
        & new_testsuite("fpm_module_dependencies", collect_module_dependencies), &
        & new_testsuite("fpm_package_dependencies", collect_package_dependencies), &
        & new_testsuite("fpm_test_backend", collect_backend), &
        & new_testsuite("fpm_installer", collect_installer), &
        & new_testsuite("fpm_versioning", collect_versioning) &
        & ]

    call get_argument(1, suite_name)
    call get_argument(2, test_name)

    if (allocated(suite_name)) then
        is = select_suite(suite, suite_name)
        if (is > 0 .and. is <= size(suite)) then
            if (allocated(test_name)) then
                write(error_unit, fmt) "Suite:", suite(is)%name
                call run_selected(suite(is)%collect, test_name, error_unit, stat)
                if (stat < 0) then
                    error stop 1
                end if
            else
                write(error_unit, fmt) "Testing:", suite(is)%name
                call run_testsuite(suite(is)%collect, error_unit, stat)
            end if
        else
            write(error_unit, fmt) "Available testsuites"
            do is = 1, size(suite)
                write(error_unit, fmt) "-", suite(is)%name
            end do
            error stop 1
        end if
    else
        do is = 1, size(suite)
            write(error_unit, fmt) "Testing:", suite(is)%name
            call run_testsuite(suite(is)%collect, error_unit, stat)
        end do
    end if

    if (stat > 0) then
       write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
       error stop 1
    end if


contains


    !> Obtain the command line argument at a given index
    subroutine get_argument(idx, arg)

        !> Index of command line argument, range [0:command_argument_count()]
        integer, intent(in) :: idx

        !> Command line argument
        character(len=:), allocatable, intent(out) :: arg

        integer :: length, stat

        call get_command_argument(idx, length=length, status=stat)
        if (stat /= 0) then
            return
        endif

        allocate(character(len=length) :: arg, stat=stat)
        if (stat /= 0) then
            return
        endif

        if (length > 0) then
            call get_command_argument(idx, arg, status=stat)
            if (stat /= 0) then
                deallocate(arg)
                return
            end if
        end if

    end subroutine get_argument


end program fpm_testing
