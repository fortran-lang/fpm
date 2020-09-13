!> Driver for unit testing
program fpm_testing
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testsuite, only : run_testsuite, new_testsuite, testsuite_t, &
        & select_suite, run_selected
    use test_toml, only : collect_toml
    use test_manifest, only : collect_manifest
    use test_source_parsing, only : collect_source_parsing
    implicit none
    integer :: stat, is
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_t), allocatable :: testsuite(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    testsuite = [ &
        & new_testsuite("fpm_toml", collect_toml), &
        & new_testsuite("fpm_manifest", collect_manifest), &
        & new_testsuite("fpm_source_parsing", collect_source_parsing) &
        & ]

    call get_argument(1, suite_name)
    call get_argument(2, test_name)

    if (allocated(suite_name)) then
        is = select_suite(testsuite, suite_name)
        if (is > 0 .and. is <= size(testsuite)) then
            if (allocated(test_name)) then
                write(error_unit, fmt) "Suite:", testsuite(is)%name
                call run_selected(testsuite(is)%collect, test_name, error_unit, stat)
                if (stat == -1) then
                    error stop 1
                end if
            else
                write(error_unit, fmt) "Testing:", testsuite(is)%name
                call run_testsuite(testsuite(is)%collect, error_unit, stat)
            end if

            if (stat > 0) then
                write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
                error stop 1
            end if
        else
            write(error_unit, fmt) "Available testsuites"
            do is = 1, size(testsuite)
                write(error_unit, fmt) "-", testsuite(is)%name
            end do
            error stop 1
        end if
    else
        do is = 1, size(testsuite)
            write(error_unit, fmt) "Testing:", testsuite(is)%name
            call run_testsuite(testsuite(is)%collect, error_unit, stat)

            if (stat > 0) then
                write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
                error stop 1
            end if
        end do
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
