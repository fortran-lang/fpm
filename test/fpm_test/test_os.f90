module test_os
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: env_variable, join_path, mkdir, os_delete_dir, is_dir, get_local_prefix, get_home
    use fpm_environment, only: os_is_unix
    use fpm_os, only: get_absolute_path

    implicit none
    private
    public :: collect_os

    character(len=*), parameter :: tmp_folder = 'tmp'

contains

    !> Collect unit tests.
    subroutine collect_os(tests)

        !> Unit tests to collect.
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
        & new_unittest('empty-path', empty_path, should_fail=.true.), &
        & new_unittest('only-tilde', only_tilde), &
        & new_unittest('invalid-tilde-path', invalid_tilde_path, should_fail=.true.), &
        & new_unittest('tilde-correct-separator', tilde_correct_separator), &
        & new_unittest('tilde-wrong-separator', tilde_wrong_separator, should_fail=.true.), &
        & new_unittest('tilde-nonexistent-path', tilde_nonexistent_path, should_fail=.true.), &
        & new_unittest('abs-path-nonexisting', abs_path_nonexisting, should_fail=.true.), &
        & new_unittest('abs-path-root', abs_path_root), &
        & new_unittest('abs-path-home', abs_path_home) &
        ]

    end subroutine collect_os

    subroutine delete_tmp_folder
        if (is_dir(tmp_folder)) call os_delete_dir(os_is_unix(), tmp_folder)
    end

    subroutine empty_path(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        call get_absolute_path('', result, error)
    end

    subroutine only_tilde(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: home

        call get_absolute_path('~', result, error)

        if (allocated(error)) then
            call test_failed(error, "Unexpected error resolving '~'")
            return
        end if

        if (.not. allocated(result)) then
            call test_failed(error, "Unexpected null result resolving '~'")
            return
        end if

        call get_home(home, error)
        if (allocated(error)) return

        if (result /= home) then
            call test_failed(error, "Result '"//result//"' doesn't equal home directory '"//home//"'")
            return
        end if

    end subroutine

    subroutine invalid_tilde_path(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        call get_absolute_path('~a', result, error)
    end

    subroutine tilde_correct_separator(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: separator
        character(len=:), allocatable :: home

        if (os_is_unix()) then
            separator = '/'
        else
            separator = '\'
        end if

        call get_absolute_path('~'//separator, result, error)

        call get_home(home, error)
        if (allocated(error)) return

        if (result /= home) then
            call test_failed(error, "Result '"//result//"' doesn't equal home directory '"//home//"'")
            return
        end if
    end

    subroutine tilde_wrong_separator(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: separator

        if (os_is_unix()) then
            separator = '\'
        else
            separator = '/'
        end if

        call get_absolute_path('~'//separator, result, error)
    end

    !> Entering a non-existing path with ~ should fail.
    subroutine tilde_nonexistent_path(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        call get_absolute_path('~/abcde', result, error)
    end

    !> Entering a non-existing absolute path should fail.
    subroutine abs_path_nonexisting(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        call get_absolute_path('/abcde', result, error)
    end

    !> Testing the most obvious absolute path: The root directory.
    subroutine abs_path_root(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: home_drive
        character(len=:), allocatable :: home_path

        if (os_is_unix()) then
            call get_absolute_path('/', result, error)

            if (result /= '/') then
                call test_failed(error, "Result '"//result//"' doesn't equal input value: '/'")
                return
            end if
        else
            call env_variable(home_drive, 'HOMEDRIVE')
            home_path = home_drive//'\'

            call get_absolute_path(home_path, result, error)

            if (result /= home_drive) then
                call test_failed(error, "Result '"//result//"' doesn't equal input value: '"//home_path//"'")
                return
            end if
        end if
    end

    !> Testing an absolute path which is not root. It should not be altered.
    subroutine abs_path_home(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: home

        call get_home(home, error)
        if (allocated(error)) return

        call get_absolute_path(home, result, error)
        if (allocated(error)) return

        if (result /= home) then
            call test_failed(error, "Result '"//result//"' doesn't equal home directory '"//home//"'")
            return
        end if
    end

end module test_os
