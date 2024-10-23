module test_os
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: join_path, mkdir, os_delete_dir, is_dir, get_local_prefix, get_home
    use fpm_environment, only: os_is_unix, get_env, set_env, delete_env
    use fpm_os, only: get_absolute_path, get_absolute_path_by_cd, get_current_directory

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
        & new_unittest('set-environment-variable', set_environment), &
        & new_unittest('invalid-tilde-path', invalid_tilde_path, should_fail=.true.), &
        & new_unittest('tilde-correct-separator', tilde_correct_separator), &
        & new_unittest('tilde-wrong-separator', tilde_wrong_separator, should_fail=.true.), &
        & new_unittest('tilde-nonexistent-path', tilde_nonexistent_path, should_fail=.true.), &
        & new_unittest('abs-path-nonexisting', abs_path_nonexisting, should_fail=.true.), &
        & new_unittest('abs-path-root', abs_path_root), &
        & new_unittest('abs-path-home', abs_path_home), &
        & new_unittest('abs-path-cd-root', abs_path_home), &
        & new_unittest('abs-path-cd-home', abs_path_cd_home), &
        & new_unittest('abs-path-cd-current', abs_path_cd_current) &
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
        if (allocated(error)) return

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

    !> Get the absolute path of the root directory.
    subroutine abs_path_root(error)
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: home_path, result

        if (os_is_unix()) then
            call get_absolute_path('/', result, error)
            if (allocated(error)) return

            if (result /= '/') then
                call test_failed(error, "Result '"//result//"' doesn't equal input value: '/'"); return
            end if
        else
            home_path = get_env('HOMEDRIVE','') //'\'

            call get_absolute_path(home_path, result, error)
            if (allocated(error)) return

            if (result /= home_path) then
                call test_failed(error, "Result '"//result//"' doesn't equal input value: '"//home_path//"'"); return
            end if
        end if
    end

    !> Get the absolute path of the home directory.
    subroutine abs_path_home(error)
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: home, result

        call get_home(home, error)
        if (allocated(error)) return

        call get_absolute_path(home, result, error)
        if (allocated(error)) return

        if (result /= home) then
            call test_failed(error, "Result '"//result//"' doesn't equal home directory '"//home//"'"); return
        end if
    end

    !> Get the absolute path of the root directory using `getcwd`/`_getcwd`.
    subroutine abs_path_cd_root(error)
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: home_path, current_dir_before, current_dir_after, result

        call get_current_directory(current_dir_before, error)
        if (allocated(error)) return

        if (os_is_unix()) then
            call get_absolute_path_by_cd('/', result, error)

            if (result /= '/') then
                call test_failed(error, "Result '"//result//"' doesn't equal input value: '/'"); return
            end if
        else
            home_path = get_env('HOMEDRIVE','')//'\'

            call get_absolute_path_by_cd(home_path, result, error)

            if (result /= home_path) then
                call test_failed(error, "Result '"//result//"' doesn't equal input value: '"//home_path//"'"); return
            end if
        end if

        call get_current_directory(current_dir_after, error)
        if (allocated(error)) return

        if (current_dir_before /= current_dir_after) then
            call test_failed(error, "Current directory before getting absolute path '"//current_dir_before// &
            & "' doesn't equal current directory after getting absolute path '"//current_dir_after//"'."); return
        end if
    end

    !> Get the absolute path of the root directory using `getcwd`/`_getcwd`.
    subroutine abs_path_cd_home(error)
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: home, current_dir_before, current_dir_after, result

        call get_current_directory(current_dir_before, error)
        if (allocated(error)) return

        call get_home(home, error)
        if (allocated(error)) return

        call get_absolute_path_by_cd(home, result, error)
        if (allocated(error)) return

        call get_current_directory(current_dir_after, error)
        if (allocated(error)) return

        if (current_dir_before /= current_dir_after) then
            call test_failed(error, "Current directory before getting absolute path '"//current_dir_before// &
            & "' doesn't equal current directory after getting absolute path '"//current_dir_after//"'."); return
        end if

        if (result /= home) then
            call test_failed(error, "Result '"//result//"' doesn't equal home directory '"//home//"'"); return
        end if
    end

    !> Get the absolute path of the current directory using `getcwd`/`_getcwd`.
    subroutine abs_path_cd_current(error)
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: current_dir, result

        call get_current_directory(current_dir, error)
        if (allocated(error)) return

        call get_absolute_path_by_cd('.', result, error)
        if (allocated(error)) return

        if (result /= current_dir) then
            call test_failed(error, "Result '"//result//"' doesn't equal current directory '"//current_dir//"'"); return
        end if
    end

    !> Test creation and deletion of an environment variable
    subroutine set_environment(error)
        type(error_t), allocatable, intent(out) :: error

        character(*), parameter :: vname = 'hiufewhiugw'
        character(*), parameter :: vvalue = '1234567890'

        character(:), allocatable :: old_value,new_value,final_value
        logical :: success

        !> Ensure there's no such variable
        old_value = get_env(vname,default='ERROR')
        if (old_value/='ERROR') then
            call test_failed(error, "There is already an env variable named "//vname)
            return
        end if

        !> Create variable
        success = set_env(vname,value=vvalue)
        if (.not.success) then
            call test_failed(error, "Cannot create environment variable "//vname)
            return
        end if

        !> Check new value
        new_value = get_env(vname,default='ERROR')
        if (new_value/=vvalue) then
            call test_failed(error, "Env "//vname//"="//new_value//'; expected '//vvalue)
            return
        end if

        !> Delete variable
        success = delete_env(vname)
        if (.not.success) then
            call test_failed(error, "Cannot delete environment variable "//vname)
            return
        end if

        !> Ensure it does not exist anymore
        !> Do not test this on Windows: due to a Windows bug, environment variables do not get deleted
        !> https://developercommunity.visualstudio.com/t/-putenv-sname-doesnt-always-delete-windows-copy-of/1587426
        if (os_is_unix()) then
            final_value = get_env(vname,default='ERROR')
            if (final_value/='ERROR') then
                call test_failed(error, "Env "//vname//"="//final_value//'; it should not exist.')
                return
            end if
        endif

    end subroutine set_environment

end module test_os
