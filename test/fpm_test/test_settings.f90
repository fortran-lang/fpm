module test_settings
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_settings, only: fpm_global_settings, get_global_settings
    use fpm_filesystem, only: is_dir, join_path, mkdir, filewrite, os_delete_dir, exists
    use fpm_environment, only: os_is_unix
    use fpm_toml, only: new_table
    use fpm_os, only: get_absolute_path, get_current_directory

    implicit none
    private
    public :: collect_settings

    character(len=*), parameter :: tmp_folder = 'tmp'
    character(len=*), parameter :: config_file_name = 'config.toml'

contains

    !> Collect unit tests.
    subroutine collect_settings(tests)

        !> Unit tests to collect.
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
        & new_unittest('no-folder', no_folder, should_fail=.true.), &
        & new_unittest('no-file', no_file, should_fail=.true.), &
        & new_unittest('empty-file', empty_file), &
        & new_unittest('empty-registry-table', empty_registry_table), &
        & new_unittest('wrong-key', wrong_key, should_fail=.true.), &
        & new_unittest('wrong-type', wrong_type, should_fail=.true.), &
        & new_unittest('has-non-existent-path-to-registry', has_non_existent_path_to_registry, should_fail=.true.), &
        & new_unittest('has-existent-path-to-registry', has_existent_path_to_registry), &
        & new_unittest('absolute-path-to-registry', absolute_path_to_registry), &
        & new_unittest('relative-path-to-registry', relative_path_to_registry), &
        & new_unittest('canonical-path-to-registry', canonical_path_to_registry), &
        & new_unittest('has-url-to-registry', has_url_to_registry), &
        & new_unittest('has-both-path-and-url-to-registry', has_both_path_and_url_to_registry, should_fail=.true.) &
        ]

    end subroutine collect_settings

    subroutine delete_tmp_folder
        if (is_dir(tmp_folder)) call os_delete_dir(os_is_unix(), tmp_folder)
    end

    subroutine setup_global_settings(global_settings, error)
        type(fpm_global_settings), intent(out) :: global_settings
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: cwd

        call get_current_directory(cwd, error)
        if (allocated(error)) return

        global_settings%path_to_config_folder = join_path(cwd, tmp_folder)
        global_settings%config_file_name = config_file_name
    end subroutine

    !> Throw error when custom path to config file was entered but no folder exists.
    subroutine no_folder(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call setup_global_settings(global_settings, error)
        if (allocated(error)) return
        call get_global_settings(global_settings, error)
    end subroutine no_folder

    !> Throw error when custom path to config file was entered but no file exists.
    subroutine no_file(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)
    end subroutine no_file

    !> Config file exists and working directory is set.
    subroutine empty_file(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), [''])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)

        call os_delete_dir(os_is_unix(), tmp_folder)

        if (allocated(error)) return

        if (allocated(global_settings%registry_settings)) then
            call test_failed(error, 'global_settings%registry_settings should not be allocated')
            return
        end if
    end subroutine empty_file

    subroutine empty_registry_table(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), ['[registry]'])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)

        call os_delete_dir(os_is_unix(), tmp_folder)

        if (allocated(error)) return

        if (.not. allocated(global_settings%registry_settings)) then
            call test_failed(error, 'global_settings%registry_settings not allocated')
            return
        end if

        if (allocated(global_settings%registry_settings%path)) then
            call test_failed(error, "Path shouldn't be allocated")
            return
        end if

        if (allocated(global_settings%registry_settings%url)) then
            call test_failed(error, "Url shouldn't be allocated")
            return
        end if
    end subroutine

    subroutine wrong_key(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), ['[registry]', 'abcd="abc"']) ! Invalid key

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)
        call os_delete_dir(os_is_unix(), tmp_folder)
    end subroutine

    subroutine wrong_type(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), ['[registry]', 'path=12345']) ! Value not a string

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)
        call os_delete_dir(os_is_unix(), tmp_folder)
    end subroutine

    subroutine has_non_existent_path_to_registry(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), ['[registry]', 'path="abc"'])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)
        call os_delete_dir(os_is_unix(), tmp_folder)
    end subroutine

    subroutine has_existent_path_to_registry(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), &
        & [character(len=10) :: '[registry]', 'path="."'])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)

        call os_delete_dir(os_is_unix(), tmp_folder)

        if (allocated(error)) return

        if (.not. allocated(global_settings%registry_settings%path)) then
            call test_failed(error, 'Path not allocated')
            return
        end if

        if (allocated(global_settings%registry_settings%url)) then
            call test_failed(error, "Url shouldn't be allocated")
            return
        end if

    end subroutine

    subroutine absolute_path_to_registry(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings
        character(len=:), allocatable :: abs_path

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call get_absolute_path(tmp_folder, abs_path, error)

        if (allocated(error)) return

        call filewrite(join_path(tmp_folder, config_file_name), &
                       [character(len=80) :: '[registry]', "path='"//abs_path//"'"])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)

        call os_delete_dir(os_is_unix(), tmp_folder)

        if (allocated(error)) return

        if (.not. allocated(global_settings%registry_settings)) then
            call test_failed(error, 'Registry settings not allocated')
            return
        end if

        if (.not. allocated(global_settings%registry_settings%path)) then
            call test_failed(error, 'Path not allocated')
            return
        end if

        if (global_settings%registry_settings%path /= abs_path) then
            call test_failed(error, "Path not set correctly: '"//global_settings%registry_settings%path//"'")
            return
        end if
    end subroutine

    subroutine relative_path_to_registry(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings
        character(len=:), allocatable :: abs_path

        call delete_tmp_folder
        call mkdir(join_path(tmp_folder, 'abc'))

        call filewrite(join_path(tmp_folder, config_file_name), ['[registry]', 'path="abc"'])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)

        call get_absolute_path(tmp_folder, abs_path, error)

        call os_delete_dir(os_is_unix(), tmp_folder)

        if (allocated(error)) return

        if (.not. allocated(global_settings%registry_settings)) then
            call test_failed(error, 'Registry settings not allocated')
            return
        end if

        if (global_settings%registry_settings%path /= join_path(abs_path, 'abc')) then
            call test_failed(error, "Path not set correctly: '"//global_settings%registry_settings%path//"'")
            return
        end if
    end subroutine

    subroutine canonical_path_to_registry(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings
        character(len=:), allocatable :: abs_path

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), &
        & [character(len=20) :: '[registry]', "path='"//join_path('..', 'tmp')//"'"])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)

        call get_absolute_path(tmp_folder, abs_path, error)

        call os_delete_dir(os_is_unix(), tmp_folder)

        if (allocated(error)) return

        if (.not. allocated(global_settings%registry_settings)) then
            call test_failed(error, 'Registry settings not allocated')
            return
        end if

        if (global_settings%registry_settings%path /= abs_path) then
            call test_failed(error, "Path not set correctly: '"//global_settings%registry_settings%path//"'")
            return
        end if
    end subroutine

    subroutine has_url_to_registry(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), ['[registry]', 'url="http"'])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)

        call os_delete_dir(os_is_unix(), tmp_folder)

        if (.not. allocated(global_settings%registry_settings)) then
            call test_failed(error, 'Registry settings not allocated')
            return
        end if

        if (allocated(global_settings%registry_settings%path)) then
            call test_failed(error, "Path shouldn't be allocated: '" &
            & //global_settings%registry_settings%path//"'")
            return
        end if

        if (.not. allocated(global_settings%registry_settings%url)) then
            call test_failed(error, 'Url not allocated')
            return
        end if
    end subroutine

    subroutine has_both_path_and_url_to_registry(error)
        type(error_t), allocatable, intent(out) :: error
        type(fpm_global_settings) :: global_settings

        call delete_tmp_folder
        call mkdir(tmp_folder)

        call filewrite(join_path(tmp_folder, config_file_name), &
                       [character(len=10) :: '[registry]', 'path="."', 'url="http"'])

        call setup_global_settings(global_settings, error)
        if (allocated(error)) return

        call get_global_settings(global_settings, error)
        call os_delete_dir(os_is_unix(), tmp_folder)
    end subroutine

end module test_settings
