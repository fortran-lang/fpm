module test_settings
   use testsuite, only: new_unittest, unittest_t, error_t, test_failed
   use fpm_command_line, only: fpm_global_settings
   use fpm_settings, only: get_global_settings
   use fpm_filesystem, only: is_dir, join_path, mkdir, filewrite, os_delete_dir, exists
   use fpm_environment, only: os_is_unix
   use fpm_toml, only: new_table

   implicit none
   private
   public collect_settings

   character(len=*), parameter :: tmp_folder = 'tmp'
   character(len=*), parameter :: config_file_name = 'config.toml'

contains

   !> Collect unit tests.
   subroutine collect_settings(tests)

      !> Unit tests to collect.
      type(unittest_t), allocatable, intent(out) :: tests(:)

      tests = [ &
      & new_unittest('no-file', no_file, should_fail=.true.), &
      & new_unittest('empty-file', empty_file), &
      & new_unittest('empty-registry-table', empty_registry_table), &
      & new_unittest('has-non-existent-path-registry', has_non_existent_path_registry, should_fail=.true.), &
      & new_unittest('has-existent-path-registry', has_existent_path_registry), &
      & new_unittest('has-url-registry', has_url_registry), &
      & new_unittest('has-both-path-and-url-registry', has_both_path_and_url_registry, should_fail=.true.) &
      ]

   end subroutine collect_settings

   subroutine delete_tmp_folder
      if (is_dir(tmp_folder)) call os_delete_dir(os_is_unix(), tmp_folder)
   end

   !> Throw error when custom path to config file was entered but none exists.
   subroutine no_file(error)

      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings

      call delete_tmp_folder()
      call get_global_settings(global_settings, error, join_path(tmp_folder, config_file_name))

   end subroutine no_file

   !> Config file exists and working directory is set.
   subroutine empty_file(error)

      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings
      character(len=:), allocatable :: path_to_config_file

      call delete_tmp_folder()
      call mkdir(tmp_folder)

      path_to_config_file = join_path(tmp_folder, config_file_name)
      call filewrite(path_to_config_file, [''])

      call get_global_settings(global_settings, error, path_to_config_file)

      call os_delete_dir(os_is_unix(), tmp_folder)

      if (.not. allocated(global_settings)) then
         call test_failed(error, 'global_settings not allocated')
         return
      end if

      if (.not. allocated(global_settings%path)) then
         call test_failed(error, 'global_settings%path not allocated')
         return
      end if

      if (allocated(global_settings%registry_settings)) then
         call test_failed(error, 'global_settings%registry_settings should not be allocated')
         return
      end if

   end subroutine empty_file

   subroutine empty_registry_table(error)
      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings
      character(len=:), allocatable :: path_to_config_file

      call delete_tmp_folder()
      call mkdir(tmp_folder)

      path_to_config_file = join_path(tmp_folder, config_file_name)
      call filewrite(path_to_config_file, ['[registry]'])

      call get_global_settings(global_settings, error, path_to_config_file)

      call os_delete_dir(os_is_unix(), tmp_folder)

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

   subroutine has_non_existent_path_registry(error)
      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings
      character(len=:), allocatable :: path_to_config_file

      call delete_tmp_folder()
      call mkdir(tmp_folder)

      path_to_config_file = join_path(tmp_folder, config_file_name)
      call filewrite(path_to_config_file, ['[registry]', 'path="abc"'])
      call get_global_settings(global_settings, error, path_to_config_file)
      call os_delete_dir(os_is_unix(), tmp_folder)
   end subroutine

   subroutine has_existent_path_registry(error)
      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings
      character(len=:), allocatable :: path_to_config_file

      call delete_tmp_folder()
      call mkdir(tmp_folder)

      path_to_config_file = join_path(tmp_folder, config_file_name)
      call filewrite(path_to_config_file, ['[registry]', 'path="tmp"'])

      call get_global_settings(global_settings, error, path_to_config_file)

      call os_delete_dir(os_is_unix(), tmp_folder)

      if (.not. allocated(global_settings%registry_settings%path)) then
         call test_failed(error, 'Path not allocated')
         return
      end if

      if (allocated(global_settings%registry_settings%url)) then
         call test_failed(error, "Url shouldn't be allocated")
         return
      end if
   end subroutine

   subroutine has_url_registry(error)
      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings
      character(len=:), allocatable :: path_to_config_file

      call delete_tmp_folder()
      call mkdir(tmp_folder)

      path_to_config_file = join_path(tmp_folder, config_file_name)
      call filewrite(path_to_config_file, ['[registry]', 'url="http"'])

      call get_global_settings(global_settings, error, path_to_config_file)

      call os_delete_dir(os_is_unix(), tmp_folder)

      if (allocated(global_settings%registry_settings%path)) then
         call test_failed(error, "Path shouldn't be allocated")
         return
      end if

      if (.not. allocated(global_settings%registry_settings%url)) then
         call test_failed(error, "Url not allocated")
         return
      end if
   end subroutine

   subroutine has_both_path_and_url_registry(error)
      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings
      character(len=:), allocatable :: path_to_config_file

      call delete_tmp_folder()
      call mkdir(tmp_folder)

      path_to_config_file = join_path(tmp_folder, config_file_name)
      call filewrite(path_to_config_file, ['[registry]', 'path="tmp"', 'url="http"'])
      call get_global_settings(global_settings, error, path_to_config_file)
      call os_delete_dir(os_is_unix(), tmp_folder)
   end subroutine

end module test_settings
