module test_settings
   use testsuite, only: new_unittest, unittest_t, error_t, test_failed
   use fpm_command_line, only: fpm_global_settings
   use fpm_settings, only: get_global_settings
   use fpm_filesystem, only: is_dir, join_path, mkdir, filewrite, os_delete_dir, exists
   use fpm_environment, only: os_is_unix

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
      & new_unittest('empty-file', empty_file) &
      ]

   end subroutine collect_settings

   !> Throw error when custom path to config file was entered but none exists.
   subroutine no_file(error)

      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings

      if (is_dir(tmp_folder)) call os_delete_dir(os_is_unix(), tmp_folder)

      call get_global_settings(global_settings, error, join_path(tmp_folder, config_file_name))

   end subroutine no_file

   !> Config file exists and working directory is set.
   subroutine empty_file(error)

      type(error_t), allocatable, intent(out) :: error
      type(fpm_global_settings), allocatable :: global_settings
      character(len=:), allocatable :: path_to_config_file

      if (is_dir(tmp_folder)) call os_delete_dir(os_is_unix(), tmp_folder)

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

   end subroutine empty_file

end module test_settings
