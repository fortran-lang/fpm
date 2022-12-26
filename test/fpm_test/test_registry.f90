module test_registry
   use testsuite, only: new_unittest, unittest_t, error_t, test_failed
   use fpm_command_line, only: fpm_registry_settings
   use fpm_registry, only: get_registry
   use fpm_filesystem, only: is_dir, join_path

   implicit none
   private
   public collect_registry

contains

   !> Collect unit tests.
   subroutine collect_registry(tests)

      !> Unit tests to collect.
      type(unittest_t), allocatable, intent(out) :: tests(:)

      tests = [ &
      & new_unittest('no-file', no_file) &
      ]

   end subroutine collect_registry

   subroutine no_file(error)

      type(error_t), allocatable, intent(out) :: error
      type(fpm_registry_settings), allocatable :: registry_settings
      character(len=*), parameter :: dummy_folder = 'dummy_folder'

      if (is_dir(dummy_folder)) then
         call test_failed(error, dummy_folder//' should not exist before test')
      end if

      call get_registry(registry_settings, join_path(dummy_folder, 'config.toml'))

      if (allocated(registry_settings)) then
         call test_failed(error, 'registry_settings should not be allocated without a config file')
      end if

   end subroutine no_file

end module test_registry
