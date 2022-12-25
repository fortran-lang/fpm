module test_registry
   use testsuite, only: new_unittest, unittest_t, error_t, test_failed
   use fpm_command_line, only: fpm_registry_settings
   use fpm_registry, only: get_registry
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
      character(len=:), allocatable :: dummy_path

      dummy_path = 'non_existent_path_to/config.toml'

      call get_registry(registry_settings, dummy_path)

      if (allocated(registry_settings)) then
         call test_failed(error, 'registry_settings should not be allocated without a config file')
      end if

   end subroutine no_file

end module test_registry
