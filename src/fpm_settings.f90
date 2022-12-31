!> Manages global settings which are defined in the global config file.
module fpm_settings
   use fpm_command_line, only: fpm_global_settings
   use fpm_filesystem, only: exists, join_path, get_local_prefix
   use fpm_environment, only: os_is_unix
   use fpm_error, only: error_t, fatal_error
   implicit none
   private
   public get_global_settings

contains
   !> Obtain global settings from the global config file.
   subroutine get_global_settings(global_settings, error, custom_path_to_config_file)
      !> Global settings to be obtained.
      type(fpm_global_settings), allocatable, intent(out) :: global_settings
      !> Error handling.
      type(error_t), allocatable, intent(out) :: error
      !> Custom path to the config file.
      character(len=*), optional, intent(in) :: custom_path_to_config_file
      !> Final path to the config file.
      character(len=:), allocatable :: path_to_config_file

      ! Use custom path to the config file if it was specified.
      if (present(custom_path_to_config_file)) then
         if (exists(custom_path_to_config_file)) then
            path_to_config_file = custom_path_to_config_file
         else
            ! Throw error if specified path doesn't exist.
            call fatal_error(error, 'No config file at: "'//custom_path_to_config_file//'"')
            return
         end if
      else
         ! Use default paths to the config file if it wasn't specified.
         if (os_is_unix()) then
            path_to_config_file = join_path(get_local_prefix(), 'share', 'fpm', 'config.toml')
         else
            path_to_config_file = join_path(get_local_prefix(), 'fpm', 'config.toml')
         end if
         ! Return quietly (not set the path) if the config file doesn't exist.
         if (.not. exists(path_to_config_file)) return
      end if

      ! Set the path to the global config file.
      allocate (global_settings)
      global_settings%path = path_to_config_file

   end subroutine get_global_settings

end module fpm_settings
