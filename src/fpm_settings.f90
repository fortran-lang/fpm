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
      !> System-dependent default path to the config file.
      character(len=:), allocatable :: default_path_to_config_file
      !> Final path to the config file.
      character(len=:), allocatable :: path_to_config_file

      ! Use custom path to the config file if it was specified.
      if (present(custom_path_to_config_file)) then
         if (exists(custom_path_to_config_file)) then
            path_to_config_file = custom_path_to_config_file
         else
            ! Throw error if specified path doesn't exist.
            call fatal_error(error, 'No config file at: "'//custom_path_to_config_file//'"')
         end if
      else
         ! Use default path to the config file if it wasn't specified and exists.
         if (os_is_unix()) then
            default_path_to_config_file = join_path(get_local_prefix(), 'share', 'fpm', 'config.toml')
         else
            default_path_to_config_file = join_path(get_local_prefix(), 'fpm', 'config.toml')
         end if
         if (exists(default_path_to_config_file)) then
            path_to_config_file = default_path_to_config_file
         end if
      end if

      ! Set the path to the global config file if it was found.
      if (allocated(path_to_config_file)) then
         allocate (global_settings)
         global_settings%path = path_to_config_file
      end if

   end subroutine get_global_settings

end module fpm_settings
