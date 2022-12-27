module fpm_registry
   use fpm_command_line, only: fpm_registry_settings
   use fpm_filesystem, only: exists, join_path, get_local_prefix, parent_dir
   use fpm_error, only: error_t, fatal_error
   implicit none
   private
   public get_registry_settings

contains
   !> Obtain registry settings and register local or custom registry if such was specified
   !> in the global config file.
   subroutine get_registry_settings(reg_settings, error, custom_path_to_config_file)
      !> Registry settings to be obtained.
      type(fpm_registry_settings), allocatable, intent(out) :: reg_settings
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
            call fatal_error(error, 'No config file at: "'//custom_path_to_config_file//'"')
         end if
      else
         ! Use default path to the config file if it wasn't manually set and exists.
         default_path_to_config_file = join_path(get_local_prefix(), 'share', 'fpm', 'config.toml')
         if (exists(default_path_to_config_file)) then
            path_to_config_file = default_path_to_config_file
         end if
      end if

      ! Obtain registry settings from config file if it was found.
      if (allocated(path_to_config_file)) then
         allocate (reg_settings)
         reg_settings%working_dir = parent_dir(path_to_config_file)
      end if

   end subroutine get_registry_settings

end module fpm_registry
