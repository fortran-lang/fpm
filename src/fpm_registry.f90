module fpm_registry
   use fpm_command_line, only: fpm_registry_settings
   use fpm_filesystem, only: exists, join_path, get_local_prefix
   implicit none
   private
   public get_registry

contains
   !> Obtain registry settings and register local or custom registry if such was specified
   !> in the global config file.
   subroutine get_registry(reg_settings, path_to_config_file)
      !> Registry settings to be obtained.
      type(fpm_registry_settings), allocatable, intent(out) :: reg_settings
      !> Custom path to the config file.
      character(len=*), optional, intent(in) :: path_to_config_file
      !> System-dependent default path to the config file.
      character(len=:), allocatable :: default_path_to_config_file

      if (present(path_to_config_file)) then
         if (exists(path_to_config_file)) then
            reg_settings%working_dir = path_to_config_file
         end if
      end if

      default_path_to_config_file = join_path(get_local_prefix(), 'fpm', 'config.toml')
      if (exists(default_path_to_config_file)) then
         reg_settings%working_dir = default_path_to_config_file
      end if
   end subroutine get_registry

end module fpm_registry
