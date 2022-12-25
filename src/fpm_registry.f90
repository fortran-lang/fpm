module fpm_registry
   use fpm_command_line, only: fpm_registry_settings
   use fpm_filesystem, only: exists, join_path, get_local_prefix
   implicit none
   private
   public get_registry

contains
   !> Obtain registry settings and register local or custom registry if such was specified
   !> in the global config file.
   subroutine get_registry(reg_settings)
      type(fpm_registry_settings), allocatable, intent(out) :: reg_settings
      character(len=:), allocatable :: path_to_config_file
      path_to_config_file = join_path(get_local_prefix(), 'fpm', 'config.toml')
      if (exists(path_to_config_file)) then
         reg_settings%working_dir = path_to_config_file
      end if
   end subroutine get_registry

end module fpm_registry
