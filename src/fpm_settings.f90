!> Manages global settings which are defined in the global config file.
module fpm_settings
   use fpm_command_line, only: fpm_global_settings
   use fpm_filesystem, only: exists, join_path, get_local_prefix
   use fpm_environment, only: os_is_unix
   use fpm_error, only: error_t, fatal_error
   use fpm_toml, only: toml_table, toml_error, toml_stat, get_value
   use fpm_os, only: get_current_directory, change_directory, get_absolute_path
   use tomlf, only: toml_load
   implicit none
   private
   public :: get_global_settings

contains
   !> Obtain global settings from the global config file.
   subroutine get_global_settings(global_settings, error, custom_path_to_config_file)
      !> Global settings to be obtained.
      type(fpm_global_settings), allocatable, intent(out) :: global_settings
      !> Error reading config file.
      type(error_t), allocatable, intent(out) :: error
      !> Custom path to the config file.
      character(len=*), optional, intent(in) :: custom_path_to_config_file
      !> Final path to the config file.
      character(len=:), allocatable :: path_to_config_file
      !> TOML table to be filled with global config settings.
      type(toml_table), allocatable :: table
      !> Error parsing to TOML table.
      type(toml_error), allocatable :: parse_error

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

      ! Load into TOML table.
      call toml_load(table, path_to_config_file, error=parse_error)

      if (allocated(parse_error)) then
         allocate (error)
         call move_alloc(parse_error%message, error%message)
         return
      end if

      call get_registry_settings(global_settings, table, error)

   end subroutine get_global_settings

   subroutine get_registry_settings(global_settings, table, error)
      type(fpm_global_settings), intent(inout) :: global_settings
      type(toml_table), intent(inout) :: table
      type(error_t), allocatable, intent(out) :: error
      type(toml_table), pointer :: child
      character(:), allocatable :: path, url, abs_path
      integer :: stat

      call get_value(table, 'registry', child, requested=.false., stat=stat)

      if (stat /= toml_stat%success) then
         call fatal_error(error, 'Error reading registry from config file "'// &
                          global_settings%path//'"')
         return
      end if

      ! Quietly return if no registry table was found.
      if (.not. associated(child)) return

      allocate (global_settings%registry_settings)

      call get_value(child, 'path', path, stat=stat)

      if (stat /= toml_stat%success) then
         call fatal_error(error, 'Error parsing path to registry: "'//path//'"')
         return
      end if

      if (allocated(path)) then
         if (.not. exists(path)) then
            call fatal_error(error, "Path to registry doesn't exist:"//'"'//path//'"')
            return
         end if

         ! Making sure that path is absolute
         call get_absolute_path(path, abs_path, error)
         if (allocated(error)) return
         global_settings%registry_settings%path = abs_path
      end if

      call get_value(child, 'url', url, stat=stat)

      if (stat /= toml_stat%success) then
         call fatal_error(error, 'Error parsing url to registry: "'//url//'"')
         return
      end if

      if (allocated(url)) then
         ! Throw error when both path and url were provided.
         if (allocated(path)) then
            call fatal_error(error, 'Do not provide both path and url to registry')
            return
         end if

         global_settings%registry_settings%url = url
      end if

   end subroutine get_registry_settings

end module fpm_settings
