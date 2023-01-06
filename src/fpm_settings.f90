!> Manages global settings which are defined in the global config file.
module fpm_settings
   use fpm_command_line, only: fpm_global_settings
   use fpm_filesystem, only: exists, join_path, get_local_prefix, is_absolute_path, canon_path
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
   subroutine get_global_settings(global_settings, error)
      !> Global settings to be obtained.
      type(fpm_global_settings), allocatable, intent(inout) :: global_settings
      !> Error reading config file.
      type(error_t), allocatable, intent(out) :: error
      !> Absolute path to the config file.
      character(len=:), allocatable :: abs_path_to_config
      !> TOML table to be filled with global config settings.
      type(toml_table), allocatable :: table
      !> Error parsing to TOML table.
      type(toml_error), allocatable :: parse_error

      if (.not. allocated(global_settings)) allocate (global_settings)

      ! Use custom path to the config file if it was specified.
      if (allocated(global_settings%path_to_folder) .and. allocated(global_settings%file_name)) then
         ! Throw error if folder doesn't exist.
         if (.not. exists(global_settings%path_to_folder)) then
            call fatal_error(error, 'Folder not found: "'//global_settings%path_to_folder//'"')
            return
         end if

         ! Throw error if file doesn't exist.
         if (.not. exists(global_settings%full_path())) then
            call fatal_error(error, 'File not found: "'//global_settings%full_path()//'"')
            return
         end if

         ! Make sure that the path to the global config file is absolute.
         call get_absolute_path(global_settings%path_to_folder, abs_path_to_config, error)
         if (allocated(error)) return

         global_settings%path_to_folder = abs_path_to_config
      else
         ! Use default path if it wasn't specified.
         if (os_is_unix()) then
            global_settings%path_to_folder = join_path(get_local_prefix(), 'share', 'fpm')
         else
            global_settings%path_to_folder = join_path(get_local_prefix(), 'fpm')
         end if

         ! Use default file name.
         global_settings%file_name = 'config.toml'

         ! Deallocate and return if path doesn't exist.
         if (.not. exists(global_settings%path_to_folder)) then
            deallocate (global_settings%path_to_folder)
            deallocate (global_settings%file_name)
            return
         end if

         ! Deallocate name and return if the config file doesn't exist.
         if (.not. exists(global_settings%full_path())) then
            deallocate (global_settings%file_name)
            return
         end if
      end if

      ! Load into TOML table.
      call toml_load(table, global_settings%full_path(), error=parse_error)

      if (allocated(parse_error)) then
         allocate (error)
         call move_alloc(parse_error%message, error%message)
         return
      end if

      ! Read registry subtable.
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
                          global_settings%full_path()//'"')
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
         if (is_absolute_path(path)) then
            abs_path = path
         else
            abs_path = canon_path(join_path(global_settings%path_to_folder, path))
         end if

         if (.not. exists(abs_path)) then
            call fatal_error(error, "No registry at: '"//abs_path//"'")
            return
         end if

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
            call fatal_error(error, 'Do not provide both path and url to the registry')
            return
         end if

         global_settings%registry_settings%url = url
      end if

   end subroutine get_registry_settings

end module fpm_settings
