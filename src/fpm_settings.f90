!> Manages global settings which are defined in the global config file.
module fpm_settings
  use fpm_filesystem, only: exists, join_path, get_local_prefix, is_absolute_path, mkdir
  use fpm_environment, only: os_is_unix
  use fpm_error, only: error_t, fatal_error
  use fpm_toml, only: toml_table, toml_error, toml_stat, get_value, toml_load, check_keys
  use fpm_os, only: get_current_directory, change_directory, get_absolute_path, &
                    convert_to_absolute_path
  implicit none
  private
  public :: fpm_global_settings, get_global_settings, get_registry_settings, official_registry_base_url

  character(*), parameter :: official_registry_base_url = 'https://registry-apis.vercel.app'

  type :: fpm_global_settings
    !> Path to the global config file excluding the file name.
    character(len=:), allocatable :: path_to_config_folder
    !> Name of the global config file. The default is `config.toml`.
    character(len=:), allocatable :: config_file_name
    !> Registry configs.
    type(fpm_registry_settings), allocatable :: registry_settings
  contains
    procedure :: has_custom_location, full_path
  end type

  type :: fpm_registry_settings
    !> The path to the local registry. If allocated, the local registry
    !> will be used instead of the remote registry and replaces the
    !> local cache.
    character(len=:), allocatable :: path
    !> The URL to the remote registry. Can be used to get packages
    !> from the official or a custom registry.
    character(len=:), allocatable :: url
    !> The path to the cache folder. If not specified, the default cache
    !> folders are `~/.local/share/fpm/dependencies` on Unix and
    !> `%APPDATA%\local\fpm\dependencies` on Windows.
    !> Cannot be used together with `path`.
    character(len=:), allocatable :: cache_path
  end type

contains
  !> Obtain global settings from the global config file.
  subroutine get_global_settings(global_settings, error)
    !> Global settings to be obtained.
    type(fpm_global_settings), intent(inout) :: global_settings
    !> Error reading config file.
    type(error_t), allocatable, intent(out) :: error
    !> TOML table to be filled with global config settings.
    type(toml_table), allocatable :: table
    !> Error parsing to TOML table.
    type(toml_error), allocatable :: parse_error

    type(toml_table), pointer :: registry_table
    integer :: stat

    ! Use custom path to the config file if it was specified.
    if (global_settings%has_custom_location()) then
      ! Throw error if folder doesn't exist.
      if (.not. exists(config_path(global_settings))) then
        call fatal_error(error, "Folder not found: '"//config_path(global_settings)//"'."); return
      end if

      ! Throw error if the file doesn't exist.
      if (.not. exists(global_settings%full_path())) then
        call fatal_error(error, "File not found: '"//global_settings%full_path()//"'."); return
      end if

      ! Make sure that the path to the global config file is absolute.
      call convert_to_absolute_path(global_settings%path_to_config_folder, error)
      if (allocated(error)) return
    else
      ! Use default path if it wasn't specified.
      if (os_is_unix()) then
        global_settings%path_to_config_folder = join_path(get_local_prefix(), 'share', 'fpm')
      else
        global_settings%path_to_config_folder = join_path(get_local_prefix(), 'fpm')
      end if

      ! Use default file name.
      global_settings%config_file_name = 'config.toml'

      ! Apply default registry settings and return if config file doesn't exist.
      if (.not. exists(global_settings%full_path())) then
        call use_default_registry_settings(global_settings); return
      end if
    end if

    ! Load into TOML table.
    call toml_load(table, global_settings%full_path(), error=parse_error)

    if (allocated(parse_error)) then
      allocate (error); call move_alloc(parse_error%message, error%message); return
    end if

    call get_value(table, 'registry', registry_table, requested=.false., stat=stat)

    if (stat /= toml_stat%success) then
      call fatal_error(error, "Error reading registry from config file '"// &
      & global_settings%full_path()//"'."); return
    end if

    ! A registry table was found.
    if (associated(registry_table)) then
      call get_registry_settings(registry_table, global_settings, error)
    else
      call use_default_registry_settings(global_settings)
    end if

  end subroutine get_global_settings

  !> Default registry settings are typically applied if the config file doesn't exist or no registry table was found in
  !> the global config file.
  subroutine use_default_registry_settings(global_settings)
    type(fpm_global_settings), intent(inout) :: global_settings

    allocate (global_settings%registry_settings)
    global_settings%registry_settings%url = official_registry_base_url
    global_settings%registry_settings%cache_path = join_path(config_path(global_settings), &
    & 'dependencies')
  end subroutine use_default_registry_settings

  !> Read registry settings from the global config file.
  subroutine get_registry_settings(table, global_settings, error)
    !> The [registry] subtable from the global config file.
    type(toml_table), target, intent(inout) :: table
    !> The global settings which can be filled with the registry settings.
    type(fpm_global_settings), intent(inout) :: global_settings
    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: path, url, cache_path
    integer :: stat

    !> List of valid keys for the dependency table.
    character(*), dimension(*), parameter :: valid_keys = [character(10) :: &
        & 'path', &
        & 'url', &
        & 'cache_path' &
        & ]

    call check_keys(table, valid_keys, error)
    if (allocated(error)) return

    allocate (global_settings%registry_settings)

    if (table%has_key('path')) then
      call get_value(table, 'path', path, stat=stat)
      if (stat /= toml_stat%success) then
        call fatal_error(error, "Error reading registry path: '"//path//"'."); return
      end if
    end if

    if (allocated(path)) then
      if (is_absolute_path(path)) then
        global_settings%registry_settings%path = path
      else
        ! Get canonical, absolute path on both Unix and Windows.
        call get_absolute_path(join_path(config_path(global_settings), path), &
        & global_settings%registry_settings%path, error)
        if (allocated(error)) return

        ! Check if the path to the registry exists.
        if (.not. exists(global_settings%registry_settings%path)) then
          call fatal_error(error, "Directory '"//global_settings%registry_settings%path// &
          & "' doesn't exist."); return
        end if
      end if
    end if

    if (table%has_key('url')) then
      call get_value(table, 'url', url, stat=stat)
      if (stat /= toml_stat%success) then
        call fatal_error(error, "Error reading registry url: '"//url//"'."); return
      end if
    end if

    if (allocated(url)) then
      ! Throw error when both path and url were provided.
      if (allocated(path)) then
        call fatal_error(error, 'Do not provide both path and url to the registry.'); return
      end if
      global_settings%registry_settings%url = url
    else if (.not. allocated(path)) then
      global_settings%registry_settings%url = official_registry_base_url
    end if

    if (table%has_key('cache_path')) then
      call get_value(table, 'cache_path', cache_path, stat=stat)
      if (stat /= toml_stat%success) then
        call fatal_error(error, "Error reading path to registry cache: '"//cache_path//"'."); return
      end if
    end if

    if (allocated(cache_path)) then
      ! Throw error when both path and cache_path were provided.
      if (allocated(path)) then
        call fatal_error(error, "Do not provide both 'path' and 'cache_path'."); return
      end if

      if (is_absolute_path(cache_path)) then
        if (.not. exists(cache_path)) call mkdir(cache_path)
        global_settings%registry_settings%cache_path = cache_path
      else
        cache_path = join_path(config_path(global_settings), cache_path)
        if (.not. exists(cache_path)) call mkdir(cache_path)
        ! Get canonical, absolute path on both Unix and Windows.
        call get_absolute_path(cache_path, global_settings%registry_settings%cache_path, error)
        if (allocated(error)) return
      end if
    else if (.not. allocated(path)) then
       global_settings%registry_settings%cache_path = join_path(config_path(global_settings), &
    &    'dependencies')
    end if
  end subroutine get_registry_settings

  !> True if the global config file is not at the default location.
  pure logical function has_custom_location(self)
    class(fpm_global_settings), intent(in) :: self

    has_custom_location = allocated(self%path_to_config_folder) .and. allocated(self%config_file_name)
  end function

  !> The full path to the global config file.
  function full_path(self) result(result)
    class(fpm_global_settings), intent(in) :: self
    character(len=:), allocatable :: result

    result = join_path(config_path(self), self%config_file_name)
  end function

  !> The path to the global config directory.
  function config_path(self)
    class(fpm_global_settings), intent(in) :: self
    character(len=:), allocatable :: config_path

    if (allocated(self%path_to_config_folder)) then
        config_path = self%path_to_config_folder
    else
        config_path = ""
    end if
  end function config_path

end module fpm_settings
