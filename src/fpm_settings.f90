!> Manages global settings which are defined in the global config file.
module fpm_settings
    use fpm_filesystem, only: exists, join_path, get_local_prefix, is_absolute_path, mkdir
    use fpm_environment, only: os_is_unix
    use fpm_error, only: error_t, fatal_error
    use fpm_toml, only: toml_table, toml_error, toml_stat, get_value, toml_load
    use fpm_os, only: get_current_directory, change_directory, get_absolute_path, &
                      convert_to_absolute_path
    implicit none
    private
    public :: fpm_global_settings, get_global_settings

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
    contains
        procedure :: uses_default_registry
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

        ! Use custom path to the config file if it was specified.
        if (global_settings%has_custom_location()) then
            ! Throw error if folder doesn't exist.
            if (.not. exists(global_settings%path_to_config_folder)) then
                call fatal_error(error, 'Folder not found: "'//global_settings%path_to_config_folder//'"')
                return
            end if

            ! Throw error if file doesn't exist.
            if (.not. exists(global_settings%full_path())) then
                call fatal_error(error, 'File not found: "'//global_settings%full_path()//'"')
                return
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

            ! Return if path or file doesn't exist.
            if (.not. exists(global_settings%path_to_config_folder) &
                .or. .not. exists(global_settings%full_path())) return

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

    !> Get settings from the [registry] table in the global config file.
    subroutine get_registry_settings(global_settings, table, error)
        type(fpm_global_settings), intent(inout) :: global_settings
        type(toml_table), intent(inout) :: table
        type(error_t), allocatable, intent(out) :: error
        type(toml_table), pointer :: child
        character(:), allocatable :: path, url, cache_path
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
            call fatal_error(error, 'Error reading registry path: "'//path//'"')
            return
        end if

        if (allocated(path)) then
            if (is_absolute_path(path)) then
                global_settings%registry_settings%path = path
            else
                ! Get canonical, absolute path on both Unix and Windows.
                call get_absolute_path(join_path(global_settings%path_to_config_folder, path), &
                                       global_settings%registry_settings%path, error)
                if (allocated(error)) return

                ! Check if the path to the registry exists.
                if (.not. exists(global_settings%registry_settings%path)) then
                    call fatal_error(error, "Directory '"//global_settings%registry_settings%path// &
                                     "' does not exist")
                    return
                end if
            end if
        end if

        call get_value(child, 'url', url, stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error, 'Error reading registry url: "'//url//'"')
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

        call get_value(child, 'cache_path', cache_path, stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error, 'Error reading path to registry cache: "'//cache_path//'"')
            return
        end if

        if (allocated(cache_path)) then
            ! Throw error when both path and cache_path were provided.
            if (allocated(path)) then
                call fatal_error(error, "Do not provide both 'path' and 'cache_path'")
                return
            end if

            if (is_absolute_path(cache_path)) then
                if (.not. exists(cache_path)) call mkdir(cache_path)
                global_settings%registry_settings%cache_path = cache_path
            else
                if (.not. exists(join_path(global_settings%path_to_config_folder, cache_path))) then
                    call mkdir(join_path(global_settings%path_to_config_folder, cache_path))
                end if
                ! Get canonical, absolute path on both Unix and Windows.
                call get_absolute_path(join_path(global_settings%path_to_config_folder, cache_path), &
                                       global_settings%registry_settings%cache_path, error)
                if (allocated(error)) return
            end if
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

        result = join_path(self%path_to_config_folder, self%config_file_name)
    end function

    !> The official registry is used by default when no local or custom registry was specified.
    pure logical function uses_default_registry(self)
        class(fpm_registry_settings), intent(in) :: self

        uses_default_registry = .not. allocated(self%path) .and. .not. allocated(self%url)
    end function

end module fpm_settings
