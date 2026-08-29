!> Search packages in both the local cache and the remote registry using the
!> `search` subcommand.
!> Packages can be filtered by package name, namespace, version, query,
!> license, page, limit, and sort order. Use `--registry` to target a custom
!> registry URL.
!>
!> Sample queries:
!>   fpm search --query mpi
!>   fpm search --namespace fortran-lang --package stdlib
!>   fpm search --license MIT --sort-by name --sort asc
!>   fpm search --query hdf5 --page 1 --limit 10 --sort-by downloads --sort desc
module fpm_cmd_search
    use fpm_command_line, only: fpm_search_settings
    use fpm_downloader, only: downloader_t
    use fpm_error, only: error_t, fatal_error, fpm_stop
    use fpm_filesystem, only: basename, dirname, get_temp_filename, list_files
    use fpm_settings, only: fpm_global_settings, get_global_settings
    use fpm_strings, only: glob, string_t
    use fpm_versioning, only: version_t
    use jonquil, only: json_object
    use tomlf, only: get_value, len, toml_array, toml_loads, toml_table
    use tomlf_utils_io, only: read_whole_file

    implicit none
    private
    public :: cmd_search

  contains

    !> Search packages in the registry and in the local cache.
    subroutine cmd_search(settings)
        class(fpm_search_settings), intent(in) :: settings

        type(fpm_global_settings) :: global_settings
        type(error_t), allocatable :: error
        class(downloader_t), allocatable :: downloader
        type(json_object) :: json
        type(version_t), allocatable :: version
        character(len=:), allocatable :: query_url, tmp_file
        integer :: stat, unit

        call get_global_settings(global_settings, error)
        if (allocated(error)) then
            call fpm_stop(1, 'Error retrieving global settings')
            return
        end if

        tmp_file = get_temp_filename()
        open (newunit=unit, file=tmp_file, action='readwrite', iostat=stat)
        if (stat /= 0) then
            call fpm_stop(1, 'Error creating temporary file for downloading package data.')
            return
        end if

        allocate (downloader)
        query_url = build_query_url(settings)
        call downloader%get_pkg_data(query_url, version, tmp_file, json, error)
        close (unit, status='delete')
        if (allocated(error)) then
            call fpm_stop(1, 'Error retrieving package data from registry: '//trim(settings%registry))
            return
        end if

        call print_local_matches(global_settings, settings, error)
        if (allocated(error)) then
            call fpm_stop(1, error%message)
            return
        end if

        call print_remote_matches(json, error)
        if (allocated(error)) then
            call fpm_stop(1, error%message)
        end if
    end subroutine cmd_search

    pure function build_query_url(settings) result(query_url)
        class(fpm_search_settings), intent(in) :: settings
        character(len=:), allocatable :: query_url

        query_url = trim(settings%registry)//'/packages_cli' &
            // '?query='//trim(settings%query) &
            // '&page='//trim(settings%page) &
            // '&package='//trim(settings%package) &
            // '&namespace='//trim(settings%namespace) &
            // '&version='//trim(settings%version) &
            // '&license='//trim(settings%license) &
            // '&limit='//trim(settings%limit) &
            // '&sort_by='//trim(settings%sort_by) &
            // '&sort='//trim(settings%sort)
    end function build_query_url

    subroutine print_local_matches(global_settings, settings, error)
        type(fpm_global_settings), intent(in) :: global_settings
        class(fpm_search_settings), intent(in) :: settings
        type(error_t), allocatable, intent(out) :: error

        type(string_t), allocatable :: file_names(:)
        type(toml_table), allocatable :: table
        character(len=:), allocatable :: file_name, manifest_text, description, package_name
        character(len=:), allocatable :: namespace, version, license
        integer :: i, stat

        if (.not. allocated(global_settings%registry_settings)) then
            call fatal_error(error, 'Registry settings are not available.')
            return
        end if

        if (.not. allocated(global_settings%registry_settings%cache_path)) then
            call fatal_error(error, 'Registry cache path is not available.')
            return
        end if

        call list_files(global_settings%registry_settings%cache_path, file_names, recurse=.true.)

        print *
        print *, 'Searching packages in local registry:'
        print *

        do i = 1, size(file_names)
            file_name = file_names(i)%s
            if (basename(file_name) /= 'fpm.toml') cycle

            namespace = basename(dirname(dirname(dirname(file_name))))
            package_name = basename(dirname(dirname(file_name)))
            version = basename(dirname(file_name))

            if (.not. matches_pattern(namespace, settings%namespace)) cycle
            if (.not. matches_pattern(package_name, settings%package)) cycle
            if (.not. matches_pattern(version, settings%version)) cycle

            call read_whole_file(file_name, manifest_text, stat)
            if (stat /= 0) then
                call fatal_error(error, 'Error reading file: '//file_name)
                return
            end if

            call toml_loads(table, manifest_text)
            if (.not. allocated(table)) then
                call fatal_error(error, 'Error loading TOML file: '//file_name)
                return
            end if

            description = ''
            license = ''
            call get_value(table, 'description', description, stat=stat)
            call get_value(table, 'license', license, stat=stat)

            if (matches_pattern(description, settings%query) .and. matches_pattern(license, settings%license)) then
                call print_package_summary(package_name, namespace, version, description)
            end if
        end do
    end subroutine print_local_matches

    subroutine print_remote_matches(json, error)
        type(json_object), intent(inout) :: json
        type(error_t), allocatable, intent(out) :: error

        type(toml_array), pointer :: packages
        type(json_object), pointer :: package
        character(len=:), allocatable :: name, namespace, description, version
        integer :: i, stat

        if (.not. json%has_key('packages')) then
            call fatal_error(error, 'Invalid package data returned')
            return
        end if

        call get_value(json, 'packages', packages)
        if (.not. associated(packages)) then
            call fatal_error(error, 'Invalid package data returned')
            return
        end if

        print *
        print *, 'Searching packages in remote registry:'
        print *
        print '(A,I0,A)', ' Found ', len(packages), ' packages in fpm - registry:'
        print *

        do i = 1, len(packages)
            call get_value(packages, i, package)
            if (.not. associated(package)) cycle

            name = ''
            namespace = ''
            description = ''
            version = ''
            call get_value(package, 'name', name, stat=stat)
            call get_value(package, 'namespace', namespace, stat=stat)
            call get_value(package, 'description', description, stat=stat)
            call get_value(package, 'version', version, stat=stat)

            call print_package_summary(name, namespace, version, description)
        end do
    end subroutine print_remote_matches

    logical function matches_pattern(value, pattern) result(match)
        character(*), intent(in) :: value
        character(len=:), allocatable, intent(in) :: pattern

        if (.not. allocated(pattern)) then
            match = .true.
        else if (len_trim(pattern) == 0) then
            match = .true.
        else
            match = glob(value, pattern)
        end if
    end function matches_pattern

    subroutine print_package_summary(name, namespace, version, description)
        character(len=*), intent(in) :: name, namespace, version, description

        print *, 'Name: ', name
        print *, 'Namespace: ', namespace
        print *, 'Version: ', version
        print *, 'Description: ', description
        print *
    end subroutine print_package_summary
  end module fpm_cmd_search
  