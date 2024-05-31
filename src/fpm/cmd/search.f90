!> Search a package from both local and remote registry using the `search` command.
!>
!> The package can be searched by packagename, namespace, query (description and README.md), and license from the registries (local and remote).
!> the remote registry URL can also be specified by the paramter --registry.
!> It can be used as `fpm search --query fortran --page 2 --name fortran --namespace fortran --license MIT --registry URL`.
module fpm_cmd_search
    use fpm_command_line, only: fpm_search_settings
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_model, only: fpm_model_t
    use fpm_error, only: error_t, fpm_stop
    use fpm_versioning, only: version_t
    use fpm_filesystem, only: exists, join_path, get_temp_filename, delete_file, basename, &
                            canon_path, dirname, list_files, is_hidden_file
    use fpm_git, only: git_archive
    use fpm_downloader, only: downloader_t
    use fpm_strings, only: string_t, string_array_contains, split, str,glob
    use fpm, only: build_model
    use fpm_error, only : error_t, fatal_error, fpm_stop
    use jonquil, only : json_object
    use tomlf, only : toml_array, get_value, len, toml_key
    use fpm_settings, only: fpm_global_settings, get_global_settings, official_registry_base_url
  
    implicit none
    private
    public :: cmd_search
  
  contains
  
    !> Search the fpm registry for a package
    subroutine cmd_search(settings)
        !> Settings for the search command.
        class(fpm_search_settings), intent(in) :: settings
        type(fpm_global_settings) :: global_settings
        character(:), allocatable :: tmp_file, name, namespace, description, query_url
        type(toml_key), allocatable :: list(:)
        integer :: stat, unit, ii
        type(json_object) :: json
        type(json_object), pointer :: p
        !> Error handling.
        type(error_t), allocatable :: error
        type(toml_array), pointer :: array
        type(version_t), allocatable :: version

        !> Downloader instance.
        class(downloader_t), allocatable :: downloader
        allocate (downloader)

        call get_global_settings(global_settings, error)
        if (allocated(error)) then
            call fpm_stop(1, "Error retrieving global settings"); return
        end if

        ! print *,global_settings%registry_settings%cache_path
        ! print *,global_settings%registry_settings%path
        ! print *,global_settings%registry_settings%url
        ! print *,global_settings%path_to_config_folder
        
        ! new function to search package names, namespace.
        ! query general term for description (both toml and readme)
        ! search by namespace, package, license
        ! search by namespace , package.
        ! from fpm.toml -> description and license
        ! README.md -> description

        !> Generate a temporary file to store the downloaded package search data
        tmp_file = get_temp_filename()
        open (newunit=unit, file=tmp_file, action='readwrite', iostat=stat)
        if (stat /= 0) then
        call fatal_error(error, "Error creating temporary file for downloading package."); return
        end if
        query_url = settings%registry//'/packages_cli' &
                    & // '?query='//settings%query &
                    & // '&page='//settings%page &
                    & // '&package='//settings%package &
                    & // '&namespace='//settings%namespace &
                    & // '&version='//settings%version &
                    & // '&license='//settings%license &
                    & // '&limit='//settings%limit &
                    & // '&sort_by='//settings%sort_by &
                    & // '&sort='//settings%sort
        
        !> search parameters: name, namespace, version, license, query -> (description1, description2)
        !> namespace,version,package from url. 
        !> description1 from fpm.toml and description2 from README.md
        !> name, license, version, description1 from fpm.toml
        !> description2 from README.md (if exists)
        !> order manipulation parameters: page, sort, sort_by, limit
        !> show page number and total_pages

        !> Get the package data from the registry
        ! print *, settings%namespace
        call downloader%get_pkg_data(query_url, version, tmp_file, json, error)
        close (unit)
        if (allocated(error)) then
            call fpm_stop(1, "Error retrieving package data from registry: "//settings%registry); return
        end if
        ! print *, settings%version

        call search_package(settings%namespace, settings%package, settings%version)
        if (json%has_key("packages")) then
            !> Better method to display the package data
            ! call get_value(json, 'packages', array)
            ! print '(A,I0,A)', ' Found ', len(array), ' packages:'
            ! do ii=1, len(array)
            !     call get_value(array, ii, p)
            !     call get_value(p, 'name', name)
            !     call get_value(p, 'namespace', namespace)
            !     call get_value(p, 'description', description)
            !     print *, "Name: ", name
            !     print *, "namespace: ", namespace
            !     print *, "Description: ", description
            !     print *, ""
            ! end do
        else 
            call fpm_stop(1, "Invalid package data returned"); return
        end if
        ! print *, "Searching in local registry is not implemented yet for all parameters."
    end subroutine cmd_search

    subroutine search_package(namespace,package,version)
        type(fpm_global_settings)             :: global_settings
        type(error_t), allocatable            :: error
        character(:), allocatable, intent(in) :: namespace,package,version
        character(:), allocatable             :: path,array(:)
        character(:), allocatable             :: wild
        type(string_t), allocatable           :: file_names(:)
        integer :: i,j
        logical :: result
        

        call get_global_settings(global_settings, error)
        if (allocated(error)) then
            call fpm_stop(1, "Error retrieving global settings"); return
        end if

        ! print *,global_settings%registry_settings%cache_path
        path = global_settings%registry_settings%cache_path

        if (namespace /= "") then
            wild = path//"/"//namespace
        else 
            wild = path//"/*"
        end if
        if (package /= "") then
            wild = wild//"/"//package
        else 
            wild = wild//"/*"
        end if
        if (version /= "") then
            wild = wild//"/"//version
        else 
            wild = wild//"/?.?.?"
        end if
        wild = wild//"/fpm.toml"
        ! print *, "Path: ", wild

        ! Scan directory for sources
        call list_files(path, file_names,recurse=.true.)
        do i=1,size(file_names)
            if (.not.is_hidden_file(file_names(i)%s)) then
                call split(file_names(i)%s,array,'/')
                if (array(size(array)) == "fpm.toml") then
                    result = glob(file_names(i)%s,wild)
                    if (result) then
                        ! print *, "Matched results" !> add count
                        print *, "Package: ", array(size(array)-3), array(size(array)-2), array(size(array)-1)
                    end if
                    !> read the toml file from the path file_names(i)%s for data and path
                end if
                ! print *, "Add as Dependency: "
                ! print *, array(size(array)), " = { namespace = '", namespace, "' }"
            end if
        end do
    end subroutine search_package
  end
  