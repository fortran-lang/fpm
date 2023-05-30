!> Upload a package to the registry using the `publish` command.
!>
!> To upload a package you need to provide a token that will be linked to your username and created for a namespace.
!> The token can be obtained from the registry website. It can be used as `fpm publish --token <token>`.
module fpm_cmd_publish
  use fpm_command_line, only: fpm_publish_settings
  use fpm_manifest, only: package_config_t, get_package_data
  use fpm_model, only: fpm_model_t
  use fpm_error, only: error_t, fpm_stop
  use fpm_versioning, only: version_t
  use fpm_filesystem, only: exists, join_path, get_temp_filename, delete_file
  use fpm_git, only: git_archive
  use fpm_downloader, only: downloader_t
  use fpm_strings, only: string_t
  use fpm_settings, only: official_registry_base_url
  use fpm, only: build_model

  implicit none
  private
  public :: cmd_publish

contains

  !> The `publish` command first builds the root package to obtain all the relevant information such as the
  !> package version. It then creates a tarball of the package and uploads it to the registry.
  subroutine cmd_publish(settings)
    type(fpm_publish_settings), intent(inout) :: settings

    type(package_config_t) :: package
    type(fpm_model_t) :: model
    type(error_t), allocatable :: error
    type(version_t), allocatable :: version
    type(string_t), allocatable :: upload_data(:)
    character(len=:), allocatable :: tmp_file
    type(downloader_t) :: downloader
    integer :: i

    ! Get package data to determine package version.
    call get_package_data(package, 'fpm.toml', error, apply_defaults=.true.)
    if (allocated(error)) call fpm_stop(1, '*cmd_build* Package error: '//error%message)
    version = package%version

    if (settings%show_package_version) then
      print *, version%s(); return
    end if

    !> Checks before uploading the package.
    if (.not. allocated(package%license)) call fpm_stop(1, 'No license specified in fpm.toml.')
    if (.not. package%build%module_naming) call fpm_stop(1, 'The package does not meet the module naming requirements. '// &
      & 'Please set "module_naming = true" in fpm.toml [build] or specify a custom module prefix.')
    if (.not. allocated(version)) call fpm_stop(1, 'No version specified in fpm.toml.')
    if (version%s() == '0') call fpm_stop(1, 'Invalid version: "'//version%s()//'".')
    if (.not. exists('fpm.toml')) call fpm_stop(1, "Cannot find 'fpm.toml' file. Are you in the project root?")

    ! Build model to obtain dependency tree.
    call build_model(model, settings%fpm_build_settings, package, error)
    if (allocated(error)) call fpm_stop(1, '*cmd_build* Model error: '//error%message)

    ! Check if package contains git dependencies. Only publish packages without git dependencies.
    do i = 1, model%deps%ndep
      if (allocated(model%deps%dep(i)%git)) then
        call fpm_stop(1, 'Do not publish packages containing git dependencies. '// &
        & "Please upload '"//model%deps%dep(i)%name//"' to the registry first.")
      end if
    end do

    tmp_file = get_temp_filename()
    call git_archive('.', tmp_file, error)
    if (allocated(error)) call fpm_stop(1, '*cmd_publish* Archive error: '//error%message)

    upload_data = [ &
    & string_t('package_name="'//package%name//'"'), &
    & string_t('package_license="'//package%license//'"'), &
    & string_t('package_version="'//version%s()//'"'), &
    & string_t('tarball=@"'//tmp_file//'"') &
    & ]

    if (allocated(settings%token)) upload_data = [upload_data, string_t('upload_token="'//settings%token//'"')]

    if (settings%show_upload_data) then
      call print_upload_data(upload_data); return
    end if

    ! Make sure a token is provided for publishing.
    if (allocated(settings%token)) then
      if (settings%token == '') then
        call delete_file(tmp_file); call fpm_stop(1, 'No token provided.')
      end if
    else
      call delete_file(tmp_file); call fpm_stop(1, 'No token provided.')
    end if

    if (settings%verbose) then
      print *, ''
      call print_upload_data(upload_data)
      print *, ''
    end if

    ! Perform network request and validate package, token etc. on the backend once
    ! https://github.com/fortran-lang/registry/issues/41 is resolved.
    if (settings%is_dry_run) then
      print *, 'Dry run successful.'
      print *, ''
      print *, 'tarball generated for upload: ', tmp_file
      return
    end if

    call downloader%upload_form(official_registry_base_url//'/packages', upload_data, error)
    call delete_file(tmp_file)
    if (allocated(error)) call fpm_stop(1, '*cmd_publish* Upload error: '//error%message)
  end

  subroutine print_upload_data(upload_data)
    type(string_t), intent(in) :: upload_data(:)
    integer :: i

    print *, 'Upload data:'
    do i = 1, size(upload_data)
      print *, upload_data(i)%s
    end do
  end
end
