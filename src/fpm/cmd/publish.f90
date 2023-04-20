!> Upload a package to the registry using the `publish` command.
!>
!> To upload a package you need to provide a token that will be linked to your username and will be created
!> for a namespace. The token can be obtained from the registry website. It can be used as `fpm publish --token <token>`.
module fpm_cmd_publish
  use fpm_command_line, only: fpm_publish_settings
  use fpm_manifest, only: package_config_t, get_package_data
  use fpm_model, only: fpm_model_t
  use fpm_error, only: error_t, fpm_stop
  use fpm_versioning, only: version_t
  use fpm_filesystem, only: exists, join_path, get_tmp_directory
  use fpm_git, only: git_archive, compressed_package_name
  use fpm_downloader, only: downloader_t
  use fpm_strings, only: string_t
  use fpm_settings, only: official_registry_base_url

  implicit none
  private
  public :: cmd_publish

contains

  !> The `publish` command first builds the model to obtain all the relevant information of the package such as the
  !> package version. It then creates a tarball of the package and uploads it to the registry.
  subroutine cmd_publish(settings)
    type(fpm_publish_settings), intent(inout) :: settings

    type(package_config_t) :: package
    type(fpm_model_t) :: model
    type(error_t), allocatable :: error
    type(version_t), allocatable :: version
    type(string_t), allocatable :: form_data(:)
    character(len=:), allocatable :: tmpdir
    type(downloader_t) :: downloader
    integer :: i

    call get_package_data(package, 'fpm.toml', error, apply_defaults=.true.)
    if (allocated(error)) call fpm_stop(1, '*cmd_build* Package error: '//error%message)

    version = package%version

    if (settings%show_package_version) then
      print *, version%s(); return
    end if

    !> Checks before uploading the package.
    if (.not. allocated(package%license)) call fpm_stop(1, 'No license specified in fpm.toml.')
    if (.not. allocated(version)) call fpm_stop(1, 'No version specified in fpm.toml.')
    if (version%s() == '0') call fpm_stop(1, 'Invalid version: "'//version%s()//'".')
    if (.not. exists('fpm.toml')) call fpm_stop(1, "Cannot find 'fpm.toml' file. Are you in the project root?")

    ! Check if package contains git dependencies. Only publish packages without git dependencies.
    do i = 1, model%deps%ndep
      if (allocated(model%deps%dep(i)%git)) then
        call fpm_stop(1, "Do not publish packages containing git dependencies. '"//model%deps%dep(i)%name//"' is a git dependency.")
      end if
    end do

    form_data = [ &
      string_t('package_name="'//package%name//'"'), &
      string_t('package_license="'//package%license//'"'), &
      string_t('package_version="'//version%s()//'"') &
      & ]

    if (allocated(settings%token)) form_data = [form_data, string_t('upload_token="'//settings%token//'"')]

    call get_tmp_directory(tmpdir, error)
    if (allocated(error)) call fpm_stop(1, '*cmd_publish* Tmp directory error: '//error%message)
    call git_archive('.', tmpdir, error)
    if (allocated(error)) call fpm_stop(1, '*cmd_publish* Pack error: '//error%message)
    form_data = [form_data, string_t('tarball=@"'//join_path(tmpdir, compressed_package_name)//'"')]

    if (settings%show_form_data) then
      do i = 1, size(form_data)
        print *, form_data(i)%s
      end do
      return
    end if

    ! Make sure a token is provided for publishing.
    if (.not. allocated(settings%token)) call fpm_stop(1, 'No token provided.')

    call downloader%upload_form(official_registry_base_url//'/packages', form_data, error)
    if (allocated(error)) call fpm_stop(1, '*cmd_publish* Upload error: '//error%message)
  end
end
