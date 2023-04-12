module fpm_cmd_publish
  use fpm_command_line, only: fpm_publish_settings
  use fpm_manifest, only: package_config_t, get_package_data
  use fpm_model, only: fpm_model_t
  use fpm_error, only: error_t, fpm_stop
  use fpm, only: build_model
  use fpm_versioning, only: version_t
  use jonquil, only: json_object, json_serialize, set_value

  implicit none
  private
  public :: cmd_publish

contains

  subroutine cmd_publish(settings)
    type(fpm_publish_settings), intent(in) :: settings

    type(package_config_t) :: package
    type(fpm_model_t) :: model
    type(error_t), allocatable :: error
    type(version_t), allocatable :: version
    type(json_object) :: json

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    if (allocated(error)) call fpm_stop(1, '*cmd_build* Package error: '//error%message)

    call build_model(model, settings%fpm_build_settings, package, error)
    if (allocated(error)) call fpm_stop(1, '*cmd_build* Model error: '//error%message)

    ! Determine version of the root package after building the model.
    if (size(model%deps%dep) < 1) call fpm_stop(1, 'Root package not found.')
    version = model%deps%dep(1)%version

    if (settings%show_package_version) then
      print *, version%s(); return
    end if

    json = json_object()

    if (.not. allocated(package%license)) call fpm_stop(1, 'No license specified in fpm.toml.')
    if (.not. allocated(version)) call fpm_stop(1, 'No version specified in fpm.toml.')
    if (version%s() == '0') call fpm_stop(1, 'Invalid version: "'//version%s()//'".')
    if (.not. allocated(settings%source_path)) call fpm_stop(1, 'No source path provided.')
    if (.not. allocated(settings%token)) call fpm_stop(1, 'No token provided.')

    if (settings%show_request) then
      call set_value(json, 'package_name', package%name)
      call set_value(json, 'package_license', package%license)
      call set_value(json, 'package_version', version%s())
      call set_value(json, 'tarball', settings%source_path)
      call set_value(json, 'upload_token', settings%token)
      print *, json_serialize(json)
    else
      print *, 'Start publishing ...'
    end if
  end
end
