module fpm_cmd_publish
  use fpm_command_line, only: fpm_publish_settings
  use fpm_manifest, only: package_config_t, get_package_data
  use fpm_model, only: fpm_model_t
  use fpm_error, only: error_t, fpm_stop
  use fpm, only: build_model

  implicit none
  private
  public :: cmd_publish

contains

  subroutine cmd_publish(settings)
    type(fpm_publish_settings), intent(in) :: settings

    type(package_config_t) :: package
    type(fpm_model_t) :: model
    type(error_t), allocatable :: error

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    if (allocated(error)) call fpm_stop(1, '*cmd_build* Package error: '//error%message)

    call build_model(model, settings%fpm_build_settings, package, error)
    if (allocated(error)) call fpm_stop(1, '*cmd_build* Model error: '//error%message)

    if (settings%print_request) then
      print *, 'Print JSON ...'
    else
      print *, 'Start publishing ...'
    end if
  end
end
