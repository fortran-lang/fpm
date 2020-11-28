!> Implementation of the fpm-update command.
module fpm_cmd_update
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm_command_line, only : fpm_update_settings
  use fpm_constants, only : fpm_manifest_file, fpm_build_dir, fpm_dependency_dir
  use fpm_error, only : error_t, fatal_error
  use fpm_filesystem, only : join_path, exists, mkdir
  use fpm_git, only : git_target_t
  use fpm_manifest, only : get_package_data, package_config_t, &
    executable_config_t, dependency_config_t
  use fpm_toml, only : toml_table, toml_parse, toml_serializer, toml_error, &
    toml_key, add_table, set_value, get_value
  use fpm_dependency, only : update_dep_lock, dependency_walker_t, &
    new_dependency_walker
  implicit none
  private
  public :: cmd_update


contains

  !> Entry point for the fpm-update command
  subroutine cmd_update(settings)
    !> Representation of the command line options for this command
    type(fpm_update_settings), intent(in) :: settings

    type(package_config_t) :: package
    type(dependency_walker_t) :: config
    type(error_t), allocatable :: error
    integer :: ii

    call get_package_data(package, fpm_manifest_file, error, apply_defaults=.true.)
    call handle_error(error)

    if (.not.exists(fpm_build_dir)) then
      call mkdir(fpm_build_dir)
    end if

    if (settings%fetch_only) then
      config = new_dependency_walker(&
          prefix=join_path(fpm_build_dir, fpm_dependency_dir), &
          verbosity=merge(2, 1, settings%verbose))
    else
      config = new_dependency_walker(&
          prefix=join_path(fpm_build_dir, fpm_dependency_dir), &
          update=settings%name, &
          update_all=size(settings%name) == 0, &
          verbosity=merge(2, 1, settings%verbose))
    end if

    call update_dep_lock(config, package, error)
    call handle_error(error)

  end subroutine cmd_update

  !> Error handling for this command
  subroutine handle_error(error)
    !> Potential error
    type(error_t), intent(in), optional :: error
    if (present(error)) then
      print '(a)', error%message
      error stop 1
    end if
  end subroutine handle_error

end module fpm_cmd_update
