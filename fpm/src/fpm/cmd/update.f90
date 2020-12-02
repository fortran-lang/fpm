module fpm_cmd_update
  use fpm_command_line, only : fpm_update_settings
  use fpm_dependency, only : dependency_tree_t, new_dependency_tree
  use fpm_error, only : error_t
  use fpm_filesystem, only : exists, mkdir, join_path
  use fpm_manifest, only : package_config_t, get_package_data
  implicit none
  private
  public :: cmd_update

contains

  !> Entry point for the update subcommand
  subroutine cmd_update(settings)
    !> Representation of the command line arguments
    type(fpm_update_settings), intent(in) :: settings
    type(package_config_t) :: package
    type(dependency_tree_t) :: deps
    type(error_t), allocatable :: error

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    call handle_error(error)

    if (.not.exists("build")) then
      call mkdir("build")
    end if

    call new_dependency_tree(deps, cache=join_path("build", "cache.toml"), &
      verbosity=2)

    call deps%add(package, error)
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
