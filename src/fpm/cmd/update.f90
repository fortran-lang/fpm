module fpm_cmd_update
  use fpm_global, only : config
  use fpm_environment, only : fpm_stop
  use fpm_command_line, only : fpm_update_settings
  use fpm_dependency, only : dependency_tree_t, new_dependency_tree
  use fpm_error, only : error_t
  use fpm_filesystem, only : exists, mkdir, join_path, delete_file
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

    integer :: ii
    character(len=:), allocatable :: cache

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    call handle_error(error)

    if (.not.exists("build")) then
      call mkdir("build")
    end if

    cache = join_path("build", "cache.toml")
    if (settings%clean) then
      call delete_file(cache)
    end if

    call new_dependency_tree(deps, cache=cache, &
      verbosity=merge(2, 1, config%verbose))

    call deps%add(package, error)
    call handle_error(error)

    if (settings%fetch_only) return

    if (size(settings%name) == 0) then
      do ii = 1, deps%ndep
        call deps%update(deps%dep(ii)%name, error)
        call handle_error(error)
      end do
    else
      do ii = 1, size(settings%name)
        call deps%update(trim(settings%name(ii)), error)
        call handle_error(error)
      end do
    end if

  end subroutine cmd_update

  !> Error handling for this command
  subroutine handle_error(error)
    !> Potential error
    type(error_t), intent(in), optional :: error
    if (present(error)) then
      call fpm_stop(stopcode=1,stopmsg=error%message)
    end if
  end subroutine handle_error

end module fpm_cmd_update
