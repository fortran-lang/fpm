module fpm_cmd_update
  use fpm_command_line, only : fpm_update_settings
  use fpm_dependency, only : dependency_tree_t, new_dependency_tree
  use fpm_error, only : error_t, fpm_stop
  use fpm_filesystem, only : exists, mkdir, join_path, delete_file, filewrite
  use fpm_manifest, only : package_config_t, get_package_data
  use fpm_toml, only: name_is_json
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

    if (.not. exists("build")) then
      call mkdir("build")
      call filewrite(join_path("build", ".gitignore"),["*"])
    end if

    cache = join_path("build", "cache.toml")
    if (settings%clean) call delete_file(cache)

    call new_dependency_tree(deps, cache=cache, &
      verbosity=merge(2, 1, settings%verbose))

    call deps%add(package, error)
    call handle_error(error)

    ! Force-update all dependencies if `--clean`
    if (settings%clean) then
        do ii = 1, deps%ndep
            deps%dep(ii)%update = .true.
        end do
    end if

    if (settings%fetch_only) return

    if (size(settings%name) == 0) then
      call deps%update(error)
      call handle_error(error)
    else
      do ii = 1, size(settings%name)
        call deps%update(trim(settings%name(ii)), error)
        call handle_error(error)
      end do
    end if

    if (len_trim(settings%dump)>0) then
        call deps%dump(trim(settings%dump), error, json=name_is_json(trim(settings%dump)))
        call handle_error(error)
    end if

  end subroutine cmd_update

  !> Error handling for this command
  subroutine handle_error(error)
    !> Potential error
    type(error_t), intent(in), optional :: error
    if (present(error)) then
      call fpm_stop(1, '*cmd_update* error: '//error%message)
    end if
  end subroutine handle_error

end module fpm_cmd_update
