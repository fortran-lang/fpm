module fpm_cmd_export
  use fpm_command_line, only : fpm_export_settings
  use fpm_dependency, only : dependency_tree_t, new_dependency_tree
  use fpm_error, only : error_t, fpm_stop
  use fpm_filesystem, only : join_path
  use fpm_lock, only : fpm_lock_acquire, fpm_lock_release
  use fpm_manifest, only : package_config_t, get_package_data
  use fpm_toml, only: name_is_json
  use fpm_model, only: fpm_model_t
  use fpm, only: build_model
  implicit none
  private
  public :: cmd_export

contains

  !> Entry point for the export subcommand
  subroutine cmd_export(settings)
    !> Representation of the command line arguments
    type(fpm_export_settings), intent(inout) :: settings
    type(package_config_t) :: package
    type(dependency_tree_t) :: deps
    type(fpm_model_t) :: model
    type(error_t), allocatable :: error

    integer :: ii
    character(len=:), allocatable :: filename

    call fpm_lock_acquire(error)
    call handle_error(error)

    if (len_trim(settings%dump_manifest)<=0 .and. &
        len_trim(settings%dump_model)<=0 .and. &
        len_trim(settings%dump_dependencies)<=0) then
        call fpm_stop(0,'*cmd_export* exiting: no manifest/model/dependencies keyword provided')
    end if

    !> Read in manifest
    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    call handle_error(error)

    !> Export manifest
    if (len_trim(settings%dump_manifest)>0) then
       filename = trim(settings%dump_manifest)
       call package%dump(filename, error, json=name_is_json(filename))
    end if

    !> Export dependency tree
    if (len_trim(settings%dump_dependencies)>0) then

        !> Generate dependency tree
        filename = join_path("build", "cache.toml")
        call new_dependency_tree(deps, cache=filename, verbosity=merge(2, 1, settings%verbose))
        call deps%add(package, error)
        call handle_error(error)

        !> Export dependency tree
        filename = settings%dump_dependencies
        call deps%dump(filename, error, json=name_is_json(filename))
        call handle_error(error)
    end if

    !> Export full model
    if (len_trim(settings%dump_model)>0) then

        call build_model(model, settings%fpm_build_settings, package, error)
        if (allocated(error)) then
            call fpm_stop(1,'*cmd_export* Model error: '//error%message)
        end if

        filename = settings%dump_model
        call model%dump(filename, error, json=name_is_json(filename))
        call handle_error(error)
    end if

    call fpm_lock_release(error)
    call handle_error(error)

  end subroutine cmd_export

  !> Error handling for this command
  subroutine handle_error(error)
    !> Potential error
    type(error_t), intent(in), optional :: error
    if (present(error)) then
      call fpm_stop(1, '*cmd_export* error: '//error%message)
    end if
  end subroutine handle_error

end module fpm_cmd_export
