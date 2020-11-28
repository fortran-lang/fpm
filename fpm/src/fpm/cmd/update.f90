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
    new_dependency_walker, check_update_deps
  implicit none
  private
  public :: cmd_update


contains

  !> Entry point for the fpm-update command
  subroutine cmd_update(settings)
    !> Representation of the command line options for this command
    type(fpm_update_settings), intent(in) :: settings

    type(toml_table), allocatable :: table
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

    call update_dep_lock(config, table, package, error)
    call handle_error(error)

    call check_update_deps(config, table, error)
    call handle_error(error)

    call report_dependencies(config, table)

  end subroutine cmd_update

  subroutine report_dependencies(config, table)
    !> Instance of the dependency handler
    class(dependency_walker_t), intent(in) :: config
    !> Table to collect all dependencies
    type(toml_table), intent(inout) :: table

    integer :: ii, unused
    character(len=:), allocatable :: version, path
    type(toml_key), allocatable :: list(:)
    type(toml_table), pointer :: dep
    logical :: required

    call table%get_keys(list)

    unused = 0
    do ii = 1, size(list)
      call get_value(table, list(ii)%key, dep)
      call get_value(dep, "required", required, .false.)
      call get_value(dep, "version", version)
      call get_value(dep, "path", path)
      if (.not.required) unused = unused + 1
      if (config%verbosity > 1) then
        write(config%unit, '("#", *(1x, a:))', advance='no') &
            list(ii)%key, "version", version, "at", path
        if (.not.required) then
          write(config%unit, '(*(1x, a:))', advance='no') "(unused)"
        end if
        write(config%unit, '(a))')
      end if
    end do
    if (unused > 0 .and. config%verbosity > 0) then
      write(config%unit, '("#", 1x, i0, *(1x, a:))') &
        unused, "unused dependencies present"
    end if

  end subroutine report_dependencies

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
