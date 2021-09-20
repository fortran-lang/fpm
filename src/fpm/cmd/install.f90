module fpm_cmd_install
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm, only : build_model
  use fpm_backend, only : build_package
  use fpm_command_line, only : fpm_install_settings
  use fpm_error, only : error_t, fatal_error, fpm_stop
  use fpm_filesystem, only : join_path, list_files
  use fpm_installer, only : installer_t, new_installer
  use fpm_manifest, only : package_config_t, get_package_data
  use fpm_model, only : fpm_model_t, FPM_SCOPE_APP
  use fpm_targets, only: targets_from_sources, build_target_t, &
                         build_target_ptr, FPM_TARGET_EXECUTABLE, &
                         filter_library_targets, filter_executable_targets
  use fpm_strings, only : string_t, resize
  implicit none
  private

  public :: cmd_install

contains

  !> Entry point for the fpm-install subcommand
  subroutine cmd_install(settings)
    !> Representation of the command line settings
    type(fpm_install_settings), intent(in) :: settings
    type(package_config_t) :: package
    type(error_t), allocatable :: error
    type(fpm_model_t) :: model
    type(build_target_ptr), allocatable :: targets(:)
    type(installer_t) :: installer
    character(len=:), allocatable :: lib, dir
    type(string_t), allocatable :: list(:)
    logical :: installable

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    call handle_error(error)

    call build_model(model, settings%fpm_build_settings, package, error)
    call handle_error(error)

    call targets_from_sources(targets, model, error)
    call handle_error(error)

    installable = (allocated(package%library) .and. package%install%library) &
      .or. allocated(package%executable)
    if (.not.installable) then
      call fatal_error(error, "Project does not contain any installable targets")
      call handle_error(error)
    end if

    if (settings%list) then
      call install_info(output_unit, package, model, targets)
      return
    end if

    if (.not.settings%no_rebuild) then
      call build_package(targets,model)
    end if

    call new_installer(installer, prefix=settings%prefix, &
      bindir=settings%bindir, libdir=settings%libdir, &
      includedir=settings%includedir, &
      verbosity=merge(2, 1, settings%verbose))

    if (allocated(package%library) .and. package%install%library) then
      call filter_library_targets(targets, list)

      if (size(list) > 0) then
        call installer%install_library(list(1)%s, error)
        call handle_error(error)

        call install_module_files(installer, dir, error)
        call handle_error(error)
      end if
    end if

    if (allocated(package%executable)) then
      call install_executables(installer, targets, error)
      call handle_error(error)
    end if

  end subroutine cmd_install

  subroutine install_info(unit, package, model, targets)
    integer, intent(in) :: unit
    type(package_config_t), intent(in) :: package
    type(fpm_model_t), intent(in) :: model
    type(build_target_ptr), intent(in) :: targets(:)

    integer :: ii, ntargets
    character(len=:), allocatable :: lib
    type(string_t), allocatable :: install_target(:), temp(:)

    allocate(install_target(0))

    call filter_library_targets(targets, temp)
    install_target = [install_target, temp]

    call filter_executable_targets(targets, FPM_SCOPE_APP, temp)
    install_target = [install_target, temp]

    ntargets = size(install_target)

    write(unit, '("#", *(1x, g0))') &
      "total number of installable targets:", ntargets
    do ii = 1, ntargets
      write(unit, '("-", *(1x, g0))') install_target(ii)%s
    end do

  end subroutine install_info

  subroutine install_module_files(installer, dir, error)
    type(installer_t), intent(inout) :: installer
    character(len=*), intent(in) :: dir
    type(error_t), allocatable, intent(out) :: error
    type(string_t), allocatable :: modules(:)
    integer :: ii

    call list_files(dir, modules, recurse=.false.)

    do ii = 1, size(modules)
      if (is_module_file(modules(ii)%s)) then
        call installer%install_header(modules(ii)%s, error)
        if (allocated(error)) exit
      end if
    end do
    if (allocated(error)) return

  end subroutine install_module_files

  subroutine install_executables(installer, targets, error)
    type(installer_t), intent(inout) :: installer
    type(build_target_ptr), intent(in) :: targets(:)
    type(error_t), allocatable, intent(out) :: error
    integer :: ii

    do ii = 1, size(targets)
      if (is_executable_target(targets(ii)%ptr)) then
        call installer%install_executable(targets(ii)%ptr%output_file, error)
        if (allocated(error)) exit
      end if
    end do
    if (allocated(error)) return

  end subroutine install_executables

  elemental function is_executable_target(target_ptr) result(is_exe)
    type(build_target_t), intent(in) :: target_ptr
    logical :: is_exe
    is_exe = target_ptr%target_type == FPM_TARGET_EXECUTABLE .and. &
      allocated(target_ptr%dependencies)
    if (is_exe) then
      is_exe = target_ptr%dependencies(1)%ptr%source%unit_scope == FPM_SCOPE_APP
    end if
  end function is_executable_target

  elemental function is_module_file(name) result(is_mod)
    character(len=*), intent(in) :: name
    logical :: is_mod
    integer :: ll
    ll = len(name)
    is_mod = name(max(1, ll-3):ll) == ".mod"
  end function is_module_file

  subroutine handle_error(error)
    type(error_t), intent(in), optional :: error
    if (present(error)) then
      call fpm_stop(1,error%message)
    end if
  end subroutine handle_error

end module fpm_cmd_install
