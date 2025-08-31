module fpm_cmd_install
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm, only : build_model
  use fpm_backend, only : build_package
  use fpm_command_line, only : fpm_install_settings
  use fpm_error, only : error_t, fatal_error, fpm_stop
  use fpm_filesystem, only : join_path, list_files
  use fpm_installer, only : installer_t, new_installer
  use fpm_manifest, only : package_config_t, get_package_data
  use fpm_model, only : fpm_model_t, FPM_SCOPE_APP, FPM_SCOPE_TEST
  use fpm_targets, only: targets_from_sources, build_target_t, &
                         build_target_ptr, FPM_TARGET_EXECUTABLE, &
                         filter_library_targets, filter_executable_targets, filter_modules
  use fpm_strings, only : string_t, resize
  implicit none
  private

  public :: cmd_install

contains

  !> Entry point for the fpm-install subcommand
  subroutine cmd_install(settings)
    !> Representation of the command line settings
    type(fpm_install_settings), intent(inout) :: settings
    type(package_config_t) :: package
    type(error_t), allocatable :: error
    type(fpm_model_t) :: model
    type(build_target_ptr), allocatable :: targets(:), libraries(:)
    type(installer_t) :: installer
    type(string_t), allocatable :: list(:)
    logical :: installable
    integer :: ntargets,i

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    call handle_error(error)
    
    call build_model(model, settings, package, error)
    call handle_error(error)

    ! ifx bug: does not resolve allocatable -> optional
    if (allocated(package%library)) then 
       call targets_from_sources(targets, model, settings%prune, package%library, error)
    else
       call targets_from_sources(targets, model, settings%prune, error=error) 
    endif
    call handle_error(error)

    call install_info(output_unit, settings%list, targets, ntargets)
    if (settings%list) return

    installable = (allocated(package%library) .and. package%install%library) &
                   .or. allocated(package%executable) .or. ntargets>0
    
    if (.not.installable) then
      call fatal_error(error, "Project does not contain any installable targets")
      call handle_error(error)
    end if

    if (.not.settings%no_rebuild) then
      call build_package(targets,model,verbose=settings%verbose,dry_run=settings%list)
    end if

    call new_installer(installer, prefix=settings%prefix, &
      bindir=settings%bindir, libdir=settings%libdir, testdir=settings%testdir, &
      includedir=settings%includedir, &
      verbosity=merge(2, 1, settings%verbose))

    if (allocated(package%library) .and. package%install%library) then
      call filter_library_targets(targets, libraries)

      if (size(libraries) > 0) then
        do i=1,size(libraries)
           call installer%install_library(libraries(i)%ptr, error)
           call handle_error(error)
        end do

        call install_module_files(installer, targets, error)
        call handle_error(error)
      end if
    end if
    
    if (allocated(package%executable) .or. ntargets>0) then
      call install_executables(installer, targets, error)
      call handle_error(error)
    end if

    if (allocated(package%test) .and. (package%install%test .or. model%include_tests)) then 
        
        call install_tests(installer, targets, error)
        call handle_error(error)
        
    end if

  end subroutine cmd_install

  subroutine install_info(unit, verbose, targets, ntargets)
    integer, intent(in) :: unit
    logical, intent(in) :: verbose
    type(build_target_ptr), intent(in) :: targets(:)
    integer, intent(out) :: ntargets

    integer :: ii
    type(string_t), allocatable :: install_target(:), apps(:), tests(:)
    type(build_target_ptr), allocatable :: libs(:)

    call filter_library_targets(targets, libs)
    call filter_executable_targets(targets, FPM_SCOPE_APP, apps)
    call filter_executable_targets(targets, FPM_SCOPE_TEST, tests)

    ntargets = size(libs) + size(apps) + size(tests)
    allocate(install_target(ntargets))
    
    do ii = 1, size(libs)
        install_target(ii) = string_t(libs(ii)%ptr%output_file)
    end do
    do ii = 1, size(apps)
        install_target(size(libs) + ii) = string_t(apps(ii)%s)
    end do
    do ii = 1, size(tests)
        install_target(size(libs) + size(apps) + ii) = string_t(tests(ii)%s)
    end do
    
    if (verbose) then 
        write(unit, '("#", *(1x, g0))') &
          "total number of installable targets:", ntargets
        do ii = 1, ntargets
          write(unit, '("-", *(1x, g0))') install_target(ii)%s
        end do
    endif

  end subroutine install_info

  subroutine install_module_files(installer, targets, error)
    type(installer_t), intent(inout) :: installer
    type(build_target_ptr), intent(in) :: targets(:)
    type(error_t), allocatable, intent(out) :: error
    type(string_t), allocatable :: modules(:)
    integer :: ii

    call filter_modules(targets, modules)

    do ii = 1, size(modules)
      call installer%install_header(modules(ii)%s//".mod", error)
      if (allocated(error)) exit
    end do
    if (allocated(error)) return

  end subroutine install_module_files

  subroutine install_executables(installer, targets, error)
    type(installer_t), intent(inout) :: installer
    type(build_target_ptr), intent(in) :: targets(:)
    type(error_t), allocatable, intent(out) :: error
    integer :: ii

    do ii = 1, size(targets)
      if (targets(ii)%ptr%is_executable_target(FPM_SCOPE_APP)) then
        call installer%install_executable(targets(ii)%ptr%output_file, error)
        if (allocated(error)) exit
      end if
    end do
    if (allocated(error)) return

  end subroutine install_executables

  subroutine install_tests(installer, targets, error)
    type(installer_t), intent(inout) :: installer
    type(build_target_ptr), intent(in) :: targets(:)
    type(error_t), allocatable, intent(out) :: error
    integer :: ii
    
    do ii = 1, size(targets)
      if (targets(ii)%ptr%is_executable_target(FPM_SCOPE_TEST)) then
        call installer%install_test(targets(ii)%ptr%output_file, error)
        if (allocated(error)) exit
      end if
    end do
    if (allocated(error)) return

  end subroutine install_tests

  subroutine handle_error(error)
    type(error_t), intent(in), optional :: error
    if (present(error)) then
      call fpm_stop(1,'*cmd_install* error: '//error%message)
    end if
  end subroutine handle_error

end module fpm_cmd_install
