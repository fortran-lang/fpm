module fpm
use fpm_strings
use fpm_command_line
use fpm_manifest
use fpm_model
use fpm_environment, only: run, get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only: number_of_rows, list_files, exists
use fpm_sources
use fpm_backend
implicit none
private
public :: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test


contains


subroutine package_name(name)
character(:), allocatable, intent(out) :: name
! Currrently a heuristic. We should update this to read the name from fpm.toml
if (exists("src/fpm.f90")) then
    name = "fpm"
else
    name = "hello_world"
end if
end subroutine

subroutine cmd_build(settings)
    type(fpm_build_settings), intent(in) :: settings

    type(fpm_manifest_t) :: manifest
    type(fpm_model_t) :: model

    print *, "# Building project"

    call build_model(model, settings, manifest)

    call build_package(model)

end subroutine

subroutine cmd_install()
    print *, "fpm error: 'fpm install' not implemented."
    error stop 1
end subroutine

subroutine cmd_new()
    print *, "fpm error: 'fpm new' not implemented."
    error stop 1
end subroutine

subroutine cmd_run()
    print *, "fpm error: 'fpm run' not implemented."
    error stop 1
end subroutine

subroutine cmd_test()
    print *, "fpm error: 'fpm test' not implemented."
    error stop 1
end subroutine

end module fpm
