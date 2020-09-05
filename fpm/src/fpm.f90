module fpm

use fpm_strings, only: string_t, str_ends_with
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings
use fpm_environment, only: run, get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only: number_of_rows, list_files, exists
use fpm_model, only: build_model, fpm_model_t
use fpm_manifest, only : get_package_data, default_executable, default_library, &
    & package_t
use fpm_error, only : error_t
implicit none
private
public :: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test


contains


subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_t) :: package
type(fpm_model_t) :: model
type(error_t), allocatable :: error
type(string_t), allocatable :: files(:)
character(:), allocatable :: basename, linking
integer :: i, n
call get_package_data(package, "fpm.toml", error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

! Populate library in case we find the default src directory
if (.not.allocated(package%library) .and. exists("src")) then
    call default_library(package%library)
end if

! Populate executable in case we find the default app directory
if (.not.allocated(package%executable) .and. exists("app")) then
    allocate(package%executable(1))
    call default_executable(package%executable(1), package%name)
end if

if (.not.(allocated(package%library) .or. allocated(package%executable))) then
    print '(a)', "Neither library nor executable found, there is nothing to do"
    error stop 1
end if

call build_model(model, settings, package)

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
