module fpm

use fpm_strings, only: string_t, str_ends_with
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings
use fpm_environment, only: run, get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only: join_path, number_of_rows, list_files, exists
use fpm_model, only: srcfile_ptr, srcfile_t, fpm_model_t
use fpm_sources, only: add_executable_sources, add_sources_from_dir, &
                       resolve_module_dependencies
use fpm_manifest, only : get_package_data, default_executable, &
                         default_library, package_t
use fpm_error, only : error_t
implicit none
private
public :: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test


contains

subroutine build_model(model, settings, package, error)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_t), intent(in) :: package
    type(error_t), allocatable, intent(out) :: error

    model%package_name = package%name

    ! #TODO: Choose flags and output directory based on cli settings & manifest inputs
    model%fortran_compiler = 'gfortran'
    model%output_directory = 'build/gfortran_debug'
    model%fortran_compile_flags = ' -Wall -Wextra -Wimplicit-interface  -fPIC -fmax-errors=1 -g '// &
                                  '-fbounds-check -fcheck-array-temporaries -fbacktrace '// &
                                  '-J'//join_path(model%output_directory,model%package_name)
    model%link_flags = ''

    ! Add sources from executable directories
    if (allocated(package%executable)) then

        call add_executable_sources(model%sources, package%executable, &
                                     is_test=.false., error=error)
                        
        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%test)) then

        call add_executable_sources(model%sources, package%test, &
                                     is_test=.true., error=error)

        if (allocated(error)) then
            return
        end if

    end if

    if (allocated(package%library)) then

        call add_sources_from_dir(model%sources,package%library%source_dir, &
                                      error=error)

        if (allocated(error)) then
            return
        end if

    end if

    call resolve_module_dependencies(model%sources)

end subroutine build_model

subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_t) :: package
type(fpm_model_t) :: model
type(error_t), allocatable :: error
call get_package_data(package, "fpm.toml", error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

! Populate library in case we find the default src directory
if (.not.allocated(package%library) .and. exists("src")) then
    allocate(package%library)
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

call build_model(model, settings, package, error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

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
