module fpm_model

! Definition and validation of the backend model

use fpm_command_line, only: fpm_build_settings
use fpm_filesystem, only: exists
use fpm_manifest, only: package_t, default_library, default_executable
use fpm_manifest_executable, only: executable_t
use fpm_sources, only: resolve_module_dependencies, add_sources_from_dir, &
                       add_executable_sources, srcfile_t
use fpm_strings, only: string_t

implicit none

private
public :: build_model, fpm_model_t

type :: fpm_model_t
    character(:), allocatable :: package_name
        ! Name of package
    type(srcfile_t), allocatable :: sources(:)
        ! Array of sources with module-dependencies resolved
    character(:), allocatable :: fortran_compiler
        ! Command line name to invoke fortran compiler
    character(:), allocatable :: fortran_compile_flags
        ! Command line flags passed to fortran for compilation
    character(:), allocatable :: link_flags
        ! Command line flags pass for linking
    character(:), allocatable :: output_directory
        ! Base directory for build
end type fpm_model_t

contains

subroutine build_model(model, settings, package)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_t), intent(in) :: package

    model%package_name = package%name

    ! #TODO: Choose flags and output directory based on cli settings & manifest inputs
    model%fortran_compiler = 'gfortran'
    model%output_directory = 'build/gfortran_debug'
    model%fortran_compile_flags = ' -Wall -Wextra -Wimplicit-interface  -fPIC -fmax-errors=1 -g '// &
                                  '-fbounds-check -fcheck-array-temporaries -fbacktrace '// &
                                  '-J'//model%output_directory
    model%link_flags = ''

    ! Add sources from executable directories
    if (allocated(package%executable)) then
        call add_executable_sources(model%sources, package%executable)
    end if
    if (allocated(package%test)) then
        call add_executable_sources(model%sources, package%test)
    end if

    if (allocated(package%library)) then
        call add_sources_from_dir(model%sources,package%library%source_dir)
    end if

    call resolve_module_dependencies(model%sources)

end subroutine build_model

end module fpm_model
