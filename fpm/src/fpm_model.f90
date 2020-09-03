module fpm_model

! Definition and validation of the backend model

use fpm_command_line, only: fpm_build_settings
use fpm_filesystem, only: exists
use fpm_manifest, only: fpm_manifest_t
use fpm_sources, only: resolve_dependencies, scan_sources, srcfile_t
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

subroutine build_model(model, settings, manifest)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(fpm_manifest_t), intent(in) :: manifest

    if (exists("src/fpm.f90")) then
        model%package_name = "fpm"
    else
        model%package_name = "hello_world"
    end if

    ! #TODO: Choose flags and output directory based on cli settings & manifest inputs
    model%fortran_compiler = 'gfortran'
    model%output_directory = 'build/gfortran_debug'
    model%fortran_compile_flags = ' -Wall -Wextra -Wimplicit-interface  -fPIC -fmax-errors=1 -g '// &
                                  '-fbounds-check -fcheck-array-temporaries -fbacktrace '// &
                                  '-J'//model%output_directory
    model%link_flags = ''

    call scan_sources(model%sources,[string_t('app'),string_t('src')])

    call resolve_dependencies(model%sources)

end subroutine build_model

end module fpm_model
