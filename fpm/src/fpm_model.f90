module fpm_model
! Definition and validation of the backend model
!
use fpm_strings
use fpm_sources
use fpm_command_line
use fpm_manifest
implicit none

type fpm_model_t
    type(srcfile_t), allocatable :: sources(:)
        ! Array of sources with module-dependencies resolved

end type fpm_model_t

contains

subroutine build_model(model, settings, manifest)
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(fpm_manifest_t), intent(in) :: manifest

    call scan_sources(model%sources,[string_t('app'),string_t('src')])

    call resolve_dependencies(model%sources)

end subroutine build_model

end module fpm_model