!># Release parameters
!> Module fpm_release contains public constants storing this build's unique version IDs
module fpm_release
    use fpm_versioning, only: version_t,new_version
    use fpm_error, only: error_t, fpm_stop
    implicit none
    private

#ifndef FPM_VERSION
#define FPM_VERSION UNDEFINED
#endif
    character(len=*), parameter :: fpm_version_ID = "FPM_VERSION"

    public :: fpm_version

    contains

    !> Return the current fpm version from fpm_version_ID as a version type
    type(version_t) function fpm_version()

        type(error_t), allocatable :: error

        call new_version(fpm_version,fpm_version_ID,error)

        if (allocated(error)) call fpm_stop(1,'*fpm*:internal error: cannot get version - '//error%message)

    end function fpm_version

end module fpm_release
