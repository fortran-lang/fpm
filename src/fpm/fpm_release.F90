!># Release parameters
!> Module fpm_release contains public constants storing this build's unique version IDs
module fpm_release
    use fpm_versioning, only: version_t,new_version
    use fpm_error, only: error_t, fpm_stop
    implicit none
    private

    public :: fpm_version
    public :: version_t

    contains

    !> Return the current fpm version from fpm_version_ID as a version type
    type(version_t) function fpm_version()

        type(error_t), allocatable :: error

! Fallback to last known version in case of undefined macro
#ifndef FPM_RELEASE_VERSION
#  define FPM_RELEASE_VERSION 0.13.0
#endif

! Accept solution from https://stackoverflow.com/questions/31649691/stringify-macro-with-gnu-gfortran
! which provides the "easiest" way to pass a macro to a string in Fortran complying with both
! gfortran's "traditional" cpp and the standard cpp syntaxes
#ifdef __GFORTRAN__ /* traditional-cpp stringification */
#  define STRINGIFY_START(X) "&
#  define STRINGIFY_END(X) &X"
#else               /* default stringification */
#  define STRINGIFY_(X) #X
#  define STRINGIFY_START(X) &
#  define STRINGIFY_END(X) STRINGIFY_(X)
#endif

        character (len=:), allocatable :: ver_string
        ver_string = STRINGIFY_START(FPM_RELEASE_VERSION)
        STRINGIFY_END(FPM_RELEASE_VERSION)

        call new_version(fpm_version,ver_string,error)

        if (allocated(error)) call fpm_stop(1,'*fpm*:internal error: cannot get version - '//error%message)

    end function fpm_version

end module fpm_release
