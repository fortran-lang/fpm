module features_default_profile
    implicit none
    private
    public :: show_features

contains

    subroutine show_features()
        write(*,*) 'Default Profile Demo'
        write(*,*) '===================='

#ifdef BASELINE
        write(*,*) 'BASELINE active'
#endif
#ifdef DEBUG
        write(*,*) 'DEBUG active'
#endif
#ifdef RELEASE
        write(*,*) 'RELEASE active'
#endif
#ifdef CUSTOM
        write(*,*) 'CUSTOM active'
#endif

        write(*,*) 'Done'

    end subroutine show_features

end module features_default_profile
