module features_demo
    implicit none
    private
    public :: show_features, get_build_info

contains

    !> Display which features are enabled
    subroutine show_features()
        write(*,*) 'FPM Features Demo'
        write(*,*) '================='
        
        ! Debug/Release flags
#ifdef DEBUG
        write(*,*) '✓ DEBUG mode enabled'
#endif
#ifdef RELEASE  
        write(*,*) '✓ RELEASE mode enabled'
#endif

        ! Platform detection
#ifdef LINUX_BUILD
        write(*,*) '✓ Linux platform detected'
#endif
#ifdef WINDOWS_BUILD
        write(*,*) '✓ Windows platform detected'  
#endif

        ! Parallel features
#ifdef USE_MPI
        write(*,*) '✓ MPI support enabled'
#endif
#ifdef USE_OPENMP
        write(*,*) '✓ OpenMP support enabled'
#endif

#ifdef PROF_FEAT1
        write(*,*) '✓ PROF_FEAT1 enabled'
#endif
#ifdef PROF_FEAT2
        write(*,*) '✓ PROF_FEAT2 enabled'
#endif

        ! Compiler info (if available)
        write(*,*) 'Build configuration:'
        call show_compiler_info()
        
    end subroutine show_features

    !> Show compiler information
    subroutine show_compiler_info()
#ifdef __GFORTRAN__
        write(*,*) '  - Compiler: GNU Fortran'
#endif
#ifdef __INTEL_COMPILER
        write(*,*) '  - Compiler: Intel Fortran'
#endif
    end subroutine show_compiler_info

    !> Get build information as a string
    function get_build_info() result(info)
        character(len=200) :: info
        
        info = 'Features: '
        
#ifdef DEBUG
        info = trim(info) // ' DEBUG'
#endif
#ifdef RELEASE
        info = trim(info) // ' RELEASE'  
#endif
#ifdef USE_MPI
        info = trim(info) // ' MPI'
#endif
#ifdef USE_OPENMP
        info = trim(info) // ' OPENMP'
#endif

        if (len_trim(info) == 10) then ! Only "Features: "
            info = trim(info) // 'NONE'
        end if
        
    end function get_build_info

end module features_demo
