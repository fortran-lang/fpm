module features_with_dependency
    use features_demo, only: show_features => show_features, get_build_info
    implicit none
    private
    public :: show_features => show_local_features
    
contains
    
    subroutine show_local_features()
        print *, "Local package features:"
        
#ifdef WITH_DEBUG_DEPENDENCY
        print *, "✓ WITH_DEBUG_DEPENDENCY - dependency built with debug features"
#endif

#ifdef WITH_RELEASE_DEPENDENCY
        print *, "✓ WITH_RELEASE_DEPENDENCY - dependency built with release features"
#endif

#ifdef WITH_MULTI_DEPENDENCY
        print *, "✓ WITH_MULTI_DEPENDENCY - dependency built with debug+mpi features"
#endif

#ifdef LINUX_FEATURES
        print *, "✓ LINUX_FEATURES - Linux-specific dependency features"
#endif

        ! If no local features are active
#if !defined(WITH_DEBUG_DEPENDENCY) && !defined(WITH_RELEASE_DEPENDENCY) && !defined(WITH_MULTI_DEPENDENCY) && !defined(LINUX_FEATURES)
        print *, "  NONE - no local features active"
#endif

        print *, ""
        print *, "Dependency (features_demo) status:"
        call show_features()  ! Call features_demo's show_features
        
    end subroutine show_local_features
    
end module features_with_dependency