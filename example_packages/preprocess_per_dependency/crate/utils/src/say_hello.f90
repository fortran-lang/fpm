module utils

    implicit none
    
contains

    subroutine say_hello(ierr)
        integer, intent(out) :: ierr

        ierr = -1
#ifdef DEPENDENCY_MACRO
        ierr = 0
#endif

        print *, "Dependency macro ", merge(" IS","NOT",ierr==0)," defined" 

    end subroutine say_hello

end module utils
