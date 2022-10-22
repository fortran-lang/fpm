module utils

    implicit none
    
contains

    subroutine say_hello()
        print '(a,1x,i0)', "Hello, X =", X
    end subroutine say_hello

end module utils
