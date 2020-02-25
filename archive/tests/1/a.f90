module a
use b, only: g
implicit none

contains

    subroutine f()
    call g()
    end subroutine

end module
