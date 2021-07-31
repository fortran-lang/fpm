module d22_m
    implicit none

    public :: count_rec
contains
    recursive subroutine count_rec(c, n)
        integer :: c,n
        if (not_defined /= 1) stop 1
        if (n > 0) then
           print *,c
           call count_rec(c+1, n-1)
        end if
    end subroutine count_rec
end module d22_m


