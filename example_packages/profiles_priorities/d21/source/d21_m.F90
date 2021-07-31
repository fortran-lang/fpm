module d21_m
    implicit none

    public :: count_iter
contains
    subroutine count_iter(n)
        integer :: n, i
        if (not_defined /= 2) stop
        do i=1,n
            print *,i
        end do
    end subroutine count_iter
end module d21_m



