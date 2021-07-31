module d2_m
    use d21_m, only: count_iter
    use d22_m, only: count_rec
    implicit none

    public :: count_to_ten
contains
    subroutine count_to_ten()
        print *,"This is test of counting to ten:"
        print *,"Iterative version"
        call count_iter(10)
        print *,"Recursive version"
        call count_rec(1,10)
    end subroutine count_to_ten
end module d2_m


