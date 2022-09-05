module parent
implicit none

interface

    module subroutine my_sub1(a)
        integer, intent(out) :: a
    end subroutine my_sub1
end interface

end module parent