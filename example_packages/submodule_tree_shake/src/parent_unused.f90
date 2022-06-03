module parent_unused
implicit none

interface

    module subroutine unused_sub(a)
        integer, intent(out) :: a
    end subroutine unused_sub
    
end interface

end module parent_unused