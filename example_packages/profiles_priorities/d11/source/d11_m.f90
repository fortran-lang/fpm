module d11_m
    implicit none

    public :: create_greeting
contains
    function create_greeting(greeting) result(created)
        character(len=*), intent(in) :: greeting
        character(len=:), allocatable :: created

        created = greeting // " "
    end function create_greeting
end module d11_m


