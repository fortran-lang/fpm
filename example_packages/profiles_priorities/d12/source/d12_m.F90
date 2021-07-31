module d12_m
    implicit none

    public :: get_name
contains
    function get_name(name, surname) result(full_name)
        character(len=*), intent(in) :: name, surname
        character(len=:), allocatable :: full_name

        if (not_defined /= 3) stop
        full_name = surname // " " // name
    end function get_name
end module d12_m



