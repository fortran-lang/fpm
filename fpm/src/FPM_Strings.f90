module FPM_Strings
implicit none

type string_t
    character(len=:), allocatable :: s
end type

contains

logical function str_ends_with(s, e) result(r)
    character(*), intent(in) :: s, e
    integer :: n1, n2
    n1 = len(s)-len(e)+1
    n2 = len(s)
    if (n1 < 1) then
        r = .false.
    else
        r = (s(n1:n2) == e)
    end if
end function


end module FPM_Strings