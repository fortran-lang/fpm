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

function f_string(c_string)
    use iso_c_binding
    character(len=1), intent(in) :: c_string(:)
    character(:), allocatable :: f_string

    integer :: i, n

    i = 0
    do while(c_string(i+1) /= C_NULL_CHAR)
      i = i + 1
    end do
    n = i

    allocate(character(n) :: f_string)
    do i=1,n
      f_string(i:i) = c_string(i)
    end do
  
  end function f_string


end module FPM_Strings