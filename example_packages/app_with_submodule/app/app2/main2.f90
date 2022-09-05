program test
use parent
implicit none

integer :: a

call my_sub1(a)

if (a /= 2) then
    write(*,*) 'FAILED: Unexpected value of a'
    stop 1
end if

end program test