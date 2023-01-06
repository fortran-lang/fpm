module test_mod
  implicit none

  include "parameters.f90"

contains

  subroutine test_sub(a)
    real(dp), intent(in) :: a

    write (*, *) 'a: ', a
  end subroutine test_sub

end module test_mod
