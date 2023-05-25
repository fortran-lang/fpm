module metapackage_minpack
  use minpack_module, only: wp
  use iso_fortran_env, only: real64
  implicit none
  private

  public :: simple_test
contains
  subroutine simple_test(success)
    logical, intent(out) :: success 
    ! Success! can read minpack module
    success = wp == real64 
  end subroutine simple_test 
end module metapackage_minpack
