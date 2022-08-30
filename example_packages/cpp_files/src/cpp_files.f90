module cpp_files
  use, intrinsic :: ISO_C_Binding
  implicit none
  private

  public :: intvec_maxval

  interface
    integer function intvec_maxval(array, n) bind(C, name = "intvec_maxval")
      integer, intent(in) :: array(*)
      integer, intent(in), value :: n
    end function intvec_maxval
  end interface
end module cpp_files
