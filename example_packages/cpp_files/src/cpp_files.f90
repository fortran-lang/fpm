module cpp_files
  use, intrinsic :: ISO_C_Binding
  implicit none
  private

  public :: intvec_maxval

  interface
    integer(c_int) function intvec_maxval(array, n) bind(C, name = "intvec_maxval")
      import :: c_int, c_size_t
      integer(c_int), intent(in) :: array(*)
      integer(c_size_t), intent(in), value :: n
    end function intvec_maxval
  end interface
end module cpp_files
