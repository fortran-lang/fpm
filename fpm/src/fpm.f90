module fpm
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Fortran Package Manager (fpm)"
  end subroutine say_hello
end module fpm
