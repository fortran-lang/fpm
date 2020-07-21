module fpm
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fpm!"
  end subroutine say_hello
end module fpm
