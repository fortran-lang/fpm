module shared_lib_extra
  implicit none
  private

  public :: say_extra_hello
contains
  subroutine say_extra_hello
    print *, "Hello, shared_lib_extra!"
  end subroutine say_extra_hello
end module shared_lib_extra
