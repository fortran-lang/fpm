module shared_lib
  implicit none
  private

  public :: say_hello
  public :: test_something
contains
  subroutine say_hello
    print *, "Hello, shared_lib!"
  end subroutine say_hello
  integer function test_something()
     test_something = 123
  end function test_something
end module shared_lib
