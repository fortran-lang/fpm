module mod1 
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, from folder src/ !" 
  end subroutine say_hello
end module mod1
