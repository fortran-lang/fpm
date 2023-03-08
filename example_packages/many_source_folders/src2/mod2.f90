module mod2 
  implicit none
  private

  public :: say_hello2
contains
  subroutine say_hello2
    print *, "Hello, from folder src2/ !"
  end subroutine say_hello2
end module mod2
