module wrapper_mod
  use hello_makefile, only: say_hello_from_makefile

  implicit none
  private

  public :: do_stuff
contains
  subroutine do_stuff
    call say_hello_from_makefile
  end subroutine do_stuff
end module wrapper_mod
