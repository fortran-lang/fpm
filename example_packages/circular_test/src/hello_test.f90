module hello_test
  use greet_m, only: make_greeting

  implicit none
  private

  public :: run_test
contains
  subroutine run_test
    print *, make_greeting("from test")
  end subroutine run_test
end module hello_test
