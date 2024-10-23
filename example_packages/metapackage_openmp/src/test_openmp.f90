module test_openmp
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello(thread_ID)
    integer, intent(in) :: thread_ID
    print "(a,i0,a)", "Hello, test_openmp is called from thread ",thread_ID,"!"
  end subroutine say_hello
end module test_openmp
