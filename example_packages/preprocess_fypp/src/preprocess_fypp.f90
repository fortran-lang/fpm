module preprocess_fypp
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, preprocess_fypp!"
  end subroutine say_hello
end module preprocess_fypp
