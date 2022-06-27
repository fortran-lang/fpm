module preprocess_cpp
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, preprocess_cpp!"
  end subroutine say_hello
end module preprocess_cpp
