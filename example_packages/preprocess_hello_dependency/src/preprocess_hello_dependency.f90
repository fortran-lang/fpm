module preprocess_hello_dependency
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello

!> If this build fails, then it implies that macros are getting passed to the dependency.
#ifdef FOO
    This breaks the build inside dependency. This implies that macros are getting passed to the dependeny.
#endif
    print *, "Hello, preprocess_hello_dependency!"
  end subroutine say_hello
end module preprocess_hello_dependency
