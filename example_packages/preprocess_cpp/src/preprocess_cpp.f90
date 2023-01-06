module preprocess_cpp
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, preprocess_cpp!"
#ifndef TESTMACRO
    This breaks the build.
#endif

#if TESTMACRO2 != 3
    This breaks the build.
#endif

#if TESTMACRO3 != 1
    This breaks the build.
#endif

  end subroutine say_hello
end module preprocess_cpp
