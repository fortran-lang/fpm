module preprocess_hello_dependency
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello

!> This build should break because we do not have FOO defined as macro in the fpm.toml file of
!> this package.
#ifndef FOO
    This breaks the build inside dependency. This implies that macros are not getting passed to the dependeny.
#endif
    print *, "Hello, preprocess_hello_dependency!"
  end subroutine say_hello
end module preprocess_hello_dependency
