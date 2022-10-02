module version_test
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, version_test!"
#if PROJECT_VERSION_MAJOR != 0
      This breaks the build.
#endif

#if PROJECT_VERSION_MINOR != 1
      This breaks the build.
#endif

#if PROJECT_VERSION_PATCH != 2
      This breaks the build.
#endif

  end subroutine say_hello
end module version_test
