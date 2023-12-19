module metapackage_stdlib
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, metapackage_stdlib!"
  end subroutine say_hello
end module metapackage_stdlib
