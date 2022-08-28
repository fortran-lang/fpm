module cpp_files
  use, intrinsic :: ISO_C_Binding
  implicit none
  private

  public :: hello_world

  interface
    subroutine hello_world() bind(C, name = "hello_world")
    end subroutine hello_world
  end interface
end module cpp_files
