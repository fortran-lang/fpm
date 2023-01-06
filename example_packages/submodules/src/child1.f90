submodule(parent) child1
  implicit none

  interface
    module function my_fun() result(b)
      integer :: b
    end function my_fun
  end interface

contains

  module procedure my_sub1
  a = 1
  end procedure my_sub1

end submodule child1
