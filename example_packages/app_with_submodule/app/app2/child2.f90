submodule(parent) child2
  implicit none

contains

  module procedure my_sub1
  a = 2
  end procedure my_sub1

end submodule child2
