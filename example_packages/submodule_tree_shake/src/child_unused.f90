submodule(parent_unused) child_unused
  implicit none

contains

  module procedure unused_sub
  a = 1
  end procedure unused_sub

end submodule child_unused
