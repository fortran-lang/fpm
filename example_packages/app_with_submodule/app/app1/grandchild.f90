submodule(parent:child1) grandchild
implicit none

contains

module procedure my_fun
    b = 1
end procedure my_fun

end submodule grandchild