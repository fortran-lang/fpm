module a_mod
  use b_mod, only: hello_world

contains

  subroutine a_mod_sub()
    call hello_world()
  end subroutine

end module
