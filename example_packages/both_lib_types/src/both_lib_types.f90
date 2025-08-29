module both_lib_types
  implicit none
  private

  public :: say_hello
  public :: get_number
contains
  subroutine say_hello
    print *, "Hello from both_lib_types!"
  end subroutine say_hello
  
  integer function get_number()
     get_number = 42
  end function get_number
end module both_lib_types