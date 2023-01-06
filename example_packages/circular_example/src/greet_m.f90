module greet_m
  implicit none
  private

  public :: make_greeting
contains
  function make_greeting(name) result(greeting)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: greeting

    greeting = "Hello, "//name//"!"
  end function make_greeting
end module greet_m
