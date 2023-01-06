module farewell_m
  implicit none
  private

  public :: make_farewell
contains
  function make_farewell(name) result(greeting)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: greeting

    greeting = "Goodbye, "//name//"!"
  end function make_farewell
end module farewell_m
