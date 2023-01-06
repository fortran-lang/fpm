program say_Hello
  use greet_m, only: make_greeting

  implicit none

  interface
    function external_function() result(i)
      integer :: i
    end function external_function
  end interface

  print *, make_greeting("World")
  print *, external_function()

end program say_Hello
