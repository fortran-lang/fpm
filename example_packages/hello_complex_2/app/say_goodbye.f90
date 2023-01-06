program say_goodbye
  use farewell_m, only: make_farewell
  use app_mod

  implicit none

  print *, make_farewell("World")
end program say_goodbye
