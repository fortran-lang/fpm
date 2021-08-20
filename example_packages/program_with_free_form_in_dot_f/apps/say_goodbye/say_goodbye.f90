program say_goodbye
    use farewell_m, only: make_farewell

    implicit none

    print *, make_farewell("World")
end program say_goodbye
