program say_Hello
    use greet_m, only: make_greeting

    implicit none

    print *, make_greeting("World")
end program say_Hello
