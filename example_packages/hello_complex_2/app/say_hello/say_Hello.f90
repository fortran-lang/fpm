program say_Hello
    use greet_m, only: make_greeting
    use app_hello_mod

    implicit none

    print *, make_greeting("World")
end program say_Hello
