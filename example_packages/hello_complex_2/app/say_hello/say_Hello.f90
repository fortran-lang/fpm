program say_Hello
    use greet_m, only: make_greeting
    use app_hello_mod, only: greet_object

    implicit none

    print *, make_greeting(greet_object)
end program say_Hello
