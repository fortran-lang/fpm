program hello_fpm
    use farewell_m, only: make_farewell
    use greet_m, only: make_greeting

    implicit none

    print *, make_greeting("fpm")
    print *, make_farewell("fpm")
end program hello_fpm
