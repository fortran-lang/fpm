program hello_fpm
    use utils1_m, only: say_hello1
    use utils1_1_m, only: say_hello1_1
    use utils2_m, only: say_hello2

    call say_hello1()
    call say_hello1_1()
    call say_hello2()

end program hello_fpm
