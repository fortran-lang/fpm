program preprocess_hello
    use preprocess_hello_dependency, only: say_hello

    implicit none
    call say_hello()
end program preprocess_hello
