program hello_fpm
    use utils, only: say_hello
    integer :: ierr

    call say_hello(ierr)
    stop ierr ! ierr==0 if DEPENDENCY_MACRO is defined

end program hello_fpm
