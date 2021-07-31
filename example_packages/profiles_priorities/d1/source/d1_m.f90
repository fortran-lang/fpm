module d1_m
    use d11_m, only: create_greeting
    use d12_m, only: get_name
    implicit none

    public :: say_hi
contains
    subroutine say_hi()
        print *, create_greeting("hello"), get_name("developer","fpm") 
    end subroutine say_hi
end module d1_m

