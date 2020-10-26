module greet_m
    use subdir_constants, only: GREET_STR
    implicit none
    private

    public :: make_greeting
contains
    function make_greeting(name) result(greeting)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: greeting

        greeting = GREET_STR // name // "!"
    end function make_greeting
end module greet_m
