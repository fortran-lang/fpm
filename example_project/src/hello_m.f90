module hello_m
    implicit none
    private

    public :: sayHello
contains
    subroutine sayHello(name)
        character(len=*), intent(in) :: name

        print *, "Hello, " // name // "!"
    end subroutine sayHello
end module hello_m
