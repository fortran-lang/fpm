module hello_makefile
    implicit none
    private

    public :: say_hello_from_makefile
contains
    subroutine say_hello_from_makefile()
        print *, "Hello from Makefile library!"
    end subroutine say_hello_from_makefile
end module hello_makefile
