module fpm
implicit none
private
public :: print_help, cmd_build

integer, parameter :: os_linux = 1
integer, parameter :: os_macos = 2
integer, parameter :: os_windows = 3

contains

integer function get_os() result(r)
#ifdef _WIN32
    r = os_windows
#elif defined __APPLE__
    r = os_macos
#elif defined __linux__
    r = os_linux
#else
    ! Unsupported platform
    r = -1
    error stop
#endif
end function

subroutine print_help()
integer :: ostype
ostype = get_os()
print *, "Fortran Package Manager (fpm)"
print *, ostype
end subroutine

subroutine run(cmd)
character(len=*), intent(in) :: cmd
integer :: stat
print *, "+ ", cmd
call execute_command_line(cmd, exitstat=stat)
if (stat /= 0) then
    print *, "Command failed"
    error stop
end if
end subroutine

logical function exists(filename) result(r)
character(len=*), intent(in) :: filename
inquire(file=filename, exist=r)
end function

subroutine cmd_build()
logical :: src
print *, "# Building project"
src = exists("src/fpm.F90")
if (src) then
    call run("gfortran -c src/fpm.F90 -o fpm.o")
end if
call run("gfortran -c app/main.f90 -o main.o")
if (src) then
    call run("gfortran main.o fpm.o -o fpm")
else
    call run("gfortran main.o -o hello_world")
end if
end subroutine

end module fpm
