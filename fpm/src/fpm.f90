module fpm
implicit none
private
public :: print_help, cmd_build

contains

subroutine print_help()
print *, "Fortran Package Manager (fpm)"
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
src = exists("src/fpm.f90")
if (src) then
    call run("gfortran -c src/fpm.f90 -o fpm.o")
end if
call run("gfortran -c app/main.f90 -o main.o")
if (src) then
    call run("gfortran main.o fpm.o -o fpm")
else
    call run("gfortran main.o -o hello_world")
end if
end subroutine

end module fpm
