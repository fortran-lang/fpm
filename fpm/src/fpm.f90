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

subroutine cmd_build()
print *, "# Building project"
call run("gfortran -c src/fpm.f90 -o fpm.o")
call run("gfortran -c app/main.f90 -o main.o")
call run("gfortran main.o fpm.o -o fpm")
end subroutine

end module fpm
