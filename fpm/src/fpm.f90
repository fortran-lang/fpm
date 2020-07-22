module fpm
implicit none
private
public :: print_help, cmd_build

integer, parameter :: os_linux = 1
integer, parameter :: os_macos = 2
integer, parameter :: os_windows = 3

contains

integer function get_os() result(r)
! Determines the OS type.
!
! Currently we use the $HOMEPATH and $HOME environment variables to determine
! the OS type. That is not 100% accurate in all cases, but it seems to be good
! enough for now. See the following issue for a more robust solution:
!
! https://github.com/fortran-lang/fpm/issues/144
!
character(len=100) :: val
integer stat
! Only Windows define $HOMEPATH by default (if a user defines $HOMEPATH on Linux
! or macOS, then this will be wrong):
call get_environment_variable("HOMEPATH", val, status=stat)
if (stat == 0) then
    r = os_windows
    return
end if

! We assume that $HOME=/home/... is Linux, $HOME=/Users/... is macOS, otherwise
! we assume Linux. This is only a heuristic and can easily fail.
call get_environment_variable("HOME", val, status=stat)
if (stat == 1) then
    print *, "$HOME does not exist"
    error stop
end if
if (stat /= 0) then
    print *, "get_environment_variable() failed"
    error stop
end if
if (val(1:6) == "/home/") then
    r = os_linux
else if (val(1:7) == "/Users/") then
    r = os_macos
else
    ! This will happen on HPC systems that typically do not use either /home nor
    ! /Users for $HOME. Those systems are typically Linux, so for now we simply
    ! set Linux here.
    r = os_linux
end if
end function

subroutine print_help()
integer :: ostype
ostype = get_os()
print *, "Fortran Package Manager (fpm)"
print *, "OS Type: ", ostype
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
