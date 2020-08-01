module fpm
use environment, only: get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
implicit none
private
public :: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test

type string_t
    character(len=:), allocatable :: s
end type

contains

integer function number_of_rows(s) result(nrows)
! determine number or rows
integer,intent(in)::s
integer :: ios
character(len=100) :: r
rewind(s)
nrows = 0
do
    read(s, *, iostat=ios) r
    if (ios /= 0) exit
    nrows = nrows + 1
end do
rewind(s)
end function


subroutine list_files(dir, files)
character(len=*), intent(in) :: dir
type(string_t), allocatable, intent(out) :: files(:)
character(len=100) :: filename
integer :: stat, u, i
! Using `inquire` / exists on directories works with gfortran, but not ifort
if (.not. exists(dir)) then
    allocate(files(0))
    return
end if
select case (get_os_type())
    case (OS_LINUX)
        call execute_command_line("ls " // dir // " > fpm_ls.out", exitstat=stat)
    case (OS_MACOS)
        call execute_command_line("ls " // dir // " > fpm_ls.out", exitstat=stat)
    case (OS_WINDOWS)
        call execute_command_line("dir /b " // dir // " > fpm_ls.out", exitstat=stat)
end select
if (stat /= 0) then
    print *, "execute_command_line() failed"
    error stop
end if
open(newunit=u, file="fpm_ls.out", status="old")
allocate(files(number_of_rows(u)))
do i = 1, size(files)
    read(u, *) filename
    files(i)%s = trim(filename)
end do
close(u)
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

logical function str_ends_with(s, e) result(r)
character(*), intent(in) :: s, e
integer :: n1, n2
n1 = len(s)-len(e)+1
n2 = len(s)
if (n1 < 1) then
    r = .false.
else
    r = (s(n1:n2) == e)
end if
end function

subroutine package_name(name)
character(:), allocatable, intent(out) :: name
! Currrently a heuristic. We should update this to read the name from fpm.toml
if (exists("src/fpm.f90")) then
    name = "fpm"
else
    name = "hello_world"
end if
end subroutine

subroutine cmd_build()
type(string_t), allocatable :: files(:)
character(:), allocatable :: basename, pkg_name, linking
integer :: i, n
print *, "# Building project"
call list_files("src", files)
linking = ""
do i = 1, size(files)
    if (str_ends_with(files(i)%s, ".f90")) then
        n = len(files(i)%s)
        basename = files(i)%s(1:n-4)
        call run("gfortran -c src/" // basename // ".f90 -o " // basename // ".o")
        linking = linking // " " // basename // ".o"
    end if
end do
call run("gfortran -c app/main.f90 -o main.o")
call package_name(pkg_name)
call run("gfortran main.o " // linking // " -o " // pkg_name)
end subroutine

subroutine cmd_install()
    print *, "fpm error: 'fpm install' not implemented."
    error stop 1
end subroutine

subroutine cmd_new()
    print *, "fpm error: 'fpm new' not implemented."
    error stop 1
end subroutine

subroutine cmd_run()
    print *, "fpm error: 'fpm run' not implemented."
    error stop 1
end subroutine

subroutine cmd_test()
    print *, "fpm error: 'fpm test' not implemented."
    error stop 1
end subroutine

end module fpm
