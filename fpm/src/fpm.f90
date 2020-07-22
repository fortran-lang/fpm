module fpm
implicit none
private
public :: print_help, cmd_build

integer, parameter :: OS_LINUX = 1
integer, parameter :: OS_MACOS = 2
integer, parameter :: OS_WINDOWS = 3

type string_t
    character(len=:), allocatable :: s
end type

contains

integer function get_os_type() result(r)
! Determine the OS type
!
! Returns one of OS_LINUX, OS_MACOS, OS_WINDOWS.
!
! Currently we use the $HOME and $HOMEPATH environment variables to determine
! the OS type. That is not 100% accurate in all cases, but it seems to be good
! enough for now. See the following issue for a more robust solution:
!
! https://github.com/fortran-lang/fpm/issues/144
!
character(len=100) :: val
integer stat
! Only Windows define $HOMEPATH by default and we test its value to improve the
! chances of it working even if a user defines $HOMEPATH on Linux or macOS.
call get_environment_variable("HOMEPATH", val, status=stat)
if (stat == 0 .and. val(1:7) == "\Users\") then
    r = OS_WINDOWS
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
    r = OS_LINUX
else if (val(1:7) == "/Users/") then
    r = OS_MACOS
else
    ! This will happen on HPC systems that typically do not use either /home nor
    ! /Users for $HOME. Those systems are typically Linux, so for now we simply
    ! set Linux here.
    r = OS_LINUX
end if
end function

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
select case (get_os_type())
    case (OS_LINUX)
        ! TODO: add `dir` into the `ls` command here:
        call execute_command_line("ls " // dir // " > fpm_ls.out", exitstat=stat)
    case (OS_MACOS)
        print *, "macOS not supported yet"
        error stop
    case (OS_WINDOWS)
        print *, "Windows not supported yet"
        error stop
end select
if (stat /= 0) then
    print *, "execute_command_line() failed"
end if
open(newunit=u, file="fpm_ls.out", status="old")
allocate(files(number_of_rows(u)))
do i = 1, size(files)
    read(u, *) filename
    files(i)%s = trim(filename)
end do
close(u)
end subroutine

subroutine print_help()
print *, "Fortran Package Manager (fpm)"
select case (get_os_type())
    case (OS_LINUX)
        print *, "OS Type: Linux"
    case (OS_MACOS)
        print *, "OS Type: macOS"
    case (OS_WINDOWS)
        print *, "OS Type: Windows"
end select
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

end module fpm
