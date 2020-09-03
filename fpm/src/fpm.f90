module fpm
use environment, only: get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_manifest, only : get_package_data, default_executable, default_library, &
    & package_t
use fpm_error, only : error_t
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

subroutine cmd_build()
type(package_t) :: package
type(error_t), allocatable :: error
type(string_t), allocatable :: files(:)
character(:), allocatable :: basename, linking
integer :: i, n
call get_package_data(package, "fpm.toml", error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

! Populate library in case we find the default src directory
if (.not.allocated(package%library) .and. exists("src")) then
    call default_library(package%library)
end if

! Populate executable in case we find the default app directory
if (.not.allocated(package%executable) .and. exists("app")) then
    allocate(package%executable(1))
    call default_executable(package%executable(1), package%name)
end if

if (.not.(allocated(package%library) .or. allocated(package%executable))) then
    print '(a)', "Neither library nor executable found, there is nothing to do"
    error stop 1
end if

linking = ""
if (allocated(package%library)) then
    call list_files(package%library%source_dir, files)
    do i = 1, size(files)
        if (str_ends_with(files(i)%s, ".f90")) then
            n = len(files(i)%s)
            basename = files(i)%s
            call run("gfortran -c " // package%library%source_dir // "/" // &
               & basename // " -o " // basename // ".o")
            linking = linking // " " // basename // ".o"
        end if
    end do
end if

do i = 1, size(package%executable)
    basename = package%executable(i)%main
    call run("gfortran -c " // package%executable(i)%source_dir // "/" // &
       & basename // " -o " // basename // ".o")
    call run("gfortran " // basename // ".o " // linking // " -o " // &
       & package%executable(i)%name)
end do
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
