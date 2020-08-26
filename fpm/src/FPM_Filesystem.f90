module FPM_Filesystem
use FPM_Strings
use environment, only: get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
implicit none

private
public :: number_of_rows, list_files, exists

integer, parameter :: LINE_BUFFER_LEN = 1000

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

function read_lines(filename) result(lines)
    character(*), intent(in) :: filename
    type(string_t), allocatable :: lines(:)

    integer :: fh, i
    character(LINE_BUFFER_LEN) :: line_buffer

    open(newunit=fh, file=filename, status="old")
    allocate(lines(number_of_rows(fh)))
    do i = 1, size(lines)
        read(fh, *) line_buffer
        lines(i)%s = trim(line_buffer)
    end do
    close(fh)

end function read_lines


subroutine list_files(dir, files)
    character(len=*), intent(in) :: dir
    type(string_t), allocatable, intent(out) :: files(:)
    integer :: stat
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
    files = read_lines("fpm_ls.out")
end subroutine

logical function exists(filename) result(r)
    character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function

end module FPM_Filesystem