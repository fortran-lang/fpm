module fpm_backend_console
use iso_fortran_env, only: stdout=>output_unit
implicit none

private
public :: console_t

character(len=*), parameter :: ESC = char(27)

type console_t
    integer :: n_line = 1
    logical :: plain_mode = .false.
    character(:), allocatable :: LINE_RESET
    character(:), allocatable :: LINE_UP
    character(:), allocatable :: LINE_DOWN
contains
    procedure :: init => console_init
    procedure :: write_line => console_write_line
    procedure :: update_line => console_update_line
end type console_t

contains

subroutine console_init(console,plain_mode)
    class(console_t), intent(out), target :: console
    logical, intent(in), optional :: plain_mode

    if (present(plain_mode)) then
        console%plain_mode = plain_mode
    end if

    if (console%plain_mode) then
        console%LINE_RESET = ""
        console%LINE_UP = ""
        console%LINE_DOWN = ""
    else
        console%LINE_RESET = ESC//"[2K"//ESC//"[1G"
        console%LINE_UP = ESC//"[1A"
        console%LINE_DOWN = ESC//"[1B"
    end if

end subroutine console_init

function console_write_line(console,str) result(line)
    class(console_t), intent(inout), target :: console
    character(*), intent(in) :: str
    integer :: line

    !$omp critical
    line = console%n_line

    write(stdout,*) console%LINE_RESET//str

    console%n_line = console%n_line + 1
    !$omp end critical

end function console_write_line

subroutine console_update_line(console,line_no,str)
    class(console_t), intent(in) :: console
    integer, intent(in) :: line_no
    character(*), intent(in) :: str

    integer :: n

    !$omp critical

    n = console%n_line - line_no !+ 1 !+ 1

    ! Step back to line
    write(stdout,'(A)',advance="no") repeat(console%LINE_UP,n)//console%LINE_RESET

    write(stdout,*) str

    ! Step forward to end
    write(stdout,'(A)',advance="no") repeat(console%LINE_DOWN,n)//console%LINE_RESET

    !$omp end critical

end subroutine console_update_line

end module fpm_backend_console