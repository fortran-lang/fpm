!># Build Backend Console
!> This module provides a lightweight implementation for printing to the console
!> and updating previously-printed console lines. It used by `[[fpm_backend_output]]`
!> for pretty-printing build status and progress.
!>
!> @note The implementation for updating previous lines relies on no other output
!> going to `stdout`/`stderr` except through the `console_t` object provided.
!>
!> @note All write statements to `stdout` are enclosed within OpenMP `critical` regions
!>
module fpm_backend_console
use iso_fortran_env, only: stdout=>output_unit
implicit none

private
public :: console_t

character(len=*), parameter :: ESC = char(27)

!> Console object
type console_t
    !> Number of lines printed
    integer :: n_line = 1
    !> 'Plain' output (no escape codes)
    logical :: plain_mode = .false.
    !> Escape code for erasing current line
    character(:), allocatable :: LINE_RESET
    !> Escape code for moving up one line
    character(:), allocatable :: LINE_UP
    !> Escape code for moving down one line
    character(:), allocatable :: LINE_DOWN
contains
    !> Write a single line to the console
    procedure :: write_line => console_write_line
    !> Update a previously-written console line
    procedure :: update_line => console_update_line
end type console_t

!> Constructor for console_t
interface console_t
    procedure :: new_console
end interface console_t

contains

!> Initialise a new console object
function new_console(plain_mode) result(console)
    !> 'Plain' output (no escape codes)
    logical, intent(in), optional :: plain_mode
    !> Console object to initialise
    type(console_t) :: console

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

end function new_console

!> Write a single line to the standard output
subroutine console_write_line(console,str,line,advance)
    !> Console object
    class(console_t), intent(inout) :: console
    !> String to write
    character(*), intent(in) :: str
    !> Integer needed to later update console line
    integer, intent(out), optional :: line
    !> Advancing output (print newline?)
    logical, intent(in), optional :: advance

    character(3) :: adv

    adv = "yes"
    if (present(advance)) then
        if (.not.advance) then
            adv = "no"
        end if
    end if

    !$omp critical

    if (present(line)) then
        line = console%n_line
    end if
    
    write(stdout,'(A)',advance=trim(adv)) console%LINE_RESET//str

    if (adv=="yes") then
        console%n_line = console%n_line + 1
    end if

    !$omp end critical

end subroutine console_write_line

!> Overwrite a previously-written line in standard output
subroutine console_update_line(console,line_no,str)
    !> Console object
    class(console_t), intent(in) :: console
    !> Integer output from `[[console_write_line]]`
    integer, intent(in) :: line_no
    !> New string to overwrite line
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