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
public :: LINE_RESET
public :: COLOR_RED, COLOR_GREEN, COLOR_YELLOW, COLOR_RESET

character(len=*), parameter :: ESC = char(27)
!> Escape code for erasing current line
character(len=*), parameter :: LINE_RESET = ESC//"[2K"//ESC//"[1G"
!> Escape code for moving up one line
character(len=*), parameter :: LINE_UP = ESC//"[1A"
!> Escape code for moving down one line
character(len=*), parameter :: LINE_DOWN = ESC//"[1B"
!> Escape code for red foreground color
character(len=*), parameter :: COLOR_RED = ESC//"[31m"
!> Escape code for green foreground color
character(len=*), parameter :: COLOR_GREEN = ESC//"[32m"
!> Escape code for yellow foreground color
character(len=*), parameter :: COLOR_YELLOW = ESC//"[93m"
!> Escape code to reset foreground color
character(len=*), parameter :: COLOR_RESET = ESC//"[0m"

!> Console object
type console_t
    !> Number of lines printed
    integer :: n_line = 1

contains
    !> Write a single line to the console
    procedure :: write_line => console_write_line
    !> Update a previously-written console line
    procedure :: update_line => console_update_line
end type console_t

contains

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
    
    write(stdout,'(A)',advance=trim(adv)) LINE_RESET//str

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

    n = console%n_line - line_no

    ! Step back to line
    write(stdout,'(A)',advance="no") repeat(LINE_UP,n)//LINE_RESET

    write(stdout,'(A)',advance="no") str

    ! Step forward to end
    write(stdout,'(A)',advance="no") repeat(LINE_DOWN,n)//LINE_RESET

    !$omp end critical

end subroutine console_update_line

end module fpm_backend_console