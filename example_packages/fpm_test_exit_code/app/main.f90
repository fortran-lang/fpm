! Test program for application exit codes
! see https://github.com/fortran-lang/fpm/issues/848

! This app expects to receive an integer command line argument, to check whether it is odd or even.
! It returns 0 on success (odd input), or among a few error codes otherwise.

program check_odd_number
    implicit none

    integer, parameter :: SUCCESS          = 0
    integer, parameter :: INVALID_ARGUMENT = 1
    integer, parameter :: NOT_AN_INTEGER   = 2
    integer, parameter :: NOT_ODD          = 3

    character(len=1024) :: buffer
    integer :: ierr,ln,the_number

    ! If the argument is missing or not an integer, return an error flag
    if (command_argument_count()/=1) stop INVALID_ARGUMENT

    ! Get command argument
    call get_command_argument(1,value=buffer,length=ln,status=ierr)

    ! On invalid string
    if (ln<1 .or. ierr/=0) stop INVALID_ARGUMENT

    ! Read to int
    read(buffer(:ln),*,iostat=ierr) the_number

    ! On invalid integer
    if (ierr/=0) stop NOT_AN_INTEGER

    ! Check if it is odd or even
    if (mod(the_number,2)==0) then
        ! Is even
        stop NOT_ODD
    else
        ! Is odd
        stop SUCCESS
    end if

end program check_odd_number
