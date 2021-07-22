!> Implementation of basic error handling.
module fpm_error
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
    use fpm_strings, only : is_fortran_name, to_fortran_name
    implicit none
    private

    public :: error_t
    public :: fatal_error, syntax_error, file_not_found_error
    public :: file_parse_error
    public :: bad_name_error
    public :: fpm_stop


    !> Data type defining an error
    type :: error_t

        !> Error message
        character(len=:), allocatable :: message

    end type error_t

contains

    !> Generic fatal runtime error
    subroutine fatal_error(error, message)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Error message
        character(len=*), intent(in) :: message

        allocate(error)
        error%message = message

    end subroutine fatal_error

    subroutine syntax_error(error, message)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Error message
        character(len=*), intent(in) :: message

        allocate(error)
        error%message = message

    end subroutine syntax_error

    function bad_name_error(error, label,name)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Error message label to add to message
        character(len=*), intent(in) :: label

        !> name value to check
        character(len=*), intent(in) :: name

        logical :: bad_name_error

        if(.not.is_fortran_name(to_fortran_name(name)))then
           bad_name_error=.true.
           allocate(error)
           error%message = 'manifest file syntax error: '//label//' name must be composed only of &
           &alphanumerics, "-" and "_"  and start with a letter ::'//name
        else
          bad_name_error=.false.
        endif

    end function bad_name_error


    !> Error created when a file is missing or not found
    subroutine file_not_found_error(error, file_name)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Name of the missing file
        character(len=*), intent(in) :: file_name

        allocate(error)
        error%message = "'"//file_name//"' could not be found, check if the file exists"

    end subroutine file_not_found_error


    !> Error created when file parsing fails
    subroutine file_parse_error(error, file_name, message, line_num, &
                                 line_string, line_col)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Name of file
        character(len=*), intent(in) :: file_name

        !> Parse error message
        character(len=*), intent(in) :: message

        !> Line number of parse error
        integer, intent(in), optional :: line_num

        !> Line context string
        character(len=*), intent(in), optional :: line_string

        !> Line context column
        integer, intent(in), optional :: line_col

        character(50) :: temp_string

        allocate(error)
        error%message = 'Parse error: '//message//new_line('a')

        error%message = error%message//file_name

        if (present(line_num)) then

            write(temp_string,'(I0)') line_num

            error%message = error%message//':'//trim(temp_string)

        end if

        if (present(line_col)) then

            if (line_col > 0) then

                write(temp_string,'(I0)') line_col
                error%message = error%message//':'//trim(temp_string)

            end if

        end if

        if (present(line_string)) then

            error%message = error%message//new_line('a')
            error%message = error%message//'   | '//line_string

            if (present(line_col)) then

                if (line_col > 0) then

                    error%message = error%message//new_line('a')
                    error%message = error%message//'   | '//repeat(' ',line_col-1)//'^'

                end if

            end if

        end if

    end subroutine file_parse_error

    subroutine fpm_stop(value,message)
    ! TODO: if verbose mode, call ERROR STOP instead of STOP
    ! TODO: if M_escape is used, add color
    ! to work with older compilers might need a case statement for values

        !> value to use on STOP
        integer, intent(in) :: value
        !> Error message
        character(len=*), intent(in) :: message
        if(message.ne.'')then
           if(value.gt.0)then
              write(stderr,'("<ERROR>",a)')trim(message)
           else
              write(stderr,'("<INFO> ",a)')trim(message)
           endif
        endif
        stop value
    end subroutine fpm_stop

end module fpm_error
