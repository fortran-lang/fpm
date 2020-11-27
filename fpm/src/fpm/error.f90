!> Implementation of basic error handling.
module fpm_error
    implicit none
    private

    public :: error_t
    public :: fatal_error, syntax_error, file_not_found_error
    public :: file_parse_error


    !> Data type defining an error
    type :: error_t

        !> Error message
        character(len=:), allocatable :: message

    end type error_t


    !> Alias syntax errors to fatal errors for now
    interface syntax_error
        module procedure :: fatal_error
    end interface syntax_error


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


end module fpm_error
