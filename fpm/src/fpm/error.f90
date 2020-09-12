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
    subroutine file_parse_error(error, file_name, line, message)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Name of file
        character(len=*), intent(in) :: file_name

        !> Line number of parse error
        integer, intent(in) :: line

        !> Parse error message
        character(len=*), intent(in) :: message

        character(50) :: line_no_string

        write(line_no_string,'(I0)') line

        allocate(error)
        error%message = 'Error while parsing file "'//file_name//'" on line '// &
                        trim(line_no_string)//': '//message

    end subroutine file_parse_error


end module fpm_error
