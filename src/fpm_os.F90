module fpm_os
    use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char
    use fpm_error, only : error_t, fatal_error
    implicit none
    private
    public :: change_directory, get_current_directory

#ifndef _WIN32
    character(len=*), parameter :: pwd_env = "PWD"
#else
    character(len=*), parameter :: pwd_env = "CD"
#endif

    interface
        function chdir(path) result(stat) &
#ifndef _WIN32
                bind(C, name="chdir")
#else
                bind(C, name="_chdir")
#endif
            import :: c_char, c_int
            character(kind=c_char, len=1), intent(in) :: path(*)
            integer(c_int) :: stat
        end function chdir
    end interface

contains

    subroutine change_directory(path, error)
        character(len=*), intent(in) :: path
        type(error_t), allocatable, intent(out) :: error

        character(kind=c_char, len=1), allocatable :: cpath(:)
        integer :: stat

        allocate(cpath(len(path)+1))
        call f_c_character(path, cpath, len(path)+1)

        stat = chdir(cpath)

        if (stat /= 0) then
            call fatal_error(error, "Failed to change directory to '"//path//"'")
        end if
    end subroutine change_directory

    subroutine f_c_character(rhs, lhs, len)
        character(kind=c_char), intent(out) :: lhs(*)
        character(len=*), intent(in) :: rhs
        integer, intent(in) :: len
        integer :: length
        length = min(len-1, len_trim(rhs))

        lhs(1:length) = transfer(rhs(1:length), lhs(1:length))
        lhs(length+1:length+1) = c_null_char

    end subroutine f_c_character

    subroutine get_current_directory(path)
        character(len=:), allocatable, intent(out) :: path

        integer :: length, stat

        call get_environment_variable(pwd_env, length=length, status=stat)
        if (stat /= 0) return

        allocate(character(len=length) :: path, stat=stat)
        if (stat /= 0) return

        if (length > 0) then
            call get_environment_variable(pwd_env, path, status=stat)
            if (stat /= 0) then
                deallocate(path)
                return
            end if
        end if

    end subroutine get_current_directory

end module fpm_os
