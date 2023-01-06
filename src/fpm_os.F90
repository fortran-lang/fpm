module fpm_os
    use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char, c_ptr, c_associated
    use fpm_error, only : error_t, fatal_error
    implicit none
    private
    public :: change_directory, get_current_directory, get_absolute_path

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

        function getcwd(buf, bufsize) result(path) &
#ifndef _WIN32
                bind(C, name="getcwd")
#else
                bind(C, name="_getcwd")
#endif
            import :: c_char, c_int, c_ptr
            character(kind=c_char, len=1), intent(in) :: buf(*)
            integer(c_int), value, intent(in) :: bufsize
            type(c_ptr) :: path
        end function getcwd
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

    subroutine get_current_directory(path, error)
        character(len=:), allocatable, intent(out) :: path
        type(error_t), allocatable, intent(out) :: error

        character(kind=c_char, len=1), allocatable :: cpath(:)
        integer(c_int), parameter :: buffersize = 1000_c_int
        type(c_ptr) :: tmp

        allocate(cpath(buffersize))

        tmp = getcwd(cpath, buffersize)
        if (c_associated(tmp)) then
            call c_f_character(cpath, path)
        else
            call fatal_error(error, "Failed to retrieve current directory")
        end if

    end subroutine get_current_directory

    subroutine f_c_character(rhs, lhs, len)
        character(kind=c_char), intent(out) :: lhs(*)
        character(len=*), intent(in) :: rhs
        integer, intent(in) :: len
        integer :: length
        length = min(len-1, len_trim(rhs))

        lhs(1:length) = transfer(rhs(1:length), lhs(1:length))
        lhs(length+1:length+1) = c_null_char

    end subroutine f_c_character

    subroutine c_f_character(rhs, lhs)
        character(kind=c_char), intent(in) :: rhs(*)
        character(len=:), allocatable, intent(out) :: lhs

        integer :: ii

        do ii = 1, huge(ii) - 1
            if (rhs(ii) == c_null_char) then
                exit
            end if
        end do
        allocate(character(len=ii-1) :: lhs)
        lhs = transfer(rhs(1:ii-1), lhs)

    end subroutine c_f_character

    !> Determine the absolute from the relative path.
    subroutine get_absolute_path(rel_path, abs_path, error)
        character(len=*), intent(in) :: rel_path
        character(len=:), allocatable, intent(out) :: abs_path
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: start_dir
  
        call get_current_directory(start_dir, error)
        if (allocated(error)) return
        call change_directory(rel_path, error)
        if (allocated(error)) return
        call get_current_directory(abs_path, error)
        if (allocated(error)) return
        call change_directory(start_dir, error)
        if (allocated(error)) return
     end subroutine get_absolute_path

end module fpm_os
