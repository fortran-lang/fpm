module fpm_os
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_ptr, c_associated
    use fpm_filesystem, only: exists, join_path, get_home
    use fpm_environment, only: os_is_unix
    use fpm_error, only: error_t, fatal_error
    implicit none
    private
    public :: change_directory, get_current_directory, get_absolute_path, convert_to_absolute_path

    integer(c_int), parameter :: buffersize = 1000_c_int

#ifndef _WIN32
    character(len=*), parameter :: pwd_env = "PWD"
#else
    character(len=*), parameter :: pwd_env = "CD"
#endif

    interface
        function chdir_(path) result(stat) &
#ifndef _WIN32
            bind(C, name="chdir")
#else
            bind(C, name="_chdir")
#endif
            import :: c_char, c_int
            character(kind=c_char, len=1), intent(in) :: path(*)
            integer(c_int) :: stat
        end function chdir_

        function getcwd_(buf, bufsize) result(path) &
#ifndef _WIN32
            bind(C, name="getcwd")
#else
            bind(C, name="_getcwd")
#endif
            import :: c_char, c_int, c_ptr
            character(kind=c_char, len=1), intent(in) :: buf(*)
            integer(c_int), value, intent(in) :: bufsize
            type(c_ptr) :: path
        end function getcwd_

        !> Determine the absolute, canonicalized path for a given path. Unix-only.
        function realpath(path, resolved_path) result(ptr) bind(C)
            import :: c_ptr, c_char, c_int
            character(kind=c_char, len=1), intent(in) :: path(*)
            character(kind=c_char, len=1), intent(out) :: resolved_path(*)
            type(c_ptr) :: ptr
        end function realpath

        !> Determine the absolute, canonicalized path for a given path.
        !> Calls custom C routine and is able to distinguish between Unix and Windows.
        function c_realpath(path, resolved_path, maxLength) result(ptr) &
            bind(C, name="c_realpath")
            import :: c_ptr, c_char, c_int
            character(kind=c_char, len=1), intent(in) :: path(*)
            character(kind=c_char, len=1), intent(out) :: resolved_path(*)
            integer(c_int), value, intent(in) :: maxLength
            type(c_ptr) :: ptr
        end function c_realpath
    end interface

contains

    subroutine change_directory(path, error)
        character(len=*), intent(in) :: path
        type(error_t), allocatable, intent(out) :: error

        character(kind=c_char, len=1), allocatable :: cpath(:)
        integer :: stat

        allocate (cpath(len(path) + 1))
        call f_c_character(path, cpath, len(path) + 1)

        stat = chdir_(cpath)

        if (stat /= 0) then
            call fatal_error(error, "Failed to change directory to '"//path//"'")
        end if
    end subroutine change_directory

    subroutine get_current_directory(path, error)
        character(len=:), allocatable, intent(out) :: path
        type(error_t), allocatable, intent(out) :: error

        character(kind=c_char, len=1), allocatable :: cpath(:)
        type(c_ptr) :: tmp

        allocate (cpath(buffersize))

        tmp = getcwd_(cpath, buffersize)
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
        length = min(len - 1, len_trim(rhs))

        lhs(1:length) = transfer(rhs(1:length), lhs(1:length))
        lhs(length + 1:length + 1) = c_null_char

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

        allocate (character(len=ii - 1) :: lhs)
        lhs = transfer(rhs(1:ii - 1), lhs)

    end subroutine c_f_character

    !> Determine the canonical, absolute path for the given path.
    subroutine get_realpath(path, real_path, error)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: real_path
        type(error_t), allocatable, intent(out) :: error

        character(kind=c_char, len=1), allocatable :: appended_path(:)
        character(kind=c_char, len=1), allocatable :: cpath(:)
        type(c_ptr) :: ptr

        if (.not. exists(path)) then
            call fatal_error(error, "Cannot get real path. Path '"//path//"' does not exist")
            return
        end if

        allocate (appended_path(len(path) + 1))
        call f_c_character(path, appended_path, len(path) + 1)

        allocate (cpath(buffersize))

! Using gfortran, _WIN32 is currently not correctly exported on Windows
#if defined(FPM_BOOTSTRAP) && !defined(_WIN32)
        ptr = realpath(appended_path, cpath)
#else
        ptr = c_realpath(appended_path, cpath, buffersize)
#endif

        if (c_associated(ptr)) then
            call c_f_character(cpath, real_path)
        else
            call fatal_error(error, "Failed to retrieve real path for '"//path//"'")
        end if

    end subroutine get_realpath

    !> Determine the canonical, absolute path for the given path.
    !> Expands home folder (~) on both Unix and Windows.
    subroutine get_absolute_path(path, absolute_path, error)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: absolute_path
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: home

        if (len_trim(path) < 1) then
            ! Empty path
            call fatal_error(error, 'Path cannot be empty')
            return
        else if (path(1:1) == '~') then
            ! Expand home
            call get_home(home, error)
            if (allocated(error)) return

            if (len_trim(path) == 1) then
                absolute_path = home
                return
            end if

            if (os_is_unix()) then
                if (path(2:2) /= '/') then
                    call fatal_error(error, "Wrong separator in path: '"//path//"'")
                    return
                end if
            else
                if (path(2:2) /= '\') then
                    call fatal_error(error, "Wrong separator in path: '"//path//"'")
                    return
                end if
            end if

            if (len_trim(path) == 2) then
                absolute_path = home
                return
            end if

            absolute_path = join_path(home, path(3:len_trim(path)))

            if (.not. exists(absolute_path)) then
                call fatal_error(error, "Path not found: '"//absolute_path//"'")
                deallocate (absolute_path)
                return
            end if
        else
            ! Get canonicalized absolute path from either the absolute or the relative path.
            call get_realpath(path, absolute_path, error)
        end if

    end subroutine

    !> Converts a path to an absolute, canonical path.
    subroutine convert_to_absolute_path(path, error)
        character(len=*), intent(inout) :: path
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: absolute_path

        call get_absolute_path(path, absolute_path, error)
        path = absolute_path
    end subroutine

end module fpm_os
