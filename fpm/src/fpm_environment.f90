module fpm_environment
    implicit none
    private

    integer, parameter, public :: OS_LINUX = 1
    integer, parameter, public :: OS_MACOS = 2
    integer, parameter, public :: OS_WINDOWS = 3

    public :: get_os_type, run
contains
    integer function get_os_type() result(r)
    ! Determine the OS type
    !
    ! Returns one of OS_LINUX, OS_MACOS, OS_WINDOWS.
    !
    ! Currently we use the $HOME and $HOMEPATH environment variables to determine
    ! the OS type. That is not 100% accurate in all cases, but it seems to be good
    ! enough for now. See the following issue for a more robust solution:
    !
    ! https://github.com/fortran-lang/fpm/issues/144
    !
    character(len=100) :: val
    integer stat
    ! Only Windows define $HOMEPATH by default and we test its value to improve the
    ! chances of it working even if a user defines $HOMEPATH on Linux or macOS.
    call get_environment_variable("HOMEPATH", val, status=stat)
    if (stat == 0 .and. val(1:7) == "\Users\") then
        r = OS_WINDOWS
        return
    end if

    ! We assume that $HOME=/home/... is Linux, $HOME=/Users/... is macOS, otherwise
    ! we assume Linux. This is only a heuristic and can easily fail.
    call get_environment_variable("HOME", val, status=stat)
    if (stat == 1) then
        print *, "$HOME does not exist"
        error stop
    end if
    if (stat /= 0) then
        print *, "get_environment_variable() failed"
        error stop
    end if
    if (val(1:6) == "/home/") then
        r = OS_LINUX
    else if (val(1:7) == "/Users/") then
        r = OS_MACOS
    else
        ! This will happen on HPC systems that typically do not use either /home nor
        ! /Users for $HOME. Those systems are typically Linux, so for now we simply
        ! set Linux here.
        r = OS_LINUX
    end if
    end function

    subroutine run(cmd)
        character(len=*), intent(in) :: cmd
        integer :: stat
        print *, "+ ", cmd
        call execute_command_line(cmd, exitstat=stat)
        if (stat /= 0) then
            print *, "Command failed"
            error stop
        end if
    end subroutine run

end module fpm_environment
