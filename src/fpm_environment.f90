!> This module contains procedures that interact with the programming environment.
!!
!! * [get_os_type] -- Determine the OS type
!! * [get_env] -- return the value of an environment variable
module fpm_environment
    implicit none
    private
    public :: get_os_type
    public :: os_is_unix
    public :: run
    public :: get_env
    public :: get_archiver

    integer, parameter, public :: OS_UNKNOWN = 0
    integer, parameter, public :: OS_LINUX   = 1
    integer, parameter, public :: OS_MACOS   = 2
    integer, parameter, public :: OS_WINDOWS = 3
    integer, parameter, public :: OS_CYGWIN  = 4
    integer, parameter, public :: OS_SOLARIS = 5
    integer, parameter, public :: OS_FREEBSD = 6
    integer, parameter, public :: OS_OPENBSD = 7
contains
    !> Determine the OS type
    integer function get_os_type() result(r)
        !!
        !! Returns one of OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN,
        !! OS_SOLARIS, OS_FREEBSD, OS_OPENBSD.
        !!
        !! At first, the environment variable `OS` is checked, which is usually
        !! found on Windows. Then, `OSTYPE` is read in and compared with common
        !! names. If this fails too, check the existence of files that can be
        !! found on specific system types only.
        !!
        !! Returns OS_UNKNOWN if the operating system cannot be determined.
        character(len=32) :: val
        integer           :: length, rc
        logical           :: file_exists

        r = OS_UNKNOWN

        ! Check environment variable `OS`.
        call get_environment_variable('OS', val, length, rc)

        if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
            r = OS_WINDOWS
            return
        end if

        ! Check environment variable `OSTYPE`.
        call get_environment_variable('OSTYPE', val, length, rc)

        if (rc == 0 .and. length > 0) then
            ! Linux
            if (index(val, 'linux') > 0) then
                r = OS_LINUX
                return
            end if

            ! macOS
            if (index(val, 'darwin') > 0) then
                r = OS_MACOS
                return
            end if

            ! Windows, MSYS, MinGW, Git Bash
            if (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
                r = OS_WINDOWS
                return
            end if

            ! Cygwin
            if (index(val, 'cygwin') > 0) then
                r = OS_CYGWIN
                return
            end if

            ! Solaris, OpenIndiana, ...
            if (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
                r = OS_SOLARIS
                return
            end if

            ! FreeBSD
            if (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
                r = OS_FREEBSD
                return
            end if

            ! OpenBSD
            if (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
                r = OS_OPENBSD
                return
            end if
        end if

        ! Linux
        inquire (file='/etc/os-release', exist=file_exists)

        if (file_exists) then
            r = OS_LINUX
            return
        end if

        ! macOS
        inquire (file='/usr/bin/sw_vers', exist=file_exists)

        if (file_exists) then
            r = OS_MACOS
            return
        end if

        ! FreeBSD
        inquire (file='/bin/freebsd-version', exist=file_exists)

        if (file_exists) then
            r = OS_FREEBSD
            return
        end if
    end function get_os_type

    !> Compare the output of [[get_os_type]] or the optional
    !! passed INTEGER value to the value for OS_WINDOWS
    !! and return .TRUE. if they match and .FALSE. otherwise
    logical function os_is_unix(os) result(unix)
        integer, intent(in), optional :: os
        integer :: build_os
        if (present(os)) then
            build_os = os
        else
            build_os = get_os_type()
        end if
        unix = os /= OS_WINDOWS
    end function os_is_unix

    !> echo command string and pass it to the system for execution
    subroutine run(cmd,echo,exitstat)
        character(len=*), intent(in) :: cmd
        logical,intent(in),optional  :: echo
        integer, intent(out),optional  :: exitstat
        logical :: echo_local
        integer :: stat

        if(present(echo))then
           echo_local=echo
        else
           echo_local=.true.
        endif
        if(echo_local) print *, '+ ', cmd

        call execute_command_line(cmd, exitstat=stat)

        if (present(exitstat)) then
            exitstat = stat
        else
            if (stat /= 0) then
                print *, 'Command failed'
                error stop
            end if
        end if

    end subroutine run

    !> get named environment variable value. It it is blank or
    !! not set return the optional default value
    function get_env(NAME,DEFAULT) result(VALUE)
    implicit none
    !> name of environment variable to get the value of
    character(len=*),intent(in)          :: NAME
    !> default value to return if the requested value is undefined or blank
    character(len=*),intent(in),optional :: DEFAULT
    !> the returned value
    character(len=:),allocatable         :: VALUE
    integer                              :: howbig
    integer                              :: stat
    integer                              :: length
        ! get length required to hold value
        length=0
        if(NAME.ne.'')then
           call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
           select case (stat)
           case (1)
               !*!print *, NAME, " is not defined in the environment. Strange..."
               VALUE=''
           case (2)
               !*!print *, "This processor doesn't support environment variables. Boooh!"
               VALUE=''
           case default
               ! make string to hold value of sufficient size
               allocate(character(len=max(howbig,1)) :: VALUE)
               ! get value
               call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
               if(stat.ne.0)VALUE=''
           end select
        else
           VALUE=''
        endif
        if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
     end function get_env

    function get_archiver() result(archiver)
        character(:), allocatable :: archiver

        associate(os_type => get_os_type())
            if (os_type /= OS_WINDOWS .and. os_type /= OS_UNKNOWN) then
                archiver = "ar -rs "
            else
                block
                    integer :: estat

                    call execute_command_line("ar --version", exitstat=estat)
                    if (estat /= 0) then
                        archiver = "lib /OUT:"
                    else
                        archiver = "ar -rs "
                    end if
                end block
            end if
        end associate
    end function
end module fpm_environment
