!> This module contains procedures that interact with the programming environment.
!!
!! * [get_os_type] -- Determine the OS type
!! * [get_env] -- return the value of an environment variable
module fpm_environment
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                           & stdout=>output_unit, &
                                           & stderr=>error_unit
    use,intrinsic :: iso_c_binding, only: c_char,c_int,c_null_char
    use fpm_error, only : fpm_stop
    implicit none
    private
    public :: get_os_type
    public :: os_is_unix
    public :: get_env
    public :: set_env
    public :: delete_env
    public :: get_command_arguments_quoted
    public :: separator
    

                        public :: OS_NAME
    integer, parameter, public :: OS_UNKNOWN = 0
    integer, parameter, public :: OS_LINUX   = 1
    integer, parameter, public :: OS_MACOS   = 2
    integer, parameter, public :: OS_WINDOWS = 3
    integer, parameter, public :: OS_CYGWIN  = 4
    integer, parameter, public :: OS_SOLARIS = 5
    integer, parameter, public :: OS_FREEBSD = 6
    integer, parameter, public :: OS_OPENBSD = 7
contains

    !> Return string describing the OS type flag
    pure function OS_NAME(os)
        integer, intent(in) :: os
        character(len=:), allocatable :: OS_NAME

        select case (os)
            case (OS_LINUX);   OS_NAME =  "Linux"
            case (OS_MACOS);   OS_NAME =  "macOS"
            case (OS_WINDOWS); OS_NAME =  "Windows"
            case (OS_CYGWIN);  OS_NAME =  "Cygwin"
            case (OS_SOLARIS); OS_NAME =  "Solaris"
            case (OS_FREEBSD); OS_NAME =  "FreeBSD"
            case (OS_OPENBSD); OS_NAME =  "OpenBSD"
            case (OS_UNKNOWN); OS_NAME =  "Unknown"
            case default     ; OS_NAME =  "UNKNOWN"
        end select
    end function OS_NAME

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
        character(len=255) :: val
        integer            :: length, rc
        logical            :: file_exists
        logical, save      :: first_run = .true.
        integer, save      :: ret = OS_UNKNOWN
        !$omp threadprivate(ret, first_run)

        if (.not. first_run) then
            r = ret
            return
        end if

        first_run = .false.
        r = OS_UNKNOWN

        ! Check environment variable `OSTYPE`.
        call get_environment_variable('OSTYPE', val, length, rc)

        if (rc == 0 .and. length > 0) then
            ! Linux
            if (index(val, 'linux') > 0) then
                r = OS_LINUX
                ret = r
                return
            end if

            ! macOS
            if (index(val, 'darwin') > 0) then
                r = OS_MACOS
                ret = r
                return
            end if

            ! Windows, MSYS, MinGW, Git Bash
            if (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
                r = OS_WINDOWS
                ret = r
                return
            end if

            ! Cygwin
            if (index(val, 'cygwin') > 0) then
                r = OS_CYGWIN
                ret = r
                return
            end if

            ! Solaris, OpenIndiana, ...
            if (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
                r = OS_SOLARIS
                ret = r
                return
            end if

            ! FreeBSD
            if (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
                r = OS_FREEBSD
                ret = r
                return
            end if

            ! OpenBSD
            if (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
                r = OS_OPENBSD
                ret = r
                return
            end if
        end if

        ! Check environment variable `OS`.
        call get_environment_variable('OS', val, length, rc)

        if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
            r = OS_WINDOWS
            ret = r
            return
        end if

        ! Linux
        inquire (file='/etc/os-release', exist=file_exists)

        if (file_exists) then
            r = OS_LINUX
            ret = r
            return
        end if

        ! macOS
        inquire (file='/usr/bin/sw_vers', exist=file_exists)

        if (file_exists) then
            r = OS_MACOS
            ret = r
            return
        end if

        ! FreeBSD
        inquire (file='/bin/freebsd-version', exist=file_exists)

        if (file_exists) then
            r = OS_FREEBSD
            ret = r
            return
        end if
    end function get_os_type

    !> Compare the output of [[get_os_type]] or the optional
    !! passed INTEGER value to the value for OS_WINDOWS
    !! and return .TRUE. if they match and .FALSE. otherwise
    logical function os_is_unix(os)
        integer, intent(in), optional :: os
        integer :: build_os
        if (present(os)) then
            build_os = os
        else
            build_os = get_os_type()
        end if
        os_is_unix = build_os /= OS_WINDOWS
    end function os_is_unix

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
        if(NAME/='')then
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
               if(stat/=0)VALUE=''
           end select
        else
           VALUE=''
        endif
        if(VALUE==''.and.present(DEFAULT))VALUE=DEFAULT
     end function get_env

    function get_command_arguments_quoted() result(args)
    character(len=:),allocatable :: args
    character(len=:),allocatable :: arg
    character(len=1)             :: quote
    integer                      :: ilength, istatus, i
    ilength=0
    args=''
        quote=merge('"',"'",separator()=='\')
        do i=2,command_argument_count() ! look at all arguments after subcommand
            call get_command_argument(number=i,length=ilength,status=istatus)
            if(istatus /= 0) then
                write(stderr,'(*(g0,1x))')'<ERROR>*get_command_arguments_stack* error obtaining argument ',i
                exit
            else
                if(allocated(arg))deallocate(arg)
                allocate(character(len=ilength) :: arg)
                call get_command_argument(number=i,value=arg,length=ilength,status=istatus)
                if(istatus /= 0) then
                    write(stderr,'(*(g0,1x))')'<ERROR>*get_command_arguments_stack* error obtaining argument ',i
                    exit
                elseif(ilength>0)then
                    if(index(arg//' ','-')/=1)then
                        args=args//quote//arg//quote//' '
                    elseif(index(arg,' ')/=0)then
                        args=args//quote//arg//quote//' '
                    else
                        args=args//arg//' '
                    endif
                else
                    args=args//repeat(quote,2)//' '
                endif
             endif
         enddo
    end function get_command_arguments_quoted

function separator() result(sep)
!>
!!##NAME
!!    separator(3f) - [M_io:ENVIRONMENT] try to determine pathname directory separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!    First using the name the program was invoked with, then the name
!!    returned by an INQUIRE(3f) of that name, then ".\NAME" and "./NAME"
!!    try to determine the separator character used to separate directory
!!    names from file basenames.
!!
!!    If a slash or backslash is not found in the name, the environment
!!    variable PATH is examined first for a backslash, then a slash.
!!
!!    Can be very system dependent. If the queries fail the default returned
!!    is "/".
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator

! use the pathname returned as arg0 to determine pathname separator
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
logical                      :: existing
character(len=1)             :: sep
!*ifort_bug*!character(len=1),save        :: sep_cache=' '
character(len=4096)          :: name
character(len=:),allocatable :: fname

   !*ifort_bug*!   if(sep_cache/=' ')then  ! use cached value. NOTE:  A parallel code might theoretically use multiple OS
   !*ifort_bug*!      sep=sep_cache
   !*ifort_bug*!      return
   !*ifort_bug*!   endif

   arg0_length=0
   name=' '
   call get_command_argument(0,length=arg0_length,status=istat)
   if(allocated(arg0))deallocate(arg0)
   allocate(character(len=arg0_length) :: arg0)
   call get_command_argument(0,arg0,status=istat)
   ! check argument name
   if(index(arg0,'\')/=0)then
      sep='\'
   elseif(index(arg0,'/')/=0)then
      sep='/'
   else
      ! try name returned by INQUIRE(3f)
      existing=.false.
      name=' '
      inquire(file=arg0,iostat=istat,exist=existing,name=name)
      if(index(name,'\')/=0)then
         sep='\'
      elseif(index(name,'/')/=0)then
         sep='/'
      else
         ! well, try some common syntax and assume in current directory
         fname='.\'//arg0
         inquire(file=fname,iostat=istat,exist=existing)
         if(existing)then
            sep='\'
         else
            fname='./'//arg0
            inquire(file=fname,iostat=istat,exist=existing)
            if(existing)then
               sep='/'
            else ! check environment variable PATH
               sep=merge('\','/',index(get_env('PATH'),'\')/=0)
               !*!write(*,*)'<WARNING>unknown system directory path separator'
            endif
         endif
      endif
   endif
   !*ifort_bug*!sep_cache=sep
end function separator

!> Set an environment variable for the current environment using the C standard library
logical function set_env(name,value,overwrite)

   !> Variable name
   character(*), intent(in) :: name
   
   !> Variable value
   character(*), intent(in) :: value
   
   !> Should a former value be overwritten? default = .true.
   logical, optional, intent(in) :: overwrite
   
   ! Local variables
   logical :: can_overwrite
   integer(c_int) :: cover,cerr
   character(kind=c_char,len=1), allocatable :: c_value(:),c_name(:)
   
   interface
      integer(c_int) function c_setenv(envname, envval, overwrite) &
                     bind(C,name="c_setenv")
         import c_int, c_char
         implicit none
         !> Pointer to the name string
         character(kind=c_char,len=1), intent(in) :: envname(*)
         !> Pointer to the value string 
         character(kind=c_char,len=1), intent(in) :: envval(*)
         !> Overwrite option
         integer(c_int), intent(in), value :: overwrite
      end function c_setenv 
   end interface
   
   !> Overwrite setting
   can_overwrite = .true.
   if (present(overwrite)) can_overwrite = overwrite
   cover = merge(1_c_int,0_c_int,can_overwrite)
   
   !> C strings
   call f2cs(name,c_name)
   call f2cs(value,c_value)
   
   !> Call setenv
#ifndef FPM_BOOTSTRAP   
   cerr = c_setenv(c_name,c_value,cover)
#endif
   set_env = cerr==0_c_int
   
end function set_env

!> Deletes an environment variable for the current environment using the C standard library
!> Returns an error if the variable did not exist in the first place
logical function delete_env(name) result(success)

   !> Variable name
   character(*), intent(in) :: name
   
   ! Local variables
   integer(c_int) :: cerr
   character(kind=c_char,len=1), allocatable :: c_name(:)
   
   interface
      integer(c_int) function c_unsetenv(envname) bind(C,name="c_unsetenv")
         import c_int, c_char
         implicit none
         !> Pointer to the name string
         character(kind=c_char,len=1), intent(in) :: envname(*)
      end function c_unsetenv      
   end interface
   
   !> C strings
   call f2cs(name,c_name)
   
   !> Call setenv
#ifndef FPM_BOOTSTRAP   
   cerr = c_unsetenv(c_name)
#endif
   success = cerr==0_c_int
   
end function delete_env

!> Fortran to C allocatable string
pure subroutine f2cs(f,c)
  use iso_c_binding, only: c_char,c_null_char
  character(*), intent(in) :: f
  character(len=1,kind=c_char), allocatable, intent(out) :: c(:)
  
  integer :: lf,i
  
  lf = len(f)
  allocate(c(lf+1))
  c(lf+1) = c_null_char 
  forall(i=1:lf) c(i) = f(i:i)
  
end subroutine f2cs 

end module fpm_environment
