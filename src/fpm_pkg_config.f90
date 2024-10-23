!># The fpm interface to pkg-config
!>
!> This module contains wrapper functions to interface with a pkg-config installation.
!>
module fpm_pkg_config

use fpm_strings, only: string_t,str_begins_with_str,len_trim,remove_newline_characters, &
    split
use fpm_error, only: error_t, fatal_error, fpm_stop
use fpm_filesystem, only: get_temp_filename,getline
use fpm_environment, only: get_env,os_is_unix,set_env,delete_env
use shlex_module, only: shlex_split => split
implicit none
private

public :: assert_pkg_config
public :: pkgcfg_get_version
public :: pkgcfg_get_libs
public :: pkgcfg_get_build_flags
public :: pkgcfg_has_package
public :: pkgcfg_list_all
public :: run_wrapper

contains
    
!> Check whether pkg-config is available on the local system
logical function assert_pkg_config()

   integer :: exitcode
   logical :: success
   type(string_t) :: log

   call run_wrapper(wrapper=string_t('pkg-config'),args=[string_t('-h')], &
                    exitcode=exitcode,cmd_success=success,screen_output=log)
   
   assert_pkg_config = exitcode==0 .and. success 
    
end function assert_pkg_config
    
!> Get package version from pkg-config
type(string_t) function pkgcfg_get_version(package,error) result(screen)

    !> Package name
    character(*), intent(in) :: package
    
    !> Error handler
    type(error_t), allocatable, intent(out) :: error

    integer :: exitcode
    logical :: success
    type(string_t) :: log    
        
    call run_wrapper(wrapper=string_t('pkg-config'), &
                     args=[string_t(package),string_t('--modversion')], &
                     exitcode=exitcode,cmd_success=success,screen_output=log)    
    
    if (success .and. exitcode==0) then 
        call remove_newline_characters(log)
        screen = log
    else
        screen = string_t("")
    end if      

end function pkgcfg_get_version

!> Check if pkgcfg has package
logical function pkgcfg_has_package(name) result(success)

    !> Package name
    character(*), intent(in) :: name
    
    integer :: exitcode
    logical :: cmdok
    type(string_t) :: log    
        
    call run_wrapper(wrapper=string_t('pkg-config'), &
                     args=[string_t(name),string_t('--exists')], &
                     exitcode=exitcode,cmd_success=cmdok,screen_output=log)    
    
    !> pkg-config --exists returns 0 only if the package exists
    success = cmdok .and. exitcode==0
            
end function pkgcfg_has_package
    

!> Get package libraries from pkg-config
function pkgcfg_get_libs(package,error) result(libraries)

    !> Package name
    character(*), intent(in) :: package
    
    !> Error handler
    type(error_t), allocatable, intent(out) :: error
    
    !> A list of libraries
    type(string_t), allocatable :: libraries(:)

    integer :: exitcode,nlib,i
    logical :: success
    character(len=:), allocatable :: tokens(:)
    type(string_t) :: log    
        
    call run_wrapper(wrapper=string_t('pkg-config'), &
                     args=[string_t(package),string_t('--libs')], &
                     exitcode=exitcode,cmd_success=success,screen_output=log)         

    if (success .and. exitcode==0) then 
        
        call remove_newline_characters(log)
        
        ! Split all arguments
        tokens = shlex_split(log%s)
        
        nlib = size(tokens)
        allocate(libraries(nlib))
        do i=1,nlib
            libraries(i) = string_t(trim(adjustl(tokens(i))))
        end do
        
    else
        
        allocate(libraries(0))
        call fatal_error(error,'cannot get <'//package//'> libraries from pkg-config')
        
    end if   

end function pkgcfg_get_libs

!> Return whole list of available pkg-cfg packages
function pkgcfg_list_all(error,descriptions) result(modules)
    
    !> Error handler
    type(error_t), allocatable, intent(out) :: error
    
    !> A list of all available packages 
    type(string_t), allocatable :: modules(:)    
    
    !> An optional list of package descriptions
    type(string_t), optional, allocatable, intent(out) :: descriptions(:)
    
    integer :: exitcode,i,spc
    logical :: success
    character(len=:), allocatable :: lines(:)
    type(string_t) :: log    
    type(string_t), allocatable :: mods(:),descr(:)
    character(*), parameter :: CRLF = achar(13)//new_line('a')
        
    call run_wrapper(wrapper=string_t('pkg-config'), &
                     args=[string_t('--list-all')], &
                     exitcode=exitcode,cmd_success=success,screen_output=log) 
                     
    if (.not.(success .and. exitcode==0)) then 
        call fatal_error(error,'cannot get pkg-config modules')
        allocate(modules(0))
        return
    end if
                    
    !> Extract list 
    call split(log%s,lines,CRLF)
    allocate(mods(size(lines)),descr(size(lines)))
    
    do i=1,size(lines)
        
        ! Module names have no spaces
        spc = index(lines(i),' ')
        
        if (spc>0) then 
            
            mods(i)  = string_t(trim(adjustl(lines(i)(1:spc))))
            descr(i) = string_t(trim(adjustl(lines(i)(spc+1:))))
            
        else
            
            mods(i)  = string_t(trim(adjustl(lines(i))))
            descr(i) = string_t("")
            
        end if
        
    end do
    
    call move_alloc(from=mods,to=modules)
    if (present(descriptions)) call move_alloc(from=descr,to=descriptions)
    
end function pkgcfg_list_all
    
!> Get build flags (option to include flags from system directories, that 
!> gfortran does not look into by default)
function pkgcfg_get_build_flags(name,allow_system,error) result(flags)
    
    !> Package name
    character(*), intent(in) :: name
    
    !> Should pkg-config look in system paths? This is necessary for gfortran 
    !> that doesn't otherwise look into them
    logical, intent(in) :: allow_system 
    
    !> Error flag 
    type(error_t), allocatable, intent(out) :: error
    
    !> List of compile flags
    type(string_t), allocatable :: flags(:)
    
    integer :: exitcode,i,nlib
    logical :: old_had,success,old_allow
    character(:), allocatable :: old,tokens(:)
    type(string_t) :: log    
    
    ! Check if the current environment includes system flags
    old = get_env('PKG_CONFIG_ALLOW_SYSTEM_CFLAGS',default='ERROR')
    old_had = old/='ERROR'
    old_allow = merge(old=='1',.false.,old_had)
    
    ! Set system flags
    success = set_env('PKG_CONFIG_ALLOW_SYSTEM_CFLAGS',value=merge('1','0',allow_system))
    if (.not.success) then 
        call fatal_error(error,'Cannot get pkg-config build flags: environment variable error.')
        return
    end if
    
    ! Now run wrapper
    call run_wrapper(wrapper=string_t('pkg-config'), &
                     args=[string_t(name),string_t('--cflags')], &
                     exitcode=exitcode,cmd_success=success,screen_output=log) 
                     
    if (success .and. exitcode==0) then 
        
        call remove_newline_characters(log)
        
        ! Split all arguments
        tokens = shlex_split(log%s)
        
        nlib = size(tokens)
        allocate(flags(nlib))
        do i=1,nlib
            flags(i) = string_t(trim(adjustl(tokens(i))))
        end do
        
    else
        
        allocate(flags(0))
        call fatal_error(error,'cannot get <'//name//'> build flags from pkg-config')
        
    end if   

    ! Restore environment variable
    if (old_had) then 
        success = set_env('PKG_CONFIG_ALLOW_SYSTEM_CFLAGS',value=old)
    else
        success = delete_env('PKG_CONFIG_ALLOW_SYSTEM_CFLAGS')
    end if
    if (.not.success) then 
        call fatal_error(error,'Cannot get pkg-config build flags: environment variable error.')
        return
    end if    
    
    
end function pkgcfg_get_build_flags
    
!> Simple call to execute_command_line involving one mpi* wrapper
subroutine run_wrapper(wrapper,args,verbose,exitcode,cmd_success,screen_output)
    type(string_t), intent(in) :: wrapper
    type(string_t), intent(in), optional :: args(:)
    logical, intent(in), optional :: verbose
    integer, intent(out), optional :: exitcode
    logical, intent(out), optional :: cmd_success
    type(string_t), intent(out), optional :: screen_output

    logical :: echo_local
    character(:), allocatable :: redirect_str,command,redirect,line
    integer :: iunit,iarg,stat,cmdstat


    if(present(verbose))then
       echo_local=verbose
    else
       echo_local=.false.
    end if

    ! No redirection and non-verbose output
    if (present(screen_output)) then
        redirect = get_temp_filename()
        redirect_str =  ">"//redirect//" 2>&1"
    else
        if (os_is_unix()) then
            redirect_str = " >/dev/null 2>&1"
        else
            redirect_str = " >NUL 2>&1"
        end if
    end if

    ! Empty command
    if (len_trim(wrapper)<=0) then
        if (echo_local) print *, '+ <EMPTY COMMAND>'
        if (present(exitcode)) exitcode = 0
        if (present(cmd_success)) cmd_success = .true.
        if (present(screen_output)) screen_output = string_t("")
        return
    end if

    ! Init command
    command = trim(wrapper%s)

    add_arguments: if (present(args)) then
        do iarg=1,size(args)
            if (len_trim(args(iarg))<=0) cycle
            command = trim(command)//' '//args(iarg)%s
        end do
    endif add_arguments

    if (echo_local) print *, '+ ', command

    ! Test command
    call execute_command_line(command//redirect_str,exitstat=stat,cmdstat=cmdstat)

    ! Command successful?
    if (present(cmd_success)) cmd_success = cmdstat==0

    ! Program exit code?
    if (present(exitcode)) exitcode = stat

    ! Want screen output?
    if (present(screen_output) .and. cmdstat==0) then

        allocate(character(len=0) :: screen_output%s)

        open(newunit=iunit,file=redirect,status='old',iostat=stat)
        if (stat == 0)then
           do
               call getline(iunit, line, stat)
               if (stat /= 0) exit

               screen_output%s = screen_output%s//new_line('a')//line

               if (echo_local) write(*,'(A)') trim(line)
           end do

           ! Close and delete file
           close(iunit,status='delete')

        else
           call fpm_stop(1,'cannot read temporary file from successful MPI wrapper')
        endif

    end if

end subroutine run_wrapper
    
end module fpm_pkg_config
