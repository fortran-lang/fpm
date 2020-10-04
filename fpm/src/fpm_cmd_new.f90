module fpm_cmd_new

use fpm_command_line, only : fpm_new_settings
use fpm_environment, only : run, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only : join_path, exists, basename, mkdir
use,intrinsic :: iso_fortran_env, only : stderr=>error_unit
implicit none
private
public :: cmd_new

contains

subroutine cmd_new(settings) ! --with-executable F --with-test F '
type(fpm_new_settings), intent(in) :: settings
character(len=:),allocatable :: bname          ! baeename of NAME
character(len=:),allocatable :: message(:)
character(len=:),allocatable :: littlefile(:)

    call mkdir(settings%name)      ! make new directory
    call run('cd '//settings%name) ! change to new directory as a test. System dependent potentially
    !! NOTE: need some system routines to handle filenames like "." like realpath() or getcwd().
    bname=basename(settings%name)

    !! weird gfortran bug?? lines truncated to concatenated string length, not 80
    !! hit some weird gfortran bug when littlefile data was an argument to warnwrite(3f), ok when a variable

    call warnwrite(join_path(settings%name, '.gitignore'), ['build/*'])        ! create NAME/.gitignore file

    littlefile=[character(len=80) :: '# '//bname, 'My cool new project!']

    call warnwrite(join_path(settings%name, 'README.md'), littlefile)          ! create NAME/README.md

    message=[character(len=80) ::             &                                ! start building NAME/fpm.toml
    &'name = "'//bname//'"                 ', &
    &'version = "0.1.0"                    ', &
    &'license = "license"                  ', &
    &'author = "Jane Doe"                  ', &
    &'maintainer = "jane.doe@example.com"  ', &
    &'copyright = "2020 Jane Doe"          ', &
    &'                                     ', &
    &'']

    if(settings%with_lib)then
        call mkdir(join_path(settings%name,'src') )
        message=[character(len=80) ::  message,   &                             ! create next section of fpm.toml
        &'[library]                            ', &
        &'source-dir="src"                     ', &
        &'']
        littlefile=[character(len=80) ::          &                             ! create placeholder module src/bname.f90
        &'module '//bname,                        &
        &'  implicit none',                       &
        &'  private',                             &
        &'',                                      &
        &'  public :: say_hello',                 &
        &'contains',                              &
        &'  subroutine say_hello',                &
        &'    print *, "Hello, '//bname//'!"',    &
        &'  end subroutine say_hello',            &
        &'end module '//bname]
       ! a proposed alternative default
        call warnwrite(join_path(settings%name, 'src', bname//'.f90'), littlefile) ! create NAME/src/NAME.f90
    endif

    if(settings%with_test)then
        call mkdir(join_path(settings%name, 'test'))                            ! create NAME/test or stop
        message=[character(len=80) ::  message,   &                             ! create next section of fpm.toml
        &'[[test]]                             ', &
        &'name="runTests"                      ', &
        &'source-dir="test"                    ', &
        &'main="main.f90"                      ', &
        &'']

        littlefile=[character(len=80) ::        &
        &'program main',                       &
        &'implicit none',                      &
        &'',                                   &
        &'print *, "Put some tests in here!"', &
        &'end program main']
        ! a proposed alternative default a little more substantive
        call warnwrite(join_path(settings%name, 'test/main.f90'), littlefile)    ! create NAME/test/main.f90
    endif

    if(settings%with_executable)then
        call mkdir(join_path(settings%name, 'app'))                             ! create NAME/app or stop
        message=[character(len=80) ::  message,   &                             ! create next section of fpm.toml
        &'[[executable]]                       ', &
        &'name="'//bname//'"                   ', &
        &'source-dir="app"                     ', &
        &'main="main.f90"                      ', &
        &'']

        littlefile=[character(len=80) ::          &
        &'program main',                          &
        &'  use '//bname//', only: say_hello',    &
        &'',                                      &
        &'  implicit none',                       &
        &'',                                      &
        &'  call say_hello',                      &
        &'end program main']
        call warnwrite(join_path(settings%name, 'app/main.f90'), littlefile)
    endif

    call warnwrite(join_path(settings%name, 'fpm.toml'), message)               ! now that built it write NAME/fpm.toml

    call run('cd ' // settings%name // ';git init')    ! assumes these commands work on all systems and git(1) is installed
contains

subroutine warnwrite(fname,data)
character(len=*),intent(in) :: fname
character(len=*),intent(in) :: data(:)

    if(.not.exists(fname))then
       call filewrite(fname,data)
    else
       write(stderr,'(*(g0,1x))')'fpm::new<WARNING>',fname,'already exists. Not overwriting'
    endif

end subroutine warnwrite

subroutine filewrite(filename,filedata)
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
! write filedata to file filename
character(len=*),intent(in)           :: filename
character(len=*),intent(in)           :: filedata(:)
integer                               :: lun, i, ios
character(len=256)                    :: message

    message=' '
    ios=0
    if(filename.ne.' ')then
        open(file=filename, &
        & newunit=lun, &
        & form='formatted', &      !  FORM      =  FORMATTED   |  UNFORMATTED
        & access='sequential', &   !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
        & action='write', &        !  ACTION    =  READ|WRITE  |  READWRITE
        & position='rewind', &     !  POSITION  =  ASIS        |  REWIND       |  APPEND
        & status='new', &          !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
        & iostat=ios, &
        & iomsg=message)
    else
        lun=stdout
        ios=0
    endif
    if(ios.ne.0)then
        write(stderr,'(*(a:,1x))')'*filewrite* error:',filename,trim(message)
        error stop 1
    endif
    do i=1,size(filedata)                                                    ! write file
        write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
        if(ios.ne.0)then
            write(stderr,'(*(a:,1x))')'*filewrite* error:',filename,trim(message)
            error stop 4
        endif
    enddo
    close(unit=lun,iostat=ios,iomsg=message)                                 ! close file
    if(ios.ne.0)then
        write(stderr,'(*(a:,1x))')'*filewrite* error:',trim(message)
        error stop 2
    endif
end subroutine filewrite

end subroutine cmd_new

end module fpm_cmd_new
