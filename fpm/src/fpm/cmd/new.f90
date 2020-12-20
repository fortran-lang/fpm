module fpm_cmd_new
!># Definition of the "new" subcommand
!>
!> A type of the general command base class [[fpm_cmd_settings]]
!> was created for the "new" subcommand ==> type [[fpm_new_settings]].
!> This procedure read the values that were set on the command line
!> from this type to decide what actions to take.
!>
!> It is virtually self-contained and so independant of the rest of the
!> application that it could function as a seperate program.
!>
!> The "new" subcommand options currently consist of a SINGLE top
!> directory name to create that must have a name that is an
!> allowable Fortran variable name. That should have been ensured
!> by the command line processing before this procedure is called.
!> So basically this routine has already had the options vetted and
!> just needs to conditionally create a few files.
!>
!> As described in the documentation documentation it will selectively
!> create the subdirectories app/, test/, src/, and example/
!> and populate them with sample files.
!>
!> It also needs to create an initial manifest file "fpm.toml".
!>
!> It then calls the system command "git init".
!>
!> It should test for file existence and not overwrite existing
!> files and inform the user if there were conflicts.
!>
!> Any changes should be reflected in the documentation in
!> [[fpm_command_line.f90]]
!>
!> FUTURE
!> A filename like "." would need system commands or a standard routine
!> like realpath(3c) to process properly.
!>
!> Perhaps allow more than one name on a single command. It is an arbitrary
!> restriction based on a concensus preference, not a required limitation.
!>
!> Initially the name of the directory is used as the module name in the
!> src file so it must be an allowable Fortran variable name. If there are
!> complaints about it it might be changed. Handling unicode at this point
!> might be problematic as not all current compilers handle it. Other
!> utilities like content trackers (ie. git) or repositories like github
!> might also have issues with alternative names or names with spaces, etc.
!> So for the time being it seems prudent to encourage simple ASCII top directory
!> names (similiar to the primary programming language Fortran itself).
!>
!> Should be able to create or pull more complicated initial examples
!> based on various templates. It should place or mention other relevant
!> documents such as a description of the manifest file format in user hands;
!> or how to access registered packages and local packages,
!> although some other command might provide that (and the help command should
!> be the first go-to for a CLI utility).

use fpm_command_line, only : fpm_new_settings
use fpm_environment, only : run, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only : join_path, exists, basename, mkdir, is_dir
use,intrinsic :: iso_fortran_env, only : stderr=>error_unit
implicit none
private
public :: cmd_new

contains

subroutine cmd_new(settings)
type(fpm_new_settings), intent(in) :: settings
character(len=:),allocatable :: bname          ! baeename of NAME
character(len=:),allocatable :: message(:)
character(len=:),allocatable :: littlefile(:)
character(len=8)             :: date

    !> get date to put into metadata in manifest file "fpm.toml"
    call date_and_time(DATE=date)

    !> TOP DIRECTORY NAME PROCESSING
    !> see if requested new directory already exists and process appropriately
    if(exists(settings%name) .and. .not.settings%backfill )then
        write(stderr,'(*(g0,1x))')&
        & '<ERROR>',settings%name,'already exists.'
        write(stderr,'(*(g0,1x))')&
        & '        perhaps you wanted to add --backfill ?'
        return
    elseif(is_dir(settings%name) .and. settings%backfill )then
        write(*,'(*(g0))')'backfilling ',settings%name
    elseif(exists(settings%name) )then
        write(stderr,'(*(g0,1x))')&
        & '<ERROR>',settings%name,'already exists and is not a directory.'
        return
    else
        ! make new directory
        call mkdir(settings%name)
    endif

    !> temporarily change to new directory as a test. NB: System dependent
    call run('cd '//settings%name)
    ! NOTE: need some system routines to handle filenames like "."
    ! like realpath() or getcwd().
    bname=basename(settings%name)

    ! create NAME/.gitignore file
    call warnwrite(join_path(settings%name, '.gitignore'), ['build/*'])

    littlefile=[character(len=80) :: '# '//bname, 'My cool new project!']

    ! create NAME/README.md
    call warnwrite(join_path(settings%name, 'README.md'), littlefile)

    ! start building NAME/fpm.toml
    message=[character(len=80) ::                 &
    &'name = "'//bname//'"                     ', &
    &'version = "0.1.0"                        ', &
    &'license = "license"                      ', &
    &'author = "Jane Doe"                      ', &
    &'maintainer = "jane.doe@example.com"      ', &
    &'copyright = "'//date(1:4)//' Jane Doe"   ', &
    &'                                         ', &
    &'']

    if(settings%with_lib)then
        call mkdir(join_path(settings%name,'src') )
        ! create next section of fpm.toml
        message=[character(len=80) ::  message,   &
        &'[library]                            ', &
        &'source-dir="src"                     ', &
        &'']
        ! create placeholder module src/bname.f90
        littlefile=[character(len=80) ::          &
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
        ! create NAME/src/NAME.f90
        call warnwrite(join_path(settings%name, 'src', bname//'.f90'),&
         & littlefile)
    endif

    if(settings%with_test)then

       ! create NAME/test or stop
       call mkdir(join_path(settings%name, 'test'))
        ! create next section of fpm.toml
        message=[character(len=80) ::  message,   &
        &'[[test]]                             ', &
        &'name="runTests"                      ', &
        &'source-dir="test"                    ', &
        &'main="check.f90"                     ', &
        &'']

        littlefile=[character(len=80) ::       &
        &'program check',                      &
        &'implicit none',                      &
        &'',                                   &
        &'print *, "Put some tests in here!"', &
        &'end program check']
        ! create NAME/test/check.f90
        call warnwrite(join_path(settings%name, 'test/check.f90'), littlefile)
    endif

    if(settings%with_example)then

       ! create NAME/example or stop
       call mkdir(join_path(settings%name, 'example'))
        ! create next section of fpm.toml
        message=[character(len=80) ::  message,   &
        &'[[example]]                          ', &
        &'name="demo"                          ', &
        &'source-dir="example"                 ', &
        &'main="demo.f90"                      ', &
        &'']

        littlefile=[character(len=80) ::          &
        &'program demo',                          &
        &'implicit none',                         &
        &'',                                      &
        &'print *, "Put some examples in here!"', &
        &'end program demo']
        ! create NAME/example/demo.f90
        call warnwrite(join_path(settings%name, 'example/demo.f90'), littlefile)
    endif

    if(settings%with_executable)then
        ! create next section of fpm.toml
        call mkdir(join_path(settings%name, 'app'))
        ! create NAME/app or stop
        message=[character(len=80) ::  message,   &
        &'[[executable]]                       ', &
        &'name="'//bname//'"                   ', &
        &'source-dir="app"                     ', &
        &'main="main.f90"                      ', &
        &'']

        if(exists(bname//'/src/'))then
            littlefile=[character(len=80) ::          &
            &'program main',                          &
            &'  use '//bname//', only: say_hello',    &
            &'  implicit none',                       &
            &'',                                      &
            &'  call say_hello()',                    &
            &'end program main']
        else
            littlefile=[character(len=80) ::                 &
            &'program main',                                 &
            &'  implicit none',                              &
            &'',                                             &
            &'  print *, "hello from project '//bname//'"',  &
            &'end program main']
        endif
        call warnwrite(join_path(settings%name, 'app/main.f90'), littlefile)
    endif

    ! now that built it write NAME/fpm.toml
    call warnwrite(join_path(settings%name, 'fpm.toml'), message)
    ! assumes git(1) is installed and in path
    call run('git init ' // settings%name)
contains

subroutine warnwrite(fname,data)
character(len=*),intent(in) :: fname
character(len=*),intent(in) :: data(:)

    if(.not.exists(fname))then
        call filewrite(fname,data)
    else
        write(stderr,'(*(g0,1x))')'<INFO>  ',fname,&
        & 'already exists. Not overwriting'
    endif

end subroutine warnwrite

subroutine filewrite(filename,filedata)
! procedure to write filedata to file filename
use,intrinsic :: iso_fortran_env, only : &
 & stdin=>input_unit, stdout=>output_unit, stderr=>error_unit

character(len=*),intent(in)           :: filename
character(len=*),intent(in)           :: filedata(:)
integer                               :: lun, i, ios
character(len=256)                    :: message

    message=' '
    ios=0
    if(filename.ne.' ')then
        open(file=filename, &
        & newunit=lun, &
        & form='formatted', &    ! FORM    = FORMATTED | UNFORMATTED
        & access='sequential', & ! ACCESS  = SEQUENTIAL| DIRECT | STREAM
        & action='write', &      ! ACTION  = READ|WRITE| READWRITE
        & position='rewind', &   ! POSITION= ASIS      | REWIND | APPEND
        & status='new', &        ! STATUS  = NEW| REPLACE| OLD| SCRATCH| UNKNOWN
        & iostat=ios, &
        & iomsg=message)
    else
        lun=stdout
        ios=0
    endif
    if(ios.ne.0)then
        write(stderr,'(*(a:,1x))')&
        & '<ERROR> *filewrite*:',filename,trim(message)
        error stop 1
    endif
    ! write file
    do i=1,size(filedata)
        write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
        if(ios.ne.0)then
            write(stderr,'(*(a:,1x))')&
            & '<ERROR> *filewrite*:',filename,trim(message)
            error stop 4
        endif
    enddo
    ! close file
    close(unit=lun,iostat=ios,iomsg=message)
    if(ios.ne.0)then
        write(stderr,'(*(a:,1x))')'<ERROR> *filewrite*:',trim(message)
        error stop 2
    endif
end subroutine filewrite

end subroutine cmd_new

end module fpm_cmd_new
