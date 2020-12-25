module fpm_cmd_new
!># Definition of the "new" subcommand
!>
!> A type of the general command base class [[fpm_cmd_settings]]
!> was created for the "new" subcommand ==> type [[fpm_new_settings]].
!> This procedure read the values that were set on the command line
!> from this type to decide what actions to take.
!>
!> It is virtually self-contained and so independant of the rest of the
!> application that it could function as a separate program.
!>
!> The "new" subcommand options currently consist of a SINGLE top
!> directory name to create that must have a name that is an
!> allowable Fortran variable name. That should have been ensured
!> by the command line processing before this procedure is called.
!> So basically this routine has already had the options vetted and
!> just needs to conditionally create a few files.
!>
!> As described in the documentation it will selectively
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
use fpm_filesystem, only : fileopen, fileclose, filewrite, warnwrite
use fpm_strings, only : join
use,intrinsic :: iso_fortran_env, only : stderr=>error_unit
implicit none
private
public :: cmd_new

contains

subroutine cmd_new(settings)
type(fpm_new_settings), intent(in) :: settings
integer,parameter            :: tfc = selected_char_kind('DEFAULT')
character(len=:,kind=tfc),allocatable :: bname          ! baeename of NAME
character(len=:,kind=tfc),allocatable :: tomlfile(:)
character(len=:,kind=tfc),allocatable :: littlefile(:)

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
    if(settings%with_full)then
       tomlfile=[character(len=80) :: &
       &'# Manifest root                                                                 ',&
       &'                                                                                ',&
       &'## Project Identification                                                       ',&
       &'name = "'//bname//'"', &
       &'  #  The project name (required) is how the project will be referred to.        ',&
       &'  #  It is used by another package using it as a dependency and as the          ',&
       &'  #  default name of the library built from src/.                               ',&
       &'                                                                                ',&
       &'version = "0.1.0"                                                               ',&
       &'  #  The project version number is a string. A recommended standardized way     ',&
       &'  #  to manage and specify versions is the Semantic Versioning scheme.          ',&
       &'                                                                                ',&
       &'license = "license"                                                             ',&
       &'  #  Licensing information specified using a standard such as SPDX              ',&
       &'  #  identifiers are preferred (eg. "Apache-2.0 OR MIT" or "LGPL-3.0-or-later").',&
       &'                                                                                ',&
       &'maintainer = "jane.doe@example.com"                                             ',&
       &'  #  Information on the project maintainer and means to reach out to them.      ',&
       &'                                                                                ',&
       &'author = "Jane Doe"                                                             ',&
       &'  #  Information on the project author.                                         ',&
       &'                                                                                ',&
       &'copyright = "Copyright 2020 Jane Doe"                                           ',&
       &'  #  A statement clarifying the Copyright status of the project.                ',&
       &'                                                                                ',&
       &'#description = "A short project summary in plain text"                          ',&
       &'  #  The description provides a short summary on the project. It should be      ',&
       &'  #  plain text and not use any markup formatting.                              ',&
       &'                                                                                ',&
       &'#categories = ["fortran", "graphics"]                                           ',&
       &'  #  Categories associated with the project. Listing only one is preferred.     ',&
       &'                                                                                ',&
       &'#keywords = ["hdf5", "mpi"]                                                     ',&
       &'  #  The keywords field is an array of strings describing the project.          ',&
       &'                                                                                ',&
       &'#homepage = "https://stdlib.fortran-lang.org"                                   ',&
       &'  #  URL to the webpage of the project.                                         ',&
       &'                                                                                ',&
       &'## TABLES                                                                       ',&
       &'                                                                                ',&
       &'## BUILD CONFIGURATION SECTION                                                  ',&
       &'[build]                                                                         ',&
       &'#                                                                               ',&
       &'# Files will be searched for automatically (by default) in src/, app/, test/    ',&
       &'# and example/. This can be turned off for app/, test, exampl                   ',&
       &'auto-executables = true  # Toggle automatic discovery of executables            ',&
       &'auto-examples = true     # Toggle automatic discovery of example programs       ',&
       &'auto-tests = true        # Toggle automatic discovery of test executables       ',&
       &'#link = ["blas", "lapack", "z", "X11"] # Linking against libraries              ',&
       &'                                                                                ',&
       &'## TARGETS                                                                      ',&
       &'']
    endif

    if(settings%with_bare)then
    elseif(settings%with_lib)then
        call mkdir(join_path(settings%name,'src') )
        ! create next section of fpm.toml
        if(settings%with_full)then
            tomlfile=[character(len=80) ::  tomlfile, &
            &'[library]                            ', &
            &'source-dir="src"                     ', &
            &'']
        endif
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

    if(settings%with_bare)then
    elseif(settings%with_test)then

       ! create NAME/test or stop
       call mkdir(join_path(settings%name, 'test'))
        ! create next section of fpm.toml
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile ,&
           &'[[test]]                             ', &
           &'name="runTests"                      ', &
           &'source-dir="test"                    ', &
           &'main="check.f90"                     ', &
           &'']
        endif

        littlefile=[character(len=80) ::       &
        &'program check',                      &
        &'implicit none',                      &
        &'',                                   &
        &'print *, "Put some tests in here!"', &
        &'end program check']
        ! create NAME/test/check.f90
        call warnwrite(join_path(settings%name, 'test/check.f90'), littlefile)
    endif

    if(settings%with_bare)then
    elseif(settings%with_example)then

       ! create NAME/example or stop
       call mkdir(join_path(settings%name, 'example'))
        ! create next section of fpm.toml
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile, &
           &'[[example]]                          ', &
           &'name="demo"                          ', &
           &'source-dir="example"                 ', &
           &'main="demo.f90"                      ', &
           &'']
        endif

        littlefile=[character(len=80) ::          &
        &'program demo',                          &
        &'implicit none',                         &
        &'',                                      &
        &'print *, "Put some examples in here!"', &
        &'end program demo']
        ! create NAME/example/demo.f90
        call warnwrite(join_path(settings%name, 'example/demo.f90'), littlefile)
    endif

    if(settings%with_bare)then
    elseif(settings%with_executable)then
        ! create next section of fpm.toml
        call mkdir(join_path(settings%name, 'app'))
        ! create NAME/app or stop
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile, &
           &'[[executable]]                       ', &
           &'name="'//bname//'"                   ', &
           &'source-dir="app"                     ', &
           &'main="main.f90"                      ', &
           &'']
        endif

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

    if(settings%with_full)then
       tomlfile=[character(len=80) ::  tomlfile,   &
       &'[dependencies]                                                                  ', &
       &'#For a complete list of keys and their attributes see                           ', &
       &'#                                                                               ', &
       &'#  https://github.com/fortran-lang/fpm/blob/master/manifest-reference.md        ', &
       &'#                                                                               ', &
       &'']
    endif

    ! now that built it write NAME/fpm.toml
    if( allocated(tomlfile) )then
        call validate_toml_data(tomlfile)
        call warnwrite(join_path(settings%name, 'fpm.toml'), tomlfile)
    else
        call create_verified_basic_manifest(join_path(settings%name, 'fpm.toml'))
    endif
    ! assumes git(1) is installed and in path
    call run('git init ' // settings%name)
contains

subroutine create_verified_basic_manifest(filename)
!> create a basic but verified default manifest file
use fpm_toml, only : toml_table, toml_serializer, set_value
use fpm_manifest_package, only : package_config_t, new_package
use fpm_error, only : error_t
implicit none
character(len=*),intent(in) :: filename
   type(toml_table)            :: table
   type(toml_serializer)       :: ser
   type(package_config_t)      :: package
   type(error_t), allocatable  :: error
   integer                     :: lun
   character(len=8)            :: date

    !> get date to put into metadata in manifest file "fpm.toml"
    call date_and_time(DATE=date)
    table = toml_table()
    ser = toml_serializer()
    call fileopen(filename,lun) ! fileopen stops on error

    call set_value(table, "name",       BNAME)
    call set_value(table, "version",    "0.1.0")
    call set_value(table, "license",    "license")
    call set_value(table, "author",     "Jane Doe")
    call set_value(table, "maintainer", "jane.doe@example.com")
    call set_value(table, "copyright",  'Copyright '//date(1:4)//', Jane Doe')
    ! continue building of manifest
    ! ...
    call new_package(package, table, error)
    if (allocated(error)) stop 3
    if(settings%verbose)then
       call table%accept(ser)
    endif
    ser%unit=lun
    call table%accept(ser)
    call fileclose(lun) ! fileopen stops on error

end subroutine create_verified_basic_manifest


subroutine validate_toml_data(input)
!> verify a string array is a valid fpm.toml file
!
use tomlf, only : toml_parse
use fpm_toml, only : toml_table, toml_serializer
implicit none
character(kind=tfc,len=:),intent(in),allocatable :: input(:)
character(len=1), parameter                      :: nl = new_line('a')
type(toml_table), allocatable                    :: table
character(kind=tfc, len=:), allocatable          :: joined_string
type(toml_serializer)                            :: ser

! you have to add a newline character by using the intrinsic
! function `new_line("a")` to get the lines processed correctly.
joined_string = join(input,right=nl)

if (allocated(table)) deallocate(table)
call toml_parse(table, joined_string)
if (allocated(table)) then
   if(settings%verbose)then
      ! If the TOML file is successfully parsed the table will be allocated and
      ! can be written to the standard output by passing the `toml_serializer`
      ! as visitor to the table.
      call table%accept(ser)
   endif
   call table%destroy
endif

end subroutine validate_toml_data

end subroutine cmd_new

end module fpm_cmd_new
