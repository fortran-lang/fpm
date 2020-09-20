module fpm

use fpm_strings, only: string_t, str_ends_with
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings
use fpm_environment, only: run, get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only: join_path, number_of_rows, list_files, exists
use fpm_model, only: srcfile_ptr, srcfile_t, fpm_model_t
use fpm_sources, only: add_executable_sources, add_sources_from_dir, &
                       resolve_module_dependencies
use fpm_manifest, only : get_package_data, default_executable, &
                         default_library, package_t
use fpm_error, only : error_t
use M_CLI2,    only : get_args, words=>unnamed, remaining
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
implicit none
private
public :: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test


contains

subroutine build_model(model, settings, package, error)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_t), intent(in) :: package
    type(error_t), allocatable, intent(out) :: error
    logical :: cli_release

    model%package_name = package%name

    ! #TODO: Choose flags and output directory based on cli settings & manifest inputs
    model%fortran_compiler = 'gfortran'
    call get_args('release',cli_release)
    if(cli_release)then
        model%output_directory = 'build/gfortran_release'
        model%fortran_compile_flags=' &
             & -O3 &
             & -Wimplicit-interface &
             & -fPIC &
             & -fmax-errors=1 &
             & -ffast-math &
             & -funroll-loops ' // &
             & '-J'//join_path(model%output_directory,model%package_name)
    else
        model%output_directory = 'build/gfortran_debug'
        model%fortran_compile_flags = ' -Wall -Wextra -Wimplicit-interface  -fPIC -fmax-errors=1 -g '// &
                                      '-fbounds-check -fcheck-array-temporaries -fbacktrace '// &
                                      '-J'//join_path(model%output_directory,model%package_name)
    endif
    model%link_flags = ''

    ! Add sources from executable directories
    if (allocated(package%executable)) then

        call add_executable_sources(model%sources, package%executable, &
                                     is_test=.false., error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%test)) then

        call add_executable_sources(model%sources, package%test, &
                                     is_test=.true., error=error)

        if (allocated(error)) then
            return
        end if

    end if

    if (allocated(package%library)) then

        call add_sources_from_dir(model%sources,package%library%source_dir, &
                                      error=error)

        if (allocated(error)) then
            return
        end if

    end if

    call resolve_module_dependencies(model%sources)

end subroutine build_model

subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_t) :: package
type(fpm_model_t) :: model
type(error_t), allocatable :: error
call get_package_data(package, "fpm.toml", error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

! Populate library in case we find the default src directory
if (.not.allocated(package%library) .and. exists("src")) then
    allocate(package%library)
    call default_library(package%library)
end if

! Populate executable in case we find the default app directory
if (.not.allocated(package%executable) .and. exists("app")) then
    allocate(package%executable(1))
    call default_executable(package%executable(1), package%name)
end if

if (.not.(allocated(package%library) .or. allocated(package%executable))) then
    print '(a)', "Neither library nor executable found, there is nothing to do"
    error stop 1
end if

call build_model(model, settings, package, error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

call build_package(model)

end subroutine

subroutine cmd_install()
    print *, "fpm error: 'fpm install' not implemented."
    error stop 1
end subroutine cmd_install

subroutine cmd_new() ! --with-executable F --with-test F '
character(len=:),allocatable :: message(:)
logical                      :: cli_with_executable  ! command line keyword value set by get_args(3f)
logical                      :: cli_with_test        ! command line keyword value set by get_args(3f)
character(len=:),allocatable :: dirname, basename
   call get_args('with-executable',cli_with_executable)                         ! get command line arguments
   call get_args('with-test',cli_with_test)
   ! assume everything unclaimed by keywords on the command line are command arguments for new command
   if(size(words).eq.2.and.len(words).gt.0)then
      dirname=trim(words(2))
      basename=dirname !! basename(dirname)
      !! are full pathnames allowed? Is more than one pathname allowed?
   elseif(size(words).gt.2)then
     write(stderr,'(a)') 'fpm::new<ERROR> more than one directory name'
     write(stderr,'(a)') '      usage: fpm new DIRECTORY_NAME --with-executable --with-test'
     stop 3
   else                                                                     ! no directory name to create or update on commandline
     write(stderr,'(a)') 'fpm::new<ERROR> missing directory name'
     write(stderr,'(a)') '      usage: fpm new DIRECTORY_NAME --with-executable --with-test'
     stop 4
   endif

   message=[character(len=80) :: &                                           ! create fpm.toml
    &'name = "'//basename//'"              ', &
    &'version = "0.1.0"                    ', &
    &'license = "license"                  ', &
    &'author = "Jane Doe"                  ', &
    &'maintainer = "jane.doe@example.com"  ', &
    &'copyright = "2020 Jane Doe"          ', &
    &'                                     ', &
    &'[library]                            ', &
    &'source-dir="src"                     ', &
    &'']

   if(cli_with_test)then
      message=[character(len=80) ::  message, &                                ! create next section of fpm.toml
       &'[[test]]                             ', &
       &'name="runTests"                      ', &
       &'source-dir="test"                    ', &
       &'main="main.f90"                      ', &
       &'']
   endif

   if(cli_with_executable)then
      message=[character(len=80) ::  message, &                               ! create next section of fpm.toml
       &'[[executable]]                       ', &
       &'name="'//basename//'"                ', &
       &'source-dir="app"                     ', &
       &'main="main.f90"                      ', &
       &'']
   endif

   write(*,'(a)')message
   print *, "fpm error: 'fpm new' not implemented."
   error stop 1
end subroutine cmd_new

subroutine cmd_run()
    integer                      :: i
    character(len=:),allocatable :: cli_args
    logical                      :: cli_release

    call get_args('release',cli_release)
    cli_args=remaining

    write(*,*)'RELEASE=',cli_release
    if(size(words).lt.2)then
        write(*,*)'RUN DEFAULTS with arguments ['//cli_args//']'
    else
        do i=2,size(words)
            write(*,*)'RUN:'//trim(words(i))//' with arguments ['//cli_args//']'
        enddo
    endif

    print *, "fpm error: 'fpm run' not implemented."
    error stop 1

end subroutine cmd_run

subroutine cmd_test()
    logical                      :: cli_release
    character(len=:),allocatable :: cli_args
    character(len=:),allocatable :: release_name
    integer                      :: i

    cli_args=remaining
    call get_args('release',cli_release)

    !! looks like would get this from model when cmd_test is implimented
    release_name=trim(merge('gfortran_release','gfortran_debug  ',cli_release))

    write(*,*)'RELEASE=',cli_release,' RELEASE_NAME=',release_name,' ARGS=',cli_args
    if( size(words) .gt.1 )then
       write(*,*)'RUN THESE:'
        do i=2,size(words)
            write(*,*)'RUN:'//trim(words(i))//' with arguments ['//cli_args//']'
        enddo
    else
       write(*,*)'RUN DEFAULTS: with arguments ['//cli_args//']'
    endif

    print *, "fpm error: 'fpm test' not implemented."
    error stop 1
end subroutine cmd_test

end module fpm
