module fpm

use fpm_strings,      only : string_t, str_ends_with
use fpm_backend,      only : build_package
use fpm_command_line, only : fpm_build_settings, fpm_new_settings, &
                             fpm_run_settings, fpm_install_settings, fpm_test_settings
use fpm_environment,  only : run, get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem,   only : join_path, number_of_rows, list_files, exists, basename, mkdir
use fpm_model,        only : srcfile_ptr, srcfile_t, fpm_model_t
use fpm_sources,      only : add_executable_sources, add_sources_from_dir, &
                             resolve_module_dependencies
use fpm_manifest,     only : get_package_data, default_executable, &
                             default_library, package_t, default_test
use fpm_error,        only : error_t
use fpm_manifest_test, only : test_t
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
implicit none
private
public :: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine build_model(model, settings, package, error)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_t), intent(in) :: package
    type(error_t), allocatable, intent(out) :: error
    integer :: i

    model%package_name = package%name

    ! #TODO: Choose flags and output directory based on cli settings & manifest inputs
    model%fortran_compiler = 'gfortran'

    if(settings%release)then
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
    if(settings%list)then
         do i=1,size(model%sources)
            write(stderr,'(*(g0,1x))')'fpm::build<INFO>:file expected at',model%sources(i)%file_name, &
             & merge('exists        ','does not exist',exists(model%sources(i)%file_name) )
         enddo
         stop
    else
       call resolve_module_dependencies(model%sources)
    endif

end subroutine build_model
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
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

! Populate test in case we find the default test directory
if (.not.allocated(package%test) .and. exists("test")) then
    allocate(package%test(1))
    call default_test(package%test(1), package%name)
end if

if (.not.(allocated(package%library) .or. allocated(package%executable) .or. allocated(package%test) )) then
    print '(a)', "Neither library nor executable found, there is nothing to do"
    error stop 1
end if

call build_model(model, settings, package, error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

call build_package(model)

end subroutine cmd_build
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_install(settings)
type(fpm_install_settings), intent(in) :: settings
    print *, "fpm error: 'fpm install' not implemented."
    error stop 1
end subroutine cmd_install
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_new(settings) ! --with-executable F --with-test F '
type(fpm_new_settings), intent(in) :: settings
integer :: ierr
character(len=:),allocatable :: bname          ! baeename of NAME
character(len=:),allocatable :: message(:)
character(len=:),allocatable :: littlefile(:)
   call mkdir(settings%name)      ! make new directory
   call run('cd '//settings%name) ! change to new directory as a test. New OS routines to improve this; system dependent potentially
   !! NOTE: need some system routines to handle filenames like "." like realpath() or getcwd().
   bname=basename(settings%name)

   !! weird gfortran bug?? lines truncated to concatenated string length, not 80
   !! hit some weird gfortran bug when littlefile data was an argument to warnwrite(3f), ok when a variable

    call warnwrite(join_path(settings%name, '.gitignore'), ['build/*'])        ! create NAME/.gitignore file

   littlefile=[character(len=80) :: '# '//bname, 'My cool new project!'] 

   call warnwrite(join_path(settings%name, 'README.md'), littlefile)           ! create NAME/README.md

   message=[character(len=80) ::              &                                ! start building NAME/fpm.toml
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
      message=[character(len=80) ::  message,    &                             ! create next section of fpm.toml
       &'[library]                            ', &
       &'source-dir="src"                     ', &
       &'']
      littlefile=[character(len=80) ::           &                             ! create placeholder module src/bname.f90
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
      call mkdir(join_path(settings%name, 'test'))                             ! create NAME/test or stop
      message=[character(len=80) ::  message,    &                             ! create next section of fpm.toml
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
      call mkdir(join_path(settings%name, 'app'))                              ! create NAME/app or stop
      message=[character(len=80) ::  message,    &                             ! create next section of fpm.toml
       &'[[executable]]                       ', &
       &'name="'//bname//'"                   ', &
       &'source-dir="app"                     ', &
       &'main="main.f90"                      ', &
       &'']

      littlefile=[character(len=80) ::           &
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
!===================================================================================================================================
subroutine warnwrite(fname,data)
character(len=*),intent(in) :: fname
character(len=*),intent(in) :: data(:)
   if(.not.exists(fname))then
      call filewrite(fname,data)
   else
      write(stderr,'(*(g0,1x))')'fpm::new<WARNING>',fname,'already exists. Not overwriting'
   endif
end subroutine warnwrite
!===================================================================================================================================
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
      write(stderr,'(*(a,1x))')'*filewrite* error:',filename,trim(message)
      error stop 1
   endif
   do i=1,size(filedata)                                                    ! write file
      write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
      if(ios.ne.0)then
         write(stderr,'(*(a,1x))')'*filewrite* error:',filename,trim(message)
         stop 4
      endif
   enddo
   close(unit=lun,iostat=ios,iomsg=message)                                 ! close file
   if(ios.ne.0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',trim(message)
      error stop 2
   endif
end subroutine filewrite

end subroutine cmd_new
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_run(settings)
type(fpm_run_settings), intent(in) :: settings
character(len=:),allocatable       :: release_name, cmd, fname
integer                            :: i, j
type(package_t)                    :: package
type(error_t), allocatable         :: error
character(len=:),allocatable       :: newwords(:)
logical,allocatable                :: foundit(:)
logical                            :: list
   call get_package_data(package, "fpm.toml", error)
   if (allocated(error)) then
       print '(a)', error%message
       stop
   endif
   release_name=trim(merge('gfortran_release','gfortran_debug  ',settings%release))
   newwords=[character(len=0) ::]
   ! Populate executable in case we find the default app directory
   if (.not.allocated(package%executable) .and. exists("app")) then
      allocate(package%executable(1))
      call default_executable(package%executable(1), package%name)
   endif
   if(size(settings%name).eq.0)then
      if ( .not.allocated(package%executable) ) then
         write(stderr,'(*(g0,1x))')'fpm::run<INFO>:no executables found in fpm.toml and no default app/ directory'
         stop
      endif
      allocate(foundit(size(package%executable)))
      do i=1,size(package%executable)
         fname=join_path('build',release_name,package%executable(i)%source_dir,package%executable(i)%name)
         newwords=[character(len=max(len(newwords),len(fname))) :: newwords,fname]
      enddo
      if(size(newwords).lt.1)then
         write(stderr,'(*(g0,1x))')'fpm::run<INFO>:no executables found in fpm.toml'
         stop
      endif
   else
      !! expand names, duplicates are a problem??
      allocate(foundit(size(settings%name)))
      foundit=.false.
      FINDIT: do i=1,size(package%executable)
         do j=1,size(settings%name)
            if(settings%name(j).eq.package%executable(i)%name)then
               fname=join_path('build',release_name,package%executable(i)%source_dir,package%executable(i)%name)
               newwords=[character(len=max(len(newwords),len(fname))) :: newwords,fname]
               foundit(j)=.true.
            endif
         enddo
      enddo FINDIT
      do i=1,size(settings%name)
         if(.not.foundit(i))then
            write(stderr,'(*(g0,1x))')'fpm::run<ERROR>:executable',trim(settings%name(i)),'not located'
         !!elseif(settings%debug)then
         !!   write(stderr,'(*(g0,1x))')'fpm::run<INFO>:executable',trim(settings%name(i)),'located at',newwords(i),&
         !!    & merge('exists        ','does not exist',exists(trim(settings%name(i))))
         endif
      enddo
      if(allocated(foundit))deallocate(foundit)
   endif
   do i=1,size(newwords)
      !! list is a new option for use with xargs, to move files to production area, valgrind, gdb, ls -l, ....
      !! maybe add as --mask and could do --mask 'echo %xx' or --mask 'cp %XX /usr/local/bin/' an so on
      !! default if blank would be filename uptodate|needs|updated|doesnotexist creation_date, ...
      !! or maybe just list filenames so can pipe through xargs, and so on
      if(settings%list)then
         write(stderr,'(*(g0,1x))')'fpm::run<INFO>:executable expected at',newwords(i),&
          & merge('exists        ','does not exist',exists(newwords(i)))
         cycle
      endif
      cmd=newwords(i) // ' ' // settings%args
      if(exists(newwords(i)))then
         call run(cmd)
      else ! try to build -- once build works conditionally this should be an unconditional call
         call cmd_build(fpm_build_settings(release=settings%release,list=.false.))
         if(exists(newwords(i)))then
            call run(cmd)
         else
            write(stderr,*)'fpm::run<ERROR>',cmd,' not found'
         endif
      endif
   enddo
   deallocate(newwords)
end subroutine cmd_run
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_test(settings)
type(fpm_test_settings), intent(in) :: settings
character(len=:),allocatable       :: release_name, cmd, fname
integer                            :: i, j
type(package_t)                    :: package
type(error_t), allocatable         :: error
character(len=:),allocatable       :: newwords(:)
logical,allocatable                :: foundit(:)
logical                            :: list
   call get_package_data(package, "fpm.toml", error)
   if (allocated(error)) then
       print '(a)', error%message
       stop
   endif
   release_name=trim(merge('gfortran_release','gfortran_debug  ',settings%release))
   newwords=[character(len=0) ::]
   
   ! Populate test in case we find the default test directory
   if (.not.allocated(package%test) .and. exists("test")) then
      allocate(package%test(1))
      call default_test(package%test(1), package%name)
   endif
   if(size(settings%name).eq.0)then
      if ( .not.allocated(package%test) ) then
         write(stderr,'(*(g0,1x))')'fpm::run<INFO>:no tests found in fpm.toml and no default test/ directory'
         stop
      endif
      allocate(foundit(size(package%test)))
      do i=1,size(package%test)
         fname=join_path('build',release_name,package%test(i)%source_dir,package%test(i)%name)
         newwords=[character(len=max(len(newwords),len(fname))) :: newwords,fname]
      enddo
      if(size(newwords).lt.1)then
         write(stderr,'(*(g0,1x))')'fpm::run<INFO>:no tests found in fpm.toml'
         stop
      endif
   else
      !! expand names, duplicates are a problem??
      allocate(foundit(size(settings%name)))
      foundit=.false.
      FINDIT: do i=1,size(package%test)
         do j=1,size(settings%name)
            if(settings%name(j).eq.package%test(i)%name)then
               fname=join_path('build',release_name,package%test(i)%source_dir,package%test(i)%name)
               newwords=[character(len=max(len(newwords),len(fname))) :: newwords,fname]
               foundit(j)=.true.
            endif
         enddo
      enddo FINDIT
      do i=1,size(settings%name)
         if(.not.foundit(i))then
            write(stderr,'(*(g0,1x))')'fpm::run<ERROR>:test',trim(settings%name(i)),'not located'
         !!elseif(settings%debug)then
         !!   write(stderr,'(*(g0,1x))')'fpm::run<INFO>:test',trim(settings%name(i)),'located at',newwords(i),&
         !!    & merge('exists        ','does not exist',exists(trim(settings%name(i))))
         endif
      enddo
      if(allocated(foundit))deallocate(foundit)
   endif
   do i=1,size(newwords)
      !! list is a new option for use with xargs, to move files to production area, valgrind, gdb, ls -l, ....
      !! maybe add as --mask and could do --mask 'echo %xx' or --mask 'cp %XX /usr/local/bin/' an so on
      !! default if blank would be filename uptodate|needs|updated|doesnotexist creation_date, ...
      !! or maybe just list filenames so can pipe through xargs, and so on
      if(settings%list)then
         write(stderr,'(*(g0,1x))')'fpm::run<INFO>:test expected at',newwords(i),&
          & merge('exists        ','does not exist',exists(newwords(i)))
         cycle
      endif
      cmd=newwords(i) // ' ' // settings%args
      if(exists(newwords(i)))then
         call run(cmd)
      else ! try to build -- once build works conditionally this should be an unconditional call
         call cmd_build(fpm_build_settings(release=settings%release,list=.false.))
         if(exists(newwords(i)))then
            call run(cmd)
         else
            write(stderr,*)'fpm::run<ERROR>',cmd,' not found'
         endif
      endif
   enddo
   deallocate(newwords)
end subroutine cmd_test
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module fpm
