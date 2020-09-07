module fpm
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use environment,  only : get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use environment,  only : filewrite, system_getcwd, ifmkdir
use M_CLI2,       only : get_args, words=>unnamed, remaining
use fpm_manifest, only : get_package_data, default_executable, default_library, &
    & package_t
use fpm_error, only : error_t
implicit none
private
public :: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test
type string_t
    character(len=:), allocatable :: s
end type

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
integer function number_of_rows(s) result(nrows)
! determine number or rows
integer,intent(in)::s
integer :: ios
character(len=100) :: r
rewind(s)
nrows = 0
do
    read(s, *, iostat=ios) r
    if (ios /= 0) exit
    nrows = nrows + 1
enddo
rewind(s)
end function number_of_rows
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine list_files(dir, files)
character(len=*), intent(in) :: dir
type(string_t), allocatable, intent(out) :: files(:)
character(len=100) :: filename
character(len=256) :: message
integer :: stat, u, i, ios
! Using `inquire` / exists on directories works with gfortran, but not ifort
if (.not. exists(dir)) then
    allocate(files(0))
    return
endif
select case (get_os_type())
    case (OS_LINUX)
        call execute_command_line("ls " // dir // " > fpm_ls.out", exitstat=stat)
    case (OS_MACOS)
        call execute_command_line("ls " // dir // " > fpm_ls.out", exitstat=stat)
    case (OS_WINDOWS)
        call execute_command_line("dir /b " // dir // " > fpm_ls.out", exitstat=stat)
end select
if (stat /= 0) then
    print *, "execute_command_line() failed"
    error stop 2
endif
open(newunit=u, file="fpm_ls.out", status="old")
allocate(files(number_of_rows(u)))
do i = 1, size(files)
    read(u, *) filename
    files(i)%s = trim(filename)
enddo
close(u,status='delete',iostat=ios,iomsg=message)
if(ios.ne.0)then
   write(*,*)'*list_files* error deleting scratch file:',trim(message)
endif
end subroutine list_files
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine run(cmd)
character(len=*), intent(in) :: cmd
integer :: stat
print *, "+ ", cmd
call execute_command_line(cmd, exitstat=stat)
if (stat /= 0) then
    print *, "Command failed"
    error stop 3
endif
end subroutine run
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
logical function exists(filename) result(r)
character(len=*), intent(in) :: filename
inquire(file=filename, exist=r)
end function exists
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
logical function str_ends_with(s, e) result(r)
character(*), intent(in) :: s, e
integer :: n1, n2
n1 = len(s)-len(e)+1
n2 = len(s)
if (n1 < 1) then
    r = .false.
else
    r = (s(n1:n2) == e)
endif
end function str_ends_with
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_build()
type(package_t) :: package
type(error_t), allocatable  :: error
type(string_t), allocatable :: files(:)
character(:), allocatable   :: basename, linking
integer                     :: i, n
logical                     :: release
character(:), allocatable   :: release_name, options 
character(:), allocatable   :: builddir, cmd, inc, appdir
character(len=1),save       :: sep='/'

  options=''
  call get_args('release',release)
  if(release)then
     release_name='gfortran_release'
     options=' &
             & -O3 &
             & -Wimplicit-interface &
             & -fPIC &
             & -fmax-errors=1 &
             & -ffast-math &
             & -funroll-loops '
  else
     release_name='gfortran_debug'
     options=' &
             & -mtune=generic &
             & -Wall &
             & -Wextra &
             & -g &
             & -Wimplicit-interface &
             & -fPIC &
             & -fmax-errors=1 &
             & -fbounds-check &
             & -fcheck-array-temporaries&
             & -fbacktrace '
  endif


call get_package_data(package, "fpm.toml", error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
endif

   builddir='build/' // release_name // sep // package%name
   inc=' -I ' // builddir // ' -J ' // builddir // ' '

   !!gfortran bug: call ifmkdir('build', 'build'// sep //release_name, 'build'// sep //release_name// sep //pkg_name])
   call ifmkdir('build')
   call ifmkdir('build'// sep //release_name)
   call ifmkdir('build'// sep //release_name// sep //package%name)

! Populate library in case we find the default src directory
if (.not.allocated(package%library) .and. exists("src")) then
    call default_library(package%library)
endif

! Populate executable in case we find the default app directory
if (.not.allocated(package%executable) .and. exists("app")) then
    allocate(package%executable(1))
    call default_executable(package%executable(1), package%name)
endif

if (.not.(allocated(package%library) .or. allocated(package%executable))) then
    print '(a)', "Neither library nor executable found, there is nothing to do"
    error stop 1
endif

linking = ""
if (allocated(package%library)) then
    call list_files(package%library%source_dir, files)
    do i = 1, size(files)
        if (str_ends_with(files(i)%s, ".f90")) then
            n = len(files(i)%s)
            basename = files(i)%s
            call run("gfortran -c " // inc // options // package%library%source_dir // "/" // &
               & basename // " -o " // builddir // sep // basename // ".o")
            linking = linking // " " // builddir // sep // basename // ".o"
        endif
    enddo
endif

if(size(files).ne.0)then
   call run('ar rv ' // builddir // '/lib' // package%name // ' ' // builddir // '/*.o')
endif

appdir= 'build/' // release_name // '/app/' // package%name
call ifmkdir(appdir)
do i = 1, size(package%executable)
    basename = package%executable(i)%main
    call run("gfortran -c " // inc // options // package%executable(i)%source_dir // "/" // &
       & basename // " -o " // appdir // sep // basename // ".o")
    call run("gfortran " // inc // options // appdir // sep //basename // ".o " // linking // " -o " // &
       & appdir// sep //package%executable(i)%name)
enddo
end subroutine cmd_build
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_install()
    print *, "fpm error: 'fpm install' not implemented."
    error stop 1
end subroutine cmd_install
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_new() ! --with-executable F --with-test F '
use environment, only : system_perror
use environment, only : system_mkdir, system_chdir, splitpath
use environment, only : R_GRP,R_OTH,R_USR,RWX_G,RWX_O
use environment, only : RWX_U,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!type(package_t) :: package
!!type(error_t), allocatable  :: error
integer :: ierr
character(len=:),allocatable :: dirname        ! name of directory specified on "new" subcommand
character(len=:),allocatable :: basename       ! baeename of dirname
!character(len=:),allocatable :: writethis(:)
!character(len=4096)          :: what_happened
character(len=:),allocatable :: message(:)
character(len=:),allocatable :: littlefile(:)
logical                      :: with_executable  ! command line keyword value set by get_args(3f)
logical                      :: with_test        ! command line keyword value set by get_args(3f)
   call get_args('with-executable',with_executable)                         ! get command line arguments
   call get_args('with-test',with_test)
   select case (get_os_type())
    case  (OS_LINUX);    sep='/'
    case  (OS_MACOS);    sep='/'
    case  (OS_WINDOWS);  sep='\'
   end select
   ! assume everything unclaimed by keywords on the command line are command arguments for new command
   if(size(words).ge.2.and.len(words).gt.0)then
      dirname=trim(words(2))
   else                                                                     ! no directory name to create or update on commandline
     write(stderr,'(a)') 'fpm::new<ERROR> missing directory name'
     write(stderr,'(a)') '      usage: fpm new DIRECTORY_NAME --with-executable --with-test'
     stop
   endif
   if( system_mkdir(dirname, IANY([R_USR, W_USR, X_USR]) ) .ne. 0)then       ! make new directory 
      call system_perror('fpm::new<WARNING>'//dirname)
      !!stop
   endif
   call system_chdir(dirname,ierr)
   if( ierr .ne. 0 )then                                                     ! change to new directory
      call system_perror('fpm::new<ERROR>'//dirname)
      stop
   endif
   if( system_mkdir('src', IANY([R_USR, W_USR, X_USR]) ) .ne. 0)then         ! make new src/ directory
      call system_perror('fpm::new<WARNING>src')
      !!stop
   endif
   call splitpath(dirname,basename=basename)                                 ! get basename of directory name
   if(basename=='')then                                                      ! if updating current directory
      call system_getcwd(dirname,ierr)
      call splitpath(dirname,basename=basename)
   endif
   littlefile=[character(len=80) ::           &
    &'module '//basename,                     &
    &'  implicit none',                       &
    &'  private',                             &
    &'',                                      &
    &'  public :: say_hello',                 &
    &'contains',                              &
    &'  subroutine say_hello',                &
    &'    print *, "Hello, '//basename//'!"', &
    &'  end subroutine say_hello',            &
    &'end module '//basename]
    !! hit some weird gfortran bug when littlefile data was an argument
    call warnwrite('src/'//basename//'.f90',littlefile)
    call warnwrite('.gitignore',[character(len=80) :: 'build/*'])

!!   weird gfortran bug?? lines truncated to concatenated string length, not 80
!!   call filewrite('README.md',[character(len=80) :: '# '//basename,'My cool new project!'])

   littlefile=[character(len=80) :: '# '//basename, 'My cool new project!']
   call warnwrite('README.md',littlefile)

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

   if(with_test)then
      message=[character(len=80) ::  message, &                                ! create next section of fpm.toml
       &'[[test]]                             ', &
       &'name="runTests"                      ', &
       &'source-dir="test"                    ', &
       &'main="main.f90"                      ', &
       &'']
      if( system_mkdir('test', IANY([R_USR, W_USR, X_USR]) ) .ne. 0)then       ! make new directory or stop
         call system_perror('fpm::new<WARNING>test')
         !!stop
      endif
      littlefile=[character(len=80) :: &
       &'program main',                       &
       &'implicit none',                      &
       &'',                                   &
       &'print *, "Put some tests in here!"', &
       &'end program main']
      call warnwrite('test/main.f90',littlefile)
   endif

   if(with_executable)then
      message=[character(len=80) ::  message, &                               ! create next section of fpm.toml
       &'[[executable]]                       ', &
       &'name="'//basename//'"                ', &
       &'source-dir="app"                     ', &
       &'main="main.f90"                      ', &
       &'']
      if( system_mkdir('app', IANY([R_USR, W_USR, X_USR]) ) .ne. 0)then       ! make new directory or stop
         call system_perror('fpm::new<WARNING>app')
         !!stop
      endif
      littlefile=[character(len=80) :: &
       &'program main',                          &
       &'  use '//basename//', only: say_hello', &
       &'',                                      &
       &'  implicit none',                       &
       &'',                                      &
       &'  call say_hello',                      &
       &'end program main']
      call warnwrite('app/main.f90',littlefile)
   endif

   call warnwrite('fpm.toml',message)

   call run('git init')                                                      ! assumes git(1) is installed and in command path
   !!call run('git add .')
   !!call run('git commit -m "initialized repo"')
   contains
!===================================================================================================================================
   subroutine warnwrite(fname,data)
   character(len=*),intent(in) :: fname
   character(len=*),intent(in) :: data(:)
   if(.not.exists(fname))then
      call filewrite(fname,data)
   else
      write(*,'(*(g0,1x))')'fpm::new<WARNING>',fname,'already exists. Not overwriting'
   endif
   end subroutine warnwrite
end subroutine cmd_new
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_run()
character(len=:),allocatable  :: release_name, args, cmd
logical                       :: release
integer                       :: i
type(package_t)               :: package
type(error_t), allocatable    :: error
   call get_package_data(package, "fpm.toml", error)
   if (allocated(error)) then
       print '(a)', error%message
       error stop 1
   endif
   call get_args('args',args)
   args=args//remaining
   call get_args('release',release)
   release_name=trim(merge('gfortran_release','gfortran_debug  ',release))
   if(size(words).eq.1)then
      words=[character(len=max(len(words),len(package%name))) :: words,package%name]
   endif
   do i=2,size(words)
      cmd='build/' // release_name // '/app/' // words(i)
      if(exists(cmd))then
         call run(cmd//' '//args)
      else
         !!call cmd_build()
         if(exists(cmd))then
            call run(cmd//' '//args)
         else
            write(*,*)'fpm::run<ERROR>',cmd,' not found'
         endif
      endif
   enddo
end subroutine cmd_run
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_test()
character(len=:),allocatable :: release_name
logical                      :: release
character(len=:),allocatable :: args
integer                      :: i
    call get_args('args',args)
    args=args//remaining
    call get_args('release',release)
    release_name=trim(merge('gfortran_release','gfortran_debug  ',release))
    write(*,*)'RELEASE_NAME=',release_name,' ARGS=',args
    write(*,*)'SPECIFICALLY NAMED'
    do i=2,size(words)
       write(*,*)words(i)
    enddo
    print *, "fpm error: 'fpm test' not implemented."
    error stop 1
end subroutine cmd_test
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module fpm
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
