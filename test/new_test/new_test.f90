program new_test
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use fpm_filesystem,  only : is_dir, list_files, exists, windows_path, join_path, &
  dirname, run
use fpm_strings,     only : string_t, operator(.in.)
use fpm_environment, only : get_os_type
use fpm_environment, only : OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD, OS_WINDOWS
implicit none
type(string_t), allocatable    :: file_names(:)
integer                        :: i, j, k
character(len=:),allocatable   :: cmdpath
character(len=:),allocatable   :: path
character(len=*),parameter     :: scr = 'fpm_scratch_'
character(len=*),parameter     :: cmds(*) = [character(len=80) :: &
! run a variety of "fpm new" variations and verify expected files are generated
'new', &
'new name-with-hyphens', &
'new '//scr//'A', &
'new '//scr//'B --lib', &
'new '//scr//'C --app', &
'new '//scr//'D --test', &
'new '//scr//'E --lib --test ', &
'new '//scr//'F --lib --app', &
'new '//scr//'G --test --app', &
'new '//scr//'H --example', &
'new '//scr//'BB --lib', &
'new '//scr//'BB --test ', &
'new '//scr//'BB --backfill --test', &
'new '//scr//'CC --test --src --app', &
'new --version', &
'new --help']
integer :: estat, cstat
character(len=256)            :: message
character(len=:),allocatable  :: directories(:)
character(len=:),allocatable  :: shortdirs(:)
character(len=:),allocatable  :: expected(:)
logical,allocatable           :: tally(:)
logical                       :: IS_OS_WINDOWS
character(len=*),parameter    :: dirs_to_be_removed = 'fpm_scratch_* name-with-hyphens'
character(len=:),allocatable  :: rm_command
   write(*,'(g0:,1x)')'TEST new SUBCOMMAND (draft):'

   cmdpath = get_command_path()
   allocate(tally(0))
   shortdirs=[character(len=80) :: 'A','B','C','D','E','F','G','H','BB','CC']
   allocate(character(len=80) :: directories(size(shortdirs)))

   !! SEE IF EXPECTED FILES ARE GENERATED
   !! Issues:
   !! o  assuming fpm command is in expected path and the new version
   !! o  DOS versus POSIX filenames
   is_os_windows=.false.
    select case (get_os_type())
    case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
       call execute_command_line('rm -rf fpm_scratch_*',exitstat=estat,cmdstat=cstat,cmdmsg=message)
       path=cmdpath
    case (OS_WINDOWS)
       path=windows_path(cmdpath)
       is_os_windows=.true.
       do i=1,size(directories)
          call execute_command_line('rmdir /s /q fpm_scratch_'//trim(shortdirs(i)),exitstat=estat,&
             cmdstat=cstat,cmdmsg=message)
       end do
    case default
       write(*,*)'ERROR: unknown OS. Stopping test'
       stop 2
    end select
   do i=1,size(directories)
      directories(i)=scr//trim(shortdirs(i))
      if( is_dir(trim(directories(i))) ) then
         write(*,*)'ERROR:',trim( directories(i) ),' already exists'
         write(*,*)'        you must remove scratch directories before performing this test'
         write(*,'(*(g0:,1x))')'directories:',(trim(directories(j)),j=1,size(directories)),'name-with-hyphens'
         stop
      endif
   enddo
   ! execute the fpm(1) commands
   do i=1,size(cmds)
      message=''
      write(*,*)path//' '//cmds(i)
      call execute_command_line(path//' '//cmds(i),exitstat=estat,cmdstat=cstat,cmdmsg=message)
      write(*,'(*(g0))')'CMD=',trim(cmds(i)),' EXITSTAT=',estat,' CMDSTAT=',cstat,' MESSAGE=',trim(message)
   enddo

   if( is_dir('name-with-hyphens') ) then
       tally=[tally,.true.]

    else
       write(*,*)'ERROR: directory name-with-hyphens/ exists'
       tally=[tally,.false.]
    endif

   ! assuming hidden files in .git and .gitignore are ignored for now
   TESTS: do i=1,size(directories)
      ! test if expected directory exists
      if( .not. is_dir(trim( directories(i))) ) then
         tally=[tally,.false.]
         write(*,*)'ERROR:',trim( directories(i) ),' is not a directory'
      else
         select case(shortdirs(i))
         case('A');  expected=[ character(len=80)::&
          &'A/app','A/fpm.toml','A/README.md','A/src','A/test','A/app/main.f90','A/src/'//scr//'A.f90','A/test/check.f90']
         case('B');  expected=[ character(len=80)::&
          &'B/fpm.toml','B/README.md','B/src','B/src/'//scr//'B.f90']
         case('C');  expected=[ character(len=80)::&
          &'C/app','C/fpm.toml','C/README.md','C/app/main.f90']
         case('D');  expected=[ character(len=80)::&
          &'D/fpm.toml','D/README.md','D/test','D/test/check.f90']
         case('E');  expected=[ character(len=80)::&
          &'E/fpm.toml','E/README.md','E/src','E/test','E/src/'//scr//'E.f90','E/test/check.f90']
         case('F');  expected=[ character(len=80)::&
          &'F/app','F/fpm.toml','F/README.md','F/src','F/app/main.f90','F/src/'//scr//'F.f90']
         case('G');  expected=[ character(len=80)::&
          &'G/app','G/fpm.toml','G/README.md','G/test','G/app/main.f90','G/test/check.f90']
         case('H');  expected=[ character(len=80)::&
          &'H/example','H/fpm.toml','H/README.md','H/example/demo.f90']
         case('BB'); expected=[ character(len=80)::&
          &'BB/fpm.toml','BB/README.md','BB/src','BB/test','BB/src/'//scr//'BB.f90','BB/test/check.f90']
         case('CC'); expected=[ character(len=80)::&
          &'CC/app','CC/fpm.toml','CC/README.md','CC/src','CC/test','CC/app/main.f90','CC/src/'//scr//'CC.f90','CC/test/check.f90']
         case default
            write(*,*)'ERROR: internal error. unknown directory name ',trim(shortdirs(i))
            stop 4
         end select
         !! MSwindows has hidden files in it
         !! Warning: This only looks for expected files. If there are more files than expected it does not fail
         call list_files(trim(directories(i)), file_names,recurse=.true.)

         if(size(expected)/=size(file_names))then
            write(*,*)'WARNING: unexpected number of files in file list=',size(file_names),' expected ',size(expected)
            write(*,'("EXPECTED: ",*(g0:,","))')(scr//trim(expected(j)),j=1,size(expected))
            write(*,'("FOUND:    ",*(g0:,","))')(trim(file_names(j)%s),j=1,size(file_names))
         endif

         do j=1,size(expected)

            expected(j)=scr//expected(j)
            if(is_os_windows) expected(j)=windows_path(expected(j))
            if( .not.(trim(expected(j)).in.file_names) )then
                tally=[tally,.false.]
                write(*,'("ERROR: FOUND ",*(g0:,", "))')( trim(file_names(k)%s), k=1,size(file_names) )
                write(*,'(*(g0))')'       BUT NO MATCH FOR ',expected(j)
                tally=[tally,.false.]
                cycle TESTS
             endif
         enddo
         tally=[tally,.true.]
      endif
   enddo TESTS

   ! Test that generated manifests are valid by building one of the packages
   write(*,'(a)') ' <INFO> Testing that generated manifest is valid (can be built)...'
   call execute_command_line(path//' build '//scr//'A', exitstat=estat, cmdstat=cstat, cmdmsg=message)
   if (estat == 0) then
      write(*,'(a)') ' <INFO> Generated manifest is valid - build succeeded'
      tally = [tally, .true.]
   else
      write(*,'(a,i0)') ' ERROR: Generated manifest is invalid - build failed with exit status ', estat
      tally = [tally, .false.]
   end if

   ! clean up scratch files; might want an option to leave them for inspection
   select case (get_os_type())
   case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
      rm_command = 'rm -rf ' // dirs_to_be_removed
   case (OS_WINDOWS)
      do i=1,size(directories)
         rm_command = 'rmdir /s /q fpm_scratch_'//trim(shortdirs(i))
         call execute_command_line('rmdir /s /q fpm_scratch_'//trim(shortdirs(i)),exitstat=estat,&
            cmdstat=cstat,cmdmsg=message)
      end do
      rm_command = 'rmdir /s /q name-with-hyphens'
   end select
   call execute_command_line(rm_command, exitstat=estat,cmdstat=cstat,cmdmsg=message)

   write(*,'("new TEST TALLY=",*(g0))')tally
   if(all(tally))then
      write(*,'(*(g0))')'PASSED: all ',count(tally),' tests passed '
   else
      write(*,*)'FAILED: PASSED=',count(tally),' FAILED=',count(.not.tally)
      stop 5
   endif
contains
  function get_command_path() result(prog)
    character(len=:), allocatable :: prog

    character(len=:), allocatable :: path
    integer :: length

    ! FIXME: Super hacky way to get the name of the fpm executable,
    ! it works better than invoking fpm again but should be replaced ASAP.
    call get_command_argument(0, length=length)
    allocate(character(len=length) :: prog)
    call get_command_argument(0, prog)
    path = dirname(prog)
    if (get_os_type() == OS_WINDOWS) then
      prog = join_path(path, "..", "app", "fpm.exe")
      if (.not.exists(prog)) then
        prog = join_path(path, "..", "..", "app", "fpm.exe")
      end if
    else
      prog = join_path(path, "..", "app", "fpm")
      if (.not.exists(prog)) then
        prog = join_path(path, "..", "..", "app", "fpm")
      end if
    end if

  end function
end program new_test
