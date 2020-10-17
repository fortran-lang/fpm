program new_test
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use fpm_filesystem,  only : is_dir, list_files, exists, windows_path
use fpm_strings,     only : string_t, operator(.in.)
use fpm_environment, only : run, get_os_type 
use fpm_environment, only : OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_WINDOWS
type(string_t), allocatable    :: file_names(:)
integer                        :: i, j, k
character(len=*),parameter     :: cmdpath = 'build/gfortran_debug/app/fpm'
character(len=:),allocatable   :: path
character(len=*),parameter     :: cmds(*) = [character(len=80) :: &
' new', &
' new no-no', &
' new A', &
' new B --lib', &
' new C --app', &
' new D --test', &
' new E --lib --test ', &
' new F --lib --app', &
' new G --test --app', &
' new BB --lib', &
' new BB --test ', &
' new BB --backfill --test', &
' new CC --test --src --app', &
' new --version', &
' new --help']
integer :: estat, cstat
character(len=256)            :: message
character(len=:),allocatable  :: directories(:)
character(len=:),allocatable  :: expected(:)
logical,allocatable           :: tally(:)
logical                       :: IS_OS_WINDOWS
   write(*,'(g0:,1x)')'TEST new SUBCOMMAND (draft):'
   allocate(tally(0))
   directories=[character(len=80) :: 'A','B','C','D','E','F','G','BB','CC']

   do i=1,size(directories)
      if( is_dir(trim(directories(i))) ) then
         write(*,*)'ERROR:',trim( directories(i) ),' already exists'
         write(*,*)'        you must remove scratch directories before performing this test'
         write(*,'(*(g0:,1x))')'directories:',(trim(directories(j)),j=1,size(directories)),'no-no'
         stop
      endif
   enddo

   !! SEE IF EXPECTED FILES ARE GENERATED
   !! Issues:
   !! o  assuming fpm command is in expected path and the new version
   !! o  DOS versus POSIX filenames
   is_os_windows=.false.
    select case (get_os_type()) 
    case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
       path=cmdpath
    case (OS_WINDOWS) 
       path=windows_path(cmdpath)
       is_os_windows=.true.
    case default
       write(*,*)'ERROR: unknown OS. Stopping test'
       stop 2
    end select 
   ! execute the fpm(1) commands
   do i=1,size(cmds)
      message=''
      write(*,*)path//' '//cmds(i)
      call execute_command_line(path//' '//cmds(i),exitstat=estat,cmdstat=cstat,cmdmsg=message)
      write(*,'(*(g0))')'CMD=',trim(cmds(i)),' EXITSTAT=',estat,' CMDSTAT=',cstat,' MESSAGE=',trim(message)
   enddo

   if( is_dir('no-no') ) then
       tally=[tally,.false.]
       write(*,*)'ERROR: directory no-no/ exists'
    else
       tally=[tally,.true.]
    endif

   ! assuming hidden files in .git and .gitignore are ignored for now
   TESTS: do i=1,size(directories)
      ! test if expected directory exists
      if( .not. is_dir(trim(directories(i))) ) then
         tally=[tally,.false.]
         write(*,*)'ERROR:',trim( directories(i) ),' is not a directory'
      else
         select case(directories(i))
         case('A');  expected=[ character(len=80)::&
          &'A/app','A/fpm.toml','A/README.md','A/src','A/test','A/app/main.f90','A/src/A.f90','A/test/main.f90']
         case('B');  expected=[ character(len=80)::&
          &'B/fpm.toml','B/README.md','B/src','B/src/B.f90']
         case('C');  expected=[ character(len=80)::&
          &'C/app','C/fpm.toml','C/README.md','C/app/main.f90']
         case('D');  expected=[ character(len=80)::&
          &'D/fpm.toml','D/README.md','D/test','D/test/main.f90']
         case('E');  expected=[ character(len=80)::&
          &'E/fpm.toml','E/README.md','E/src','E/test','E/src/E.f90','E/test/main.f90']
         case('F');  expected=[ character(len=80)::&
          &'F/app','F/fpm.toml','F/README.md','F/src','F/app/main.f90','F/src/F.f90']
         case('G');  expected=[ character(len=80)::&
          &'G/app','G/fpm.toml','G/README.md','G/test','G/app/main.f90','G/test/main.f90']
         case('BB'); expected=[ character(len=80)::&
          &'BB/fpm.toml','BB/README.md','BB/src','BB/test','BB/src/BB.f90','BB/test/main.f90']
         case('CC'); expected=[ character(len=80)::&
          &'CC/app','CC/fpm.toml','CC/README.md','CC/src','CC/test','CC/app/main.f90','CC/src/CC.f90','CC/test/main.f90']
         case default
            write(*,*)'ERROR: internal error. unknown directory name ',trim(directories(i))
            stop 4
         end select
         !! MSwindows has hidden files in it
         !! Warning: This only looks for expected files. If there are more files than expected it does not fail
         call list_files(trim(directories(i)), file_names,recurse=.true.)

         if(size(expected).ne.size(file_names))then
            write(*,*)'WARNING: unexpected number of files in file list=',size(file_names),' expected ',size(expected)
            write(*,'("EXPECTED: ",*(g0:,","))')(trim(expected(j)),j=1,size(expected))
            write(*,'("FOUND:    ",*(g0:,","))')(trim(file_names(j)%s),j=1,size(file_names))
         endif

         do j=1,size(expected)
            
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

   write(*,'("TALLY=",*(g0))')tally
   if(all(tally))then
      write(*,'(*(g0))')'PASSED: all ',count(tally),' tests passed '
   else
      write(*,*)'FAILED: PASSED=',count(tally),' FAILED=',count(.not.tally)
      stop 5
   endif

end program new_test
