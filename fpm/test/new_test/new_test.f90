program new_test
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use fpm_filesystem,  only : is_dir, list_files, exists
use fpm_strings,     only : string_t 
use fpm_environment, only : run, get_os_type 
use fpm_environment, only : OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_WINDOWS
type(string_t), allocatable    :: file_names(:)
character(len=:), allocatable  :: fnames(:)
character(len=:), allocatable  :: directory
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
   !! DOS versus POSIX filenames
   ! assuming fpm command is in path and the new version
    select case (get_os_type()) 
    case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
       path=cmdpath
    case (OS_WINDOWS) 
       path=u2d(cmdpath)
    case default
       write(*,*)'ERROR: unknown OS. Stopping test'
       stop 2
    end select 


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
         case('A')
          expected=[ character(len=80)::&
          &'A/app','A/fpm.toml','A/README.md','A/src','A/test','A/app/main.f90','A/src/A.f90','A/test/main.f90']
         case('B')
          expected=[ character(len=80)::&
          &'B/fpm.toml','B/README.md','B/src','B/src/B.f90']
         case('C')
          expected=[ character(len=80)::&
          &'C/app','C/fpm.toml','C/README.md','C/app/main.f90']
         case('D')
          expected=[ character(len=80)::&
          &'D/fpm.toml','D/README.md','D/test','D/test/main.f90']
         case('E')
          expected=[ character(len=80)::&
          &'E/fpm.toml','E/README.md','E/src','E/test','E/src/E.f90','E/test/main.f90']
         case('F')
          expected=[ character(len=80)::&
          &'F/app','F/fpm.toml','F/README.md','F/src','F/app/main.f90','F/src/F.f90']
         case('G')
          expected=[ character(len=80)::&
          &'G/app','G/fpm.toml','G/README.md','G/test','G/app/main.f90','G/test/main.f90']
         case('BB')
          expected=[ character(len=80)::&
          &'BB/fpm.toml','BB/README.md','BB/src','BB/test','BB/src/BB.f90','BB/test/main.f90']
         case('CC')
          expected=[ character(len=80)::&
          &'CC/app','CC/fpm.toml','CC/README.md','CC/src','CC/test','CC/app/main.f90','CC/src/CC.f90','CC/test/main.f90']
         case default
            write(*,*)'ERROR: internal error. unknown directory name ',trim(directories(i))
            stop 4
         end select
         !! MSwindows has hidden files in it
         call list_files(trim(directories(i)), file_names,recurse=.true.)
         if(allocated(fnames))deallocate(fnames)
         allocate(character(len=0) :: fnames(0))
         do j=1,size(file_names)
           if(file_names(j)%s(1:1).eq.'.'.or.index(file_names(j)%s,'/.').ne.0.or.index(file_names(j)%s,'\.').ne.0)cycle
           fnames=[character(len=max(len(fnames),len(file_names(j)%s))) :: fnames,file_names(j)%s]
         enddo
         write(*,'(*(g0))',advance='no')'>>>DIRECTORY ',trim(directories(i)),':   '
         write(*,'(*(g0:,", "))')( file_names(j)%s, j=1,size(file_names) )
         if(size(expected).ne.size(fnames))then
            write(*,*)'unexpected number of files in file list=',size(fnames),' expected ',size(expected)
            tally=[tally,.false.]
            cycle TESTS
         else
            select case (get_os_type()) 
            case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
            case (OS_WINDOWS) 
               do j=1,size(expected)
                  expected(j)=u2d(expected(j))
               enddo 
            case default
               write(*,*)'ERROR: unknown OS. Stopping test'
               stop 3
            end select 
            do j=1,size(expected)
               if( .not.any(fnames(j)==expected) )then
                  tally=[tally,.false.]
                  write(*,'("ERROR: EXPECTED ",*(g0:,", "))')( trim(expected(k)), k=1,size(expected) )
                  write(*,'(*(g0))')'       NO MATCH FOR ',fnames(j)
                  cycle TESTS
               endif
            enddo
            tally=[tally,.true.]
         endif
      endif
   enddo TESTS
   write(*,'("TALLY=",*(g0))')tally
   if(all(tally))then
      write(*,'(*(g0))')'PASSED: all ',count(tally),' tests passed '
   else
      write(*,*)'FAILED: PASSED=',count(tally),' FAILED=',count(.not.tally)
      stop 5
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
function u2d(pathname) result(dos)
! simplistically replace / with \ to make posix pathname DOS pathname
character(len=*),intent(in) :: pathname
character(len=:),allocatable :: dos
integer :: i
dos=pathname
do i=1,len(pathname)
   if(pathname(i:i).eq.'/')dos(i:i)='\'
enddo
end function u2d
!-----------------------------------------------------------------------------------------------------------------------------------
function djb2_hash_arr(chars,continue) result(hash_128)
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64
implicit none

!$@(#) djb2_hash(3fp): DJB2 hash of array (algorithm by Daniel J. Bernstein ) for character array

character(len=1),intent(in)  :: chars(:)
logical,intent(in),optional  :: continue
integer                      :: i
integer(kind=int64)          :: hash_128
integer(kind=int64),save     :: hash_64=5381

   if(present(continue))then
      hash_64 = hash_64
   else
      hash_64 = 5381_int64
   endif
   do i=1,size(chars)
      hash_64 = (ishft(hash_64,5) + hash_64) + ichar(chars(i),kind=int64)
   enddo
   hash_128=transfer([hash_64,0_int64],hash_128)
      DEBUG : block
         integer :: ios
         write(6,'("*djb2_hash*       hashing string=",*(a))',advance='no')chars
         write(6,'(1x,"hash=",i0,1x,"hex hash=",z32.32)')hash_128,hash_128
         flush(6,iostat=ios)
      endblock DEBUG
end function djb2_hash_arr
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine slurp(filename,text,length,lines)
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none

!$@(#) M_io::slurp(3f): allocate text array and read file filename into it

class(*),intent(in)                      :: filename    ! filename to shlep
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer,intent(out),optional             :: length      ! length of longest line
integer,intent(out),optional             :: lines       ! number of lines

integer :: nchars=0             ! holds size of file
integer :: igetunit             ! use newunit=igetunit in f08
integer :: ios=0                ! used for I/O error status
integer :: length_local
integer :: lines_local
integer :: i
integer :: icount
character(len=256)  :: message
character(len=4096) :: local_filename

   length_local=0
   lines_local=0

   message=''
      select type(FILENAME)
       type is (character(len=*))
          open(newunit=igetunit, file=trim(filename), action="read", iomsg=message,&
           &form="unformatted", access="stream",status='old',iostat=ios)
          local_filename=filename
       type is (integer)
          rewind(unit=filename,iostat=ios,iomsg=message)
          write(local_filename,'("unit ",i0)')filename
          igetunit=filename
      end select

   if(ios.eq.0)then  ! if file was successfully opened

      inquire(unit=igetunit, size=nchars)

      if(nchars.le.0)then
         call stderr_local( '*slurp* empty file '//trim(local_filename) )
         return
      endif
      ! read file into text array
      !
      if(allocated(text))deallocate(text) ! make sure text array not allocated
      allocate ( text(nchars) )           ! make enough storage to hold file
      read(igetunit,iostat=ios,iomsg=message) text      ! load input file -> text array
      if(ios.ne.0)then
         call stderr_local( '*slurp* bad read of '//trim(local_filename)//':'//trim(message) )
      endif
   else
      call stderr_local('*slurp* '//message)
      allocate ( text(0) )           ! make enough storage to hold file
   endif

   close(iostat=ios,unit=igetunit)            ! close if opened successfully or not

   if(present(lines).or.present(length))then  ! get length of longest line and number of lines
      icount=0
      do i=1,nchars
         if(text(i).eq.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
            icount=0
         endif
         icount=icount+1
      enddo
      if(nchars.ne.0)then
         if(text(nchars).ne.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
         endif
      endif
      if(present(lines))lines=lines_local
      if(present(length))length=length_local
   endif
end subroutine slurp
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine stderr_local(message)
character(len=*) :: message
   write(stderr,'(a)')trim(message)    ! write message to standard error
end subroutine stderr_local
!-----------------------------------------------------------------------------------------------------------------------------------
end program new_test
