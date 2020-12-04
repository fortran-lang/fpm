program help_test
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer                        :: i
integer                        :: be, af
character(len=:),allocatable   :: path
integer                        :: estat, cstat
character(len=256)             :: message
logical,allocatable            :: tally(:)
character(len=1),allocatable   :: book1(:), book2(:)
!intel_bug!character(len=:),allocatable   :: page1(:)
character(len=132),allocatable   :: page1(:)
integer                        :: lines
integer                        :: chars
! run a variety of "fpm help" variations and verify expected files are generated
character(len=*),parameter     :: cmds(*) = [character(len=80) :: &
! build manual as pieces using various help commands
'fpm run -- --help        > fpm_scratch_help.txt',&
'fpm run -- help new     >> fpm_scratch_help.txt',&
'fpm run -- build --help >> fpm_scratch_help.txt',&
'fpm run -- help run     >> fpm_scratch_help.txt',&
'fpm run -- help test    >> fpm_scratch_help.txt',&
'fpm run -- help runner  >> fpm_scratch_help.txt',&
'fpm run -- help list    >> fpm_scratch_help.txt',&
'fpm run -- help help    >> fpm_scratch_help.txt',&
'fpm run -- --version    >> fpm_scratch_help.txt',&
! generate manual
'fpm run -- help manual   > fpm_scratch_manual.txt']

!'fpm run             >> fpm_scratch_help.txt',&
!'fpm run -- --list       >> fpm_scratch_help.txt',&
!'fpm run -- list --list  >> fpm_scratch_help.txt',&
character(len=*),parameter :: names(*)=[character(len=10) :: 'fpm','new','build','run','test','runner','list','help']

   write(*,'(g0:,1x)')'<INFO>TEST help SUBCOMMAND STARTED'
   if(allocated(tally))deallocate(tally)
   allocate(tally(0))
   call wipe('fpm_scratch_help.txt')
   call wipe('fpm_scratch_manual.txt')

   ! check that output has NAME SYNOPSIS DESCRIPTION
   do i=1,size(names)
      write(*,*)'<INFO>check '//names(i)//' for NAME SYNOPSIS DESCRIPTION'
      path= 'fpm run -- help '//names(i)//' >fpm_scratch_help.txt'
      message=''
      call execute_command_line(path,exitstat=estat,cmdstat=cstat,cmdmsg=message)
      write(*,'(*(g0))')'<INFO>CMD=',path,' EXITSTAT=',estat,' CMDSTAT=',cstat,' MESSAGE=',trim(message)
      tally=[tally,all([estat.eq.0,cstat.eq.0])]
      call swallow('fpm_scratch_help.txt',page1)
      if(size(page1).lt.3)then
         write(*,*)'<ERROR>help for '//names(i)//' ridiculiously small'
         tally=[tally,.false.]
         exit
      endif
      !!write(*,*)findloc(page1,'NAME').eq.1
      be=count(.not.tally)
      tally=[tally,merge(.true.,.false.,count(page1.eq.'NAME').eq.1)]
      tally=[tally,merge(.true.,.false.,count(page1.eq.'SYNOPSIS').eq.1)]
      tally=[tally,merge(.true.,.false.,count(page1.eq.'DESCRIPTION').eq.1)]
      af=count(.not.tally)
      if(be.ne.af)then
         write(*,*)'<ERROR>missing expected sections in ',names(i)
         write(*,'(a)')page1
      endif
      write(*,*)'<INFO>have completed ',count(tally),' tests'
      call wipe('fpm_scratch_help.txt')
      call wipe('fpm_scratch_manual.txt')
   enddo


   ! execute the fpm(1) commands
   do i=1,size(cmds)
      message=''
      path= cmds(i)
      call execute_command_line(path,exitstat=estat,cmdstat=cstat,cmdmsg=message)
      write(*,'(*(g0))')'<INFO>CMD=',path,' EXITSTAT=',estat,' CMDSTAT=',cstat,' MESSAGE=',trim(message)
      tally=[tally,all([estat.eq.0,cstat.eq.0])]
   enddo

   ! compare book written in fragments with manual
   call slurp('fpm_scratch_help.txt',book1)
   call slurp('fpm_scratch_manual.txt',book2)
   write(*,*)'<INFO>book1 ',size(book1), len(book1)
   write(*,*)'<INFO>book2 ',size(book2), len(book2)
   !if(size(book1).ne.size(book2))then
   !      write(*,*)'<ERROR>manual and appended pages are not the same size'
   !      tally=[tally,.false.]
   !else
   !   if(all(book1.ne.book2))then
   !      tally=[tally,.false.]
   !      write(*,*)'<ERROR>manual and appended pages are not the same'
   !   else
   !      write(*,*)'<INFO>manual and appended pages are the same'
   !      tally=[tally,.true.]
   !   endif
   !endif

   ! overall size of manual
   chars=size(book2)
   lines=max(count(char(10).eq.book2),count(char(13).eq.book2))
   if( (chars.lt.13000) .or. (lines.lt.350) )then
      write(*,*)'<ERROR>manual is suspiciously small, bytes=',chars,' lines=',lines
      tally=[tally,.false.]
   else
      write(*,*)'<INFO>manual size is bytes=',chars,' lines=',lines
      tally=[tally,.true.]
   endif

   write(*,'("<INFO>HELP TEST TALLY=",*(g0))')tally
   call wipe('fpm_scratch_help.txt')
   call wipe('fpm_scratch_manual.txt')
   if(all(tally))then
      write(*,'(*(g0))')'<INFO>PASSED: all ',count(tally),' tests passed '
   else
      write(*,*)'<INFO>FAILED: PASSED=',count(tally),' FAILED=',count(.not.tally)
      stop 5
   endif
   write(*,'(g0:,1x)')'<INFO>TEST help SUBCOMMAND COMPLETE'
contains

subroutine wipe(filename)
character(len=*),intent(in) :: filename
integer :: ios
integer :: lun
character(len=256) :: message
open(file=filename,newunit=lun,iostat=ios,iomsg=message)
if(ios.eq.0)then
   close(unit=lun,iostat=ios,status='delete',iomsg=message)
   if(ios.ne.0)then
      write(*,*)'<ERROR>'//trim(message)
   endif
else
   write(*,*)'<ERROR>'//trim(message)
endif
end subroutine wipe

subroutine slurp(filename,text)
implicit none
!$@(#) M_io::slurp(3f): allocate text array and read file filename into it
character(*),intent(in)                  :: filename    ! filename to shlep
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer                                  :: nchars, igetunit, ios
character(len=256)                       :: message
character(len=4096)                      :: local_filename
   ios=0
   nchars=0
   message=''
   open(newunit=igetunit, file=trim(filename), action="read", iomsg=message,&
    &form="unformatted", access="stream",status='old',iostat=ios)
   local_filename=filename
   if(ios.eq.0)then  ! if file was successfully opened
      inquire(unit=igetunit, size=nchars)
      if(nchars.le.0)then
         call stderr_local( '*slurp* empty file '//trim(local_filename) )
         return
      endif
      ! read file into text array
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
end subroutine slurp

subroutine stderr_local(message)
character(len=*) :: message
   write(*,'(a)')trim(message)    ! write message to standard error
end subroutine stderr_local

subroutine swallow(FILENAME,pageout)
implicit none
character(len=*),intent(in)              :: FILENAME   ! file to read
!intel-bug!character(len=:),allocatable,intent(out) :: pageout(:) ! page to hold file in memory
character(len=132),allocatable,intent(out) :: pageout(:) ! page to hold file in memory
character(len=1),allocatable             :: text(:)    ! array to hold file in memory

   call slurp(FILENAME,text) ! allocate character array and copy file into it

   if(.not.allocated(text))then
       write(*,*)'<ERROR>*swallow* failed to load file '//FILENAME
   else  ! convert array of characters to array of lines
      pageout=page(text)
      deallocate(text)     ! release memory
   endif
end subroutine swallow

function page(array)  result (table)

!$@(#) M_strings::page(3fp): function to copy char array to page of text

character(len=1),intent(in)  :: array(:)
!intel-bug!character(len=:),allocatable :: table(:)
character(len=132),allocatable :: table(:)
integer                      :: i
integer                      :: linelength
integer                      :: length
integer                      :: lines
integer                      :: linecount
integer                      :: position
integer                      :: sz
!!character(len=1),parameter   :: nl=new_line('A')
character(len=1),parameter   :: nl=char(10)
   lines=0
   linelength=0
   length=0
   sz=size(array)
   do i=1,sz
      if(array(i).eq.nl)then
         linelength=max(linelength,length)
         lines=lines+1
         length=0
      else
         length=length+1
      endif
   enddo
   if(sz.gt.0)then
      if(array(sz).ne.nl)then
         lines=lines+1
      endif
   endif

   if(allocated(table))deallocate(table)
   !intel-bug!allocate(character(len=linelength) :: table(lines))
   allocate(character(len=132) :: table(lines))
   table=' '

   linecount=1
   position=1
   do i=1,sz
      if(array(i).eq.nl)then
         linecount=linecount+1
         position=1
      elseif(linelength.ne.0)then
         write(*,*)'<INFO>',linecount,position,array(i)
         table(linecount)(position:position)=array(i)
         position=position+1
      endif
   enddo
end function page

end program help_test
