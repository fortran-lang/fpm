program main

! for each set of command options, call this command recursively which will print the resulting parameters with a
! given test command CMD from the TEST() array.
!
! Then read the expected values as a NAMELIST group from the test array and compare the expected
! results with the actual results.
!
! the PARSE() subroutine is a copy of the app/main.f90 program except it creates and writes a NAMELIST file instead
! of actually calling the subcommands.
!
! The program will exit with a non-zero status if any of the tests fail

use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
implicit none

! convenient arbitrary sizes for test

! assuming no name over 15 characters to make output have shorter lines
character(len=15),allocatable        :: name(:),act_name(:)  ; namelist/act_cli/act_name
integer,parameter                    :: max_names=10

character(len=:),allocatable         :: command
character(len=:),allocatable         :: cmd
integer                              :: cstat, estat
integer                              :: act_cstat, act_estat
integer                              :: i, ios
logical                              :: w_e,act_w_e          ; namelist/act_cli/act_w_e
logical                              :: w_t,act_w_t          ; namelist/act_cli/act_w_t

logical                              :: release,act_release  ; namelist/act_cli/act_release
character(len=:),allocatable         :: args,act_args        ; namelist/act_cli/act_args
namelist/expected/cmd,cstat,estat,w_e,w_t,name,release,args
integer                              :: lun
logical,allocatable                  :: tally(:)
logical,allocatable                  :: subtally(:)
character(len=256)                   :: message

! table of arguments to pass to program and expected non-default values for that execution in NAMELIST group format
character(len=*),parameter           :: tests(*)= [ character(len=256) :: &

'CMD="new",                                           ESTAT=1,', &
!'CMD="new -unknown",                                  ESTAT=2,', &
'CMD="new my_project another yet_another -with-test", ESTAT=2,', &
'CMD="new my_project --with-executable",              W_E=T,       NAME="my_project",',  &
'CMD="new my_project --with-executable -with-test",   W_E=T,W_T=T, NAME="my_project",',  &
'CMD="new my_project -with-test",                           W_T=T, NAME="my_project",',  &
'CMD="new my_project",                                             NAME="my_project",',  &

'CMD="run",                                                                                               ', &
'CMD="run my_project",                                             NAME="my_project",                     ', &
'CMD="run proj1 p2 project3",                                      NAME="proj1","p2","project3",          ', &
'CMD="run proj1 p2 project3 --release",                            NAME="proj1","p2","project3",RELEASE=T,', &
'CMD="run proj1 p2 project3 --release -- arg1 -x ""and a long one""", &
   &NAME="proj1","p2","project3",RELEASE=T ARGS="""arg1"" -x ""and a long one""",                         ', &

'CMD="test",                                                                                              ', &
'CMD="test my_project",                                            NAME="my_project",                     ', &
'CMD="test proj1 p2 project3",                                     NAME="proj1","p2","project3",          ', &
'CMD="test proj1 p2 project3 --release",                           NAME="proj1","p2","project3",RELEASE=T,', &
'CMD="test proj1 p2 project3 --release -- arg1 -x ""and a long one""", &
   &NAME="proj1","p2","project3",RELEASE=T ARGS="""arg1"" -x ""and a long one""",                         ', &

'CMD="build",                                                      NAME= RELEASE=F,ARGS="",', &
'CMD="build --release",                                            NAME= RELEASE=T,ARGS="",', &
' ' ]
character(len=256) :: readme(3)

readme(1)='&EXPECTED'  ! top and bottom line for a NAMELIST group read from TEST() used to set the expected values
readme(3)=' /'
tally=[logical ::]     ! an array that tabulates the command test results as pass or fail.

if(command_argument_count().eq.0)then  ! assume if called with no arguments to do the tests. This means you cannot
                                       ! have a test of no parameters. Could improve on this.
                                       ! if called with parameters assume this is a test and call the routine to
                                       ! parse the resulting values after calling the CLI command line parser
                                       ! and write the NAMELIST group so it can be read and tested against the
                                       ! expected results
   write(*,*)'start tests of the CLI command line parser'
   command=repeat(' ',4096)
   call get_command_argument(0,command)
   command=trim(command)
   write(*,*)'command=',command

   do i=1,size(tests)
      if(tests(i).eq.' ')then
         open(file='_test_cli',newunit=lun,delim='quote')
         close(unit=lun,status='delete')
         exit
      endif
      ! blank out name group EXPECTED
      name=[(repeat(' ',len(name)),i=1,max_names)] ! the words on the command line sans the subcommand name
      release=.false.                ! --release
      w_e=.false.                    ! --with-executable
      w_t=.false.                    ! --with-test
      args=repeat(' ',132)           ! -- ARGS
      cmd=repeat(' ',132)            ! the command line arguments to test
      cstat=0                        ! status values from EXECUTE_COMMAND_LINE()
      estat=0
      readme(2)=' '//tests(i)        ! select command options to test for CMD and set nondefault expected values
      read(readme,nml=expected)

      write(*,'(*(g0))')'START:  TEST ',i,' CMD=',trim(cmd)
      ! call this program which will crack command line and write results to scratch file _test_cli
      call execute_command_line(command//' '//trim(cmd),cmdstat=act_cstat,exitstat=act_estat)
      if(cstat.eq.act_cstat.and.estat.eq.act_estat)then
          if(estat.eq.0)then
             open(file='_test_cli',newunit=lun,delim='quote')
             act_name=[(repeat(' ',len(act_name)),i=1,max_names)]
             act_release=.false.
             act_w_e=.false.
             act_w_t=.false.
             act_args=repeat(' ',132)
             read(lun,nml=act_cli,iostat=ios,iomsg=message)
             if(ios.ne.0)then
                write(*,'(a)')'ERROR:',trim(message)
             endif
             close(unit=lun)
             ! compare results to expected values
             subtally=[logical ::]
             call test_test('NAME',all(act_name.eq.name))
             call test_test('RELEASE',act_release.eqv.release)
             call test_test('WITH_EXPECTED',act_w_e.eqv.w_e)
             call test_test('WITH_TESTED',act_w_t.eqv.w_t)
             call test_test('WITH_TEST',act_w_t.eqv.w_t)
             call test_test('ARGS',act_args.eq.args)
             if(all(subtally))then
                write(*,'(*(g0))')'PASSED: TEST ',i,' STATUS: expected ',cstat,' ',estat,' actual ',act_cstat,' ',act_estat,&
                & ' for [',trim(cmd),']'
                tally=[tally,.true.]
             else
                write(*,'(*(g0))')'FAILED: TEST ',i,' STATUS: expected ',cstat,' ',estat,' actual ',act_cstat,' ',act_estat,&
                & ' for [',trim(cmd),']'
                print '(4a)', &
                   'This file was compiled by ', &
                   compiler_version(),           &
                   ' using the options ',        &
                   compiler_options()
                write(*,nml=act_cli,delim='quote')
                tally=[tally,.false.]
             endif
          else
             write(*,'(*(g0))')'PASSED: TEST ',i,' EXPECTED BAD STATUS: expected ',cstat,' ',estat, &
             ' actual ',act_cstat,' ',act_estat,' for [',trim(cmd),']'
             tally=[tally,.true.]
          endif
      else
          write(*,'(*(g0))')'FAILED: TEST ',i,'BAD STATUS: expected ',cstat,' ',estat,' actual ',act_cstat,' ',act_estat,&
          ' for [',trim(cmd),']'
          tally=[tally,.false.]
      endif
   enddo
   ! write up total results and if anything failed exit with a non-zero status
   write(*,'(*(g0))')'TALLY;',tally
   if(all(tally))then
      write(*,'(*(g0))')'PASSED: all ',count(tally),' tests passed '
   else
      write(*,*)'FAILED: PASSED=',count(tally),' FAILED=',count(.not.tally)
      stop 4
   endif
else
   ! call this program with arguments
   !=============================================
   debugit: block
   integer :: j, ilen
   character(len=256) :: big_argument
   write(*,*)'arguments seen directly by program'
   do j=1,command_argument_count()
      call get_command_argument(number=j,value=big_argument,length=ilen)
      write(*,'(*(g0))')j,'[',big_argument(:ilen),']'
   enddo
   end block debugit
   !=============================================
   call parse()
endif

contains

subroutine test_test(name,tst)
character(len=*) :: name
logical,intent(in) :: tst
   !!write(*,'(*(g0,1x))')'        SUBTEST ',name,' ',merge('PASSED','FAILED',tst)
   subtally=[subtally,tst]
end subroutine test_test

subroutine parse()
! all the extended types for settings from the main program
use fpm_command_line, only: &
        fpm_cmd_settings, &
        fpm_new_settings, &
        fpm_build_settings, &
        fpm_run_settings, &
        fpm_test_settings, &
        fpm_install_settings, &
        get_command_line_settings
use fpm, only: cmd_build, cmd_install, cmd_run, cmd_test
use fpm_new_subcommand, only: cmd_new 
class(fpm_cmd_settings), allocatable :: cmd_settings
! duplicates the calls as seen in the main program for fpm
call get_command_line_settings(cmd_settings)

allocate (character(len=len(name)) :: act_name(0) )
act_args=''
act_w_e=.false.
act_w_t=.false.
act_release=.false.

select type(settings=>cmd_settings)
type is (fpm_new_settings)
    act_w_e=settings%with_executable
    act_w_t=settings%with_test
    act_name=[trim(settings%name)]
type is (fpm_build_settings)
    act_release=settings%release
type is (fpm_run_settings)
    act_release=settings%release
    act_name=settings%name
    act_args=settings%args
type is (fpm_test_settings)
    act_release=settings%release
    act_name=settings%name
    act_args=settings%args
type is (fpm_install_settings)
end select

open(file='_test_cli',newunit=lun,delim='quote')
write(lun,nml=act_cli,delim='quote')
!!write(*,nml=act_cli)
close(unit=lun)

end subroutine parse

end program main
