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
character(len=15),allocatable        :: features(:),act_features(:) ; namelist/act_cli/act_features
integer,parameter                    :: max_names=10

character(len=:),allocatable         :: command
character(len=:),allocatable         :: cmd
integer                              :: cstat, estat
integer                              :: act_cstat, act_estat
integer                              :: i, ios
logical                              :: w_e,act_w_e          ; namelist/act_cli/act_w_e
logical                              :: w_t,act_w_t          ; namelist/act_cli/act_w_t
logical                              :: c_s,act_c_s          ; namelist/act_cli/act_c_s
logical                              :: c_a,act_c_a          ; namelist/act_cli/act_c_a
logical                              :: c_t,act_c_t          ; namelist/act_cli/act_c_t
logical                              :: c_apps,act_c_apps    ; namelist/act_cli/act_c_apps
logical                              :: c_ex,act_c_ex        ; namelist/act_cli/act_c_ex
logical                              :: reg_c,act_reg_c      ; namelist/act_cli/act_reg_c
logical                              :: show_v,act_show_v    ; namelist/act_cli/act_show_v
logical                              :: show_u_d,act_show_u_d; namelist/act_cli/act_show_u_d
logical                              :: dry_run,act_dry_run  ; namelist/act_cli/act_dry_run
character(len=:), allocatable        :: token, act_token     ; namelist/act_cli/act_token

character(len=:), allocatable        :: profile,act_profile  ; namelist/act_cli/act_profile
character(len=:), allocatable        :: args,act_args        ; namelist/act_cli/act_args
character(len=:), allocatable        :: query,act_query      ; namelist/act_cli/act_query
character(len=:), allocatable        :: page,act_page        ; namelist/act_cli/act_page
character(len=:), allocatable        :: registry,act_registry; namelist/act_cli/act_registry
character(len=:), allocatable        :: namespace,act_namespace ; namelist/act_cli/act_namespace
character(len=:), allocatable        :: package_version,act_package_version ; namelist/act_cli/act_package_version
character(len=:), allocatable        :: license,act_license  ; namelist/act_cli/act_license
character(len=:), allocatable        :: limit,act_limit      ; namelist/act_cli/act_limit
character(len=:), allocatable        :: sort_by,act_sort_by  ; namelist/act_cli/act_sort_by
character(len=:), allocatable        :: sort,act_sort        ; namelist/act_cli/act_sort
namelist/expected/cmd,cstat,estat,w_e,w_t,c_s,c_a,c_t,c_apps,c_ex,reg_c,name,features,profile,args,query,page,registry,namespace,&
   package_version,license,limit,sort_by,sort,show_v,show_u_d,dry_run,token
integer                              :: lun
logical,allocatable                  :: tally(:)
logical,allocatable                  :: subtally(:)
character(len=256)                   :: message

! table of arguments to pass to program and expected non-default values for that execution in NAMELIST group format
character(len=*),parameter           :: tests(*)= [ character(len=512) :: &

'CMD="new",                                           ESTAT=1,', &
!'CMD="new -unknown",                                  ESTAT=2,', &
'CMD="new my_project another yet_another -test", ESTAT=2,', &
'CMD="new my_project --app",                          W_E=T,       NAME="my_project",',  &
'CMD="new my_project --app --test",                   W_E=T,W_T=T, NAME="my_project",',  &
'CMD="new my_project --test",                               W_T=T, NAME="my_project",',  &
'CMD="new my_project",                                W_E=T,W_T=T, NAME="my_project",',  &

'CMD="run",                                                                                               ', &
'CMD="run my_project",                                             NAME="my_project",                     ', &
'CMD="run proj1 p2 project3",                                      NAME="proj1","p2","project3",          ', &
'CMD="run proj1 p2 project3 --profile debug",                      NAME="proj1","p2","project3",profile="debug",', &
'CMD="run proj1 p2 project3 --profile release",                    NAME="proj1","p2","project3",profile="release",', &
'CMD="run proj1 p2 project3 --profile release -- arg1 -x ""and a long one""", &
   &NAME="proj1","p2","project3",profile="release",ARGS="""arg1"" ""-x"" ""and a long one""",                         ', &

'CMD="test",                                                                                              ', &
'CMD="test my_project",                                            NAME="my_project",                     ', &
'CMD="test proj1 p2 project3",                                     NAME="proj1","p2","project3",          ', &
'CMD="test proj1 p2 project3 --profile debug",                     NAME="proj1","p2","project3",profile="debug",', &
'CMD="test proj1 p2 project3 --profile release",                   NAME="proj1","p2","project3",profile="release",', &
'CMD="test proj1 p2 project3 --profile release -- arg1 -x ""and a long one""", &
   &NAME="proj1","p2","project3",profile="release",ARGS="""arg1"" ""-x"" ""and a long one""",                         ', &

'CMD="search", QUERY="", PAGE="1", REGISTRY="https://github.com/fortran-lang/' // &
'fpm-registry", NAMESPACE="*", PACKAGE_VERSION="*", LICENSE="", ' // &
'LIMIT="10", SORT_BY="name", SORT="asc",', &
'CMD="search --query mpi --registry https://example.org --namespace fortran-lang ' // &
'--package_version 0.1.0 --license MIT --page 2 --limit 25 ' // &
'--sort-by downloads --sort desc", &
   &QUERY="mpi", PAGE="2", REGISTRY="https://example.org", NAMESPACE="fortran-lang", ' // &
   &'PACKAGE_VERSION="0.1.0", LICENSE="MIT", LIMIT="25", ' // &
   &'SORT_BY="downloads", SORT="desc",', &

'CMD="build",                                                      NAME=, profile="",features=,ARGS="",', &
'CMD="build --profile release",                                    NAME=, profile="release",features=,ARGS="",', &
'CMD="build --features debug,mpi",                                 NAME=, profile="",features="debug","mpi",ARGS="",', &
'CMD="build --features single_feature",                            NAME=, profile="",features="single_feature",ARGS="",', &
'CMD="test --features debug,openmp",                               NAME=, profile="",features="debug","openmp",ARGS="",', &

'CMD="clean",                                                      NAME=, ARGS="",', &
'CMD="clean --skip",                                        C_S=T, NAME=, ARGS="",', &
'CMD="clean --all",                                         C_A=T, NAME=, ARGS="",', &
'CMD="clean --test",                                        C_T=T, NAME=, ARGS="",', &
'CMD="clean --apps",                                     C_APPS=T, NAME=, ARGS="",', &
'CMD="clean --examples",                                    C_EX=T, NAME=, ARGS="",', &
'CMD="clean --test --apps",                           C_T=T, C_APPS=T, NAME=, ARGS="",', &
'CMD="clean --registry-cache",                            REG_C=T, NAME=, ARGS="",', &
'CMD="publish --token abc --show-package-version",       SHOW_V=T, NAME=, token="abc",ARGS="",', &
'CMD="publish --token abc --show-upload-data",           SHOW_U_D=T, NAME=, token="abc",ARGS="",', &
'CMD="publish --token abc --dry-run",                    DRY_RUN=T, NAME=, token="abc",ARGS="",', &
'CMD="publish --token abc",                                        NAME=, token="abc",ARGS="",', &
' ' ]
character(len=512) :: readme(3)

readme(1)='&EXPECTED'  ! top and bottom line for a NAMELIST group read from TEST() used to set the expected values
readme(3)=' /'
tally=[logical ::]     ! an array that tabulates the command test results as pass or fail.

if(command_argument_count()==0)then  ! assume if called with no arguments to do the tests. This means you cannot
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
      if(tests(i)==' ')then
         open(file='_test_cli',newunit=lun,delim='quote')
         close(unit=lun,status='delete')
         exit
      endif
      ! blank out name group EXPECTED
      name=[(repeat(' ',len(name)),i=1,max_names)] ! the words on the command line sans the subcommand name
      if(.not.allocated(features)) allocate(character(len=15) :: features(max_names))
      features=[(repeat(' ',15),i=1,max_names)] ! the features on the command line
      profile=''                     ! --profile PROF
      w_e=.false.                    ! --app
      w_t=.false.                    ! --test
      c_s=.false.                    ! --skip
      c_a=.false.                    ! --all
      reg_c=.false.                  ! --registry-cache
      show_v=.false.                 ! --show-package-version
      show_u_d=.false.               ! --show-upload-data
      dry_run=.false.                ! --dry-run
      token=''                       ! --token TOKEN
      args=repeat(' ',512)           ! -- ARGS
      query=''                       ! --query
      page=''                        ! --page
      registry=''                    ! --registry
      namespace=''                   ! --namespace
      package_version=''             ! --package_version
      license=''                     ! --license
      limit=''                       ! --limit
      sort_by=''                     ! --sort-by
      sort=''                        ! --sort
      cmd=repeat(' ',512)            ! the command line arguments to test
      cstat=0                        ! status values from EXECUTE_COMMAND_LINE()
      estat=0
      readme(2)=' '//tests(i)        ! select command options to test for CMD and set nondefault expected values
      read(readme,nml=expected)

      write(*,'(*(g0))')'START:  TEST ',i,' CMD=',trim(cmd)
      ! call this program which will crack command line and write results to scratch file _test_cli
      call execute_command_line(command//' '//trim(cmd),cmdstat=act_cstat,exitstat=act_estat)
      if(cstat==act_cstat.and.estat==act_estat)then
          if(estat==0)then
             open(file='_test_cli',newunit=lun,delim='quote')
             act_name=[(repeat(' ',len(act_name)),i=1,max_names)]
             if(.not.allocated(act_features)) allocate(character(len=15) :: act_features(max_names))
             act_features=[(repeat(' ',15),i=1,max_names)]
             act_profile=''
             act_w_e=.false.
             act_w_t=.false.
             act_c_s=.false.
             act_c_a=.false.
             act_reg_c=.false.
             act_show_v=.false.
             act_show_u_d=.false.
             act_dry_run=.false.
             act_token=''
             act_args=repeat(' ',512)
             act_query=''
             act_page=''
             act_registry=''
             act_namespace=''
             act_package_version=''
             act_license=''
             act_limit=''
             act_sort_by=''
             act_sort=''
             read(lun,nml=act_cli,iostat=ios,iomsg=message)
             if(ios/=0)then
                write(*,'(a)')'ERROR:',trim(message)
             endif
             close(unit=lun)
             ! compare results to expected values
             subtally=[logical ::]
             call test_test('NAME',all(act_name==name))
             call test_test('FEATURES',all(act_features==features))
             call test_test('PROFILE',act_profile==profile)
             call test_test('SKIP',act_c_s.eqv.c_s)
             call test_test('ALL',act_c_a.eqv.c_a)
             call test_test('REGISTRY-CACHE',act_reg_c.eqv.reg_c)
             call test_test('WITH_EXPECTED',act_w_e.eqv.w_e)
             call test_test('WITH_TESTED',act_w_t.eqv.w_t)
             call test_test('WITH_TEST',act_w_t.eqv.w_t)
             call test_test('SHOW-PACKAGE-VERSION',act_show_v.eqv.show_v)
             call test_test('SHOW-UPLOAD-DATA',act_show_u_d.eqv.show_u_d)
             call test_test('DRY-RUN',act_dry_run.eqv.dry_run)
             call test_test('TOKEN',act_token==token)
             call test_test('ARGS',act_args==args)
             call test_test('QUERY',act_query==query)
             call test_test('PAGE',act_page==page)
             call test_test('REGISTRY',act_registry==registry)
             call test_test('NAMESPACE',act_namespace==namespace)
             call test_test('PACKAGE_VERSION',act_package_version==package_version)
             call test_test('LICENSE',act_license==license)
             call test_test('LIMIT',act_limit==limit)
             call test_test('SORT_BY',act_sort_by==sort_by)
             call test_test('SORT',act_sort==sort)
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

subroutine test_test(label,tst)
character(len=*) :: label
logical,intent(in) :: tst
   !!write(*,'(*(g0,1x))')'        SUBTEST ',label,' ',merge('PASSED','FAILED',tst)
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
        fpm_clean_settings, &
        fpm_install_settings, &
   fpm_search_settings, &
        get_command_line_settings, &
        fpm_publish_settings
use fpm, only: cmd_run, cmd_clean
use fpm_cmd_install, only: cmd_install
use fpm_cmd_new, only: cmd_new
use fpm_cmd_publish, only: cmd_publish
class(fpm_cmd_settings), allocatable :: cmd_settings
! duplicates the calls as seen in the main program for fpm
call get_command_line_settings(cmd_settings)

allocate (character(len=len(name)) :: act_name(0) )
allocate(character(len=15) :: act_features(max_names))
act_features=[(repeat(' ',15),i=1,max_names)]
act_args=''
act_w_e=.false.
act_w_t=.false.
act_c_s=.false.
act_c_a=.false.
act_reg_c=.false.
act_show_v=.false.
act_show_u_d=.false.
act_dry_run=.false.
act_token=''
act_profile=''
act_query=''
act_page=''
act_registry=''
act_namespace=''
act_package_version=''
act_license=''
act_limit=''
act_sort_by=''
act_sort=''

select type(settings=>cmd_settings)
type is (fpm_new_settings)
    act_w_e=settings%with_executable
    act_w_t=settings%with_test
    act_name=[trim(settings%name)]
type is (fpm_build_settings)
    if (allocated(settings%profile)) act_profile=settings%profile
    if (allocated(settings%features)) then
        do i = 1, min(size(settings%features),size(act_features))
            act_features(i) = settings%features(i)%s
        end do
    end if
type is (fpm_run_settings)
    if (allocated(settings%profile)) act_profile=settings%profile
    if (allocated(settings%features)) then
        do i = 1, min(size(settings%features),size(act_features))
            act_features(i) = settings%features(i)%s
        end do
    end if
    act_name=settings%name
    if (allocated(settings%args)) act_args=settings%args
type is (fpm_test_settings)
    if (allocated(settings%profile)) act_profile=settings%profile
    if (allocated(settings%features)) then
        do i = 1, min(size(settings%features),size(act_features))
            act_features(i) = settings%features(i)%s
        end do
    end if
    act_name=settings%name
    if (allocated(settings%args)) act_args=settings%args
type is (fpm_clean_settings)
    act_c_s=settings%clean_skip
    act_c_a=settings%clean_all
    act_c_t=settings%clean_test
    act_c_apps=settings%clean_apps
    act_c_ex=settings%clean_examples
    act_reg_c=settings%registry_cache
type is (fpm_install_settings)
type is (fpm_publish_settings)
    act_show_v=settings%show_package_version
    act_show_u_d=settings%show_upload_data
    act_dry_run=settings%is_dry_run
    act_token=settings%token
type is (fpm_search_settings)
   if (allocated(settings%query)) act_query=settings%query
   if (allocated(settings%page)) act_page=settings%page
   if (allocated(settings%registry)) act_registry=settings%registry
   if (allocated(settings%namespace)) act_namespace=settings%namespace
   if (allocated(settings%version)) act_package_version=settings%version
   if (allocated(settings%license)) act_license=settings%license
   if (allocated(settings%limit)) act_limit=settings%limit
   if (allocated(settings%sort_by)) act_sort_by=settings%sort_by
   if (allocated(settings%sort)) act_sort=settings%sort
end select

open(file='_test_cli',newunit=lun,delim='quote')
write(lun,nml=act_cli,delim='quote')
close(unit=lun)

end subroutine parse

end program main
