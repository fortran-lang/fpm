! RUN IN VERBOSE MODE: fpm test '*test_suite*' -- brief=F
module M_module_1
use, intrinsic :: iso_fortran_env, only: &
& stdin => input_unit,   &
& stdout => output_unit, &
& stderr => error_unit
use M_framework, only : unit_test_start, unit_test, unit_test_msg
use M_framework, only : unit_test_end, unit_test_stop, unit_test_mode
use M_framework, only : unit_test_level, unit_test_flags
use fpm_strings
implicit none
character(len=*),parameter :: g='(*(g0,1x))'
character(len=*),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=*),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
logical, parameter :: T=.true., F=.false.
logical            :: matched
character(len=:),allocatable :: tmsg
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_fnv_1a()
   tmsg="Hash a CHARACTER(*) string of default kind or a TYPE(STRING_T) array"
   call unit_test_start("fnv_1a",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("fnv_1a                   ", 0 .eq. 0, "checking",100)
   call unit_test_end("fnv_1a",msg="")
end subroutine test_suite_fnv_1a                   
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_f_string()

   tmsg="convert C-like array of single characters terminated by C_NULL_CHAR to string"
   call unit_test_start("f_string",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("f_string                 ", 0 .eq. 0, "checking",100)
   call unit_test_end("f_string",msg="")
end subroutine test_suite_f_string                 
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_str()
   tmsg='function returns argument as a numeric value from a string'
   call unit_test_start('str',msg=tmsg,matched=matched)
   if(.not.matched)return
   !call unit_test('str', str('3.0d0') == 3.0d0,msg='test string to real for overloaded REAL()')
   !call unit_test('str', str('1234') == 1234,msg='test string to integer for overloaded INT()')
   !call unit_test('str', str('3.0d0') == 3.0d0,msg='test string to double for overloaded DBLE()')
   call unit_test_end("str",msg="")
end subroutine test_suite_str
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_glob()
! This main() routine passes a bunch of test strings into the above code.
! In performance comparison mode, it does that over and over.  Otherwise,
! it does it just once.  Either way, it outputs a passed/failed result.
!
integer :: nReps
logical :: allpassed
integer :: i
   tmsg="function compares text strings, one of which can have wildcards ('*' or '?')."
   call unit_test_start("glob",msg=tmsg,matched=matched)
   if(.not.matched)return
   allpassed = .true.
   nReps = 1000000
   nReps = 10    ! Can choose as many repetitions as you're expecting in the real world.

   do i=1,nReps
     ! Cases with repeating character sequences.
     allpassed=  test("a*abab",       "a*b",    .true.)   .and.  allpassed
     allpassed=  test("ab",           "*?",     .true.)   .and.  allpassed
     allpassed=  test("abc",          "*?",     .true.)   .and.  allpassed
     allpassed=  test("abcccd",       "*ccd",   .true.)   .and.  allpassed
     allpassed=  test("bLah",         "bLaH",   .false.)  .and.  allpassed
     allpassed=  test("mississippi",  "*sip*",  .true.)   .and.  allpassed
     allpassed= &
      & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.) .and. allpassed
     allpassed= &
      & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.) .and. allpassed
     allpassed= &
      & test("mississipissippi", "*issip*ss*", .true.) .and. allpassed
     allpassed= &
      & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.) .and. allpassed
     allpassed= &
      & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.) .and. allpassed
     allpassed=  test("xyxyxyzyxyz",  "xy*z*xyz",  .true.)   .and.  allpassed
     allpassed=  test("xyxyxyxyz",    "xy*xyz",    .true.)   .and.  allpassed
     allpassed=  test("mississippi",  "mi*sip*",   .true.)   .and.  allpassed
     allpassed=  test("ababac",       "*abac*",    .true.)   .and.  allpassed
     allpassed=  test("aaazz",        "a*zz*",     .true.)   .and.  allpassed
     allpassed=  test("a12b12",       "*12*23",    .false.)  .and.  allpassed
     allpassed=  test("a12b12",       "a12b",      .false.)  .and.  allpassed
     allpassed=  test("a12b12",       "*12*12*",   .true.)   .and.  allpassed
     ! Additional cases where the '*' char appears in the tame string.
     allpassed=  test("*",     "*",      .true.)   .and.  allpassed
     allpassed=  test("a*r",   "a*",     .true.)   .and.  allpassed
     allpassed=  test("a*ar",  "a*aar",  .false.)  .and.  allpassed
     ! More double wildcard scenarios.
     allpassed=  test("XYXYXYZYXYz",  "XY*Z*XYz",   .true.)   .and.  allpassed
     allpassed=  test("missisSIPpi",  "*SIP*",      .true.)   .and.  allpassed
     allpassed=  test("mississipPI",  "*issip*PI",  .true.)   .and.  allpassed
     allpassed=  test("xyxyxyxyz",    "xy*xyz",     .true.)   .and.  allpassed
     allpassed=  test("miSsissippi",  "mi*sip*",    .true.)   .and.  allpassed
     allpassed=  test("miSsissippi",  "mi*Sip*",    .false.)  .and.  allpassed
     allpassed=  test("abAbac",       "*Abac*",     .true.)   .and.  allpassed
     allpassed=  test("aAazz",        "a*zz*",      .true.)   .and.  allpassed
     allpassed=  test("A12b12",       "*12*23",     .false.)  .and.  allpassed
     allpassed=  test("a12B12",       "*12*12*",    .true.)   .and.  allpassed
     allpassed=  test("oWn",          "*oWn*",      .true.)   .and.  allpassed
     ! Completely tame (no wildcards) cases.
     allpassed= test("bLah", "bLah", .true.) .and. allpassed
     ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
     allpassed= test("a", "*?", .true.) .and. allpassed
     ! More mixed wildcard tests including coverage for false positives.
     allpassed=  test("a",      "??",         .false.)  .and.  allpassed
     allpassed=  test("ab",     "?*?",        .true.)   .and.  allpassed
     allpassed=  test("ab",     "*?*?*",      .true.)   .and.  allpassed
     allpassed=  test("abc",    "?**?*?",     .true.)   .and.  allpassed
     allpassed=  test("abc",    "?**?*&?",    .false.)  .and.  allpassed
     allpassed=  test("abcd",   "?b*??",      .true.)   .and.  allpassed
     allpassed=  test("abcd",   "?a*??",      .false.)  .and.  allpassed
     allpassed=  test("abcd",   "?**?c?",     .true.)   .and.  allpassed
     allpassed=  test("abcd",   "?**?d?",     .false.)  .and.  allpassed
     allpassed=  test("abcde",  "?*b*?*d*?",  .true.)   .and.  allpassed
     ! Single-character-match cases.
     allpassed=  test("bLah",   "bL?h",  .true.)   .and.  allpassed
     allpassed=  test("bLaaa",  "bLa?",  .false.)  .and.  allpassed
     allpassed=  test("bLah",   "bLa?",  .true.)   .and.  allpassed
     allpassed=  test("bLaH",   "?Lah",  .false.)  .and.  allpassed
     allpassed=  test("bLaH",   "?LaH",  .true.)   .and.  allpassed
     allpassed=  test('abcdefghijk'  ,  '?b*',      .true.)  .and.  allpassed
     allpassed=  test('abcdefghijk'  ,  '*c*',      .true.)  .and.  allpassed
     allpassed=  test('abcdefghijk'  ,  '*c',       .false.) .and.  allpassed
     allpassed=  test('abcdefghijk'  ,  '*c*k',     .true.)  .and.  allpassed
     allpassed=  test('LS'           ,  '?OW',      .false.) .and.  allpassed
     allpassed=  test('teztit'       ,  'tez*t*t',  .true.)  .and.  allpassed
       ! Two pattern match problems that might pose difficulties
     allpassed=  test('e '           , '*e* ',         .true.)  .and.  allpassed
     allpassed=  test('abcde       ' , '*e      *',    .true.)  .and.  allpassed
     allpassed=  test('bababa'       , 'b*ba',         .true.)  .and.  allpassed
     allpassed=  test('baaaaax'      , 'b*ax',         .true.)  .and.  allpassed
     allpassed=  test('baaaaa'       , 'b*ax',         .false.) .and.  allpassed
     allpassed=  test('baaaaax'      , 'b*a',          .false.) .and.  allpassed
     allpassed=  test(''             , 'b*',           .false.) .and.  allpassed
     allpassed=  test(''             , '*',            .true.)  .and.  allpassed
     allpassed=  test('b'            , '',             .false.) .and.  allpassed
     allpassed=  test('3'            , '??',           .false.) .and.  allpassed
     ! known flaws
     allpassed=  test('baaaaa'//char(0), 'b*a'//char(0), .true.) .and.  allpassed
     allpassed=  test(''//char(0),       ''//char(0),    .true.) .and.  allpassed
     ! Many-wildcard scenarios.
     allpassed= test(&
     &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
     &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
     &"a*a*a*a*a*a*aa*aaa*a*a*b",&
     &.true.) .and. allpassed
     allpassed= test(&
     &"abababababababababababababababababababaacacacacacacac&
     &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
     &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
     &.true.) .and. allpassed
     allpassed= test(&
     &"abababababababababababababababababababaacacacacacaca&
     &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
     &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
     &.false.) .and. allpassed
     allpassed= test(&
     &"abababababababababababababababababababaacacacacacacacad&
     &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
     &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
     &.false.) .and. allpassed
     allpassed= test(&
     &"abababababababababababababababababababaacacacacacacacad&
     &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
     &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
     &.true.) .and. allpassed
     allpassed= test("aaabbaabbaab", "*aabbaa*a*", .true.) .and. allpassed
     allpassed= &
     test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
     &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.) .and. allpassed
     allpassed= test("aaaaaaaaaaaaaaaaa",&
     &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.) .and. allpassed
     allpassed= test("aaaaaaaaaaaaaaaa",&
     &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.) .and. allpassed
     allpassed= test(&
     &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
     &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
     & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
     &*abc*abc*abc*",&
     &.false.) .and. allpassed
     allpassed= test(&
     &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
     &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
     &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
     &.true.) .and. allpassed
     allpassed= test("abc*abcd*abcd*abc*abcd",&
     &"abc*abc*abc*abc*abc", .false.) .and. allpassed
     allpassed= test( "abc*abcd*abcd*abc*abcd*abcd&
     &*abc*abcd*abc*abc*abcd", &
     &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
     &.true.) .and. allpassed
     allpassed= test("abc",&
     &"********a********b********c********", .true.) .and. allpassed
     allpassed=&
     &test("********a********b********c********", "abc",.false.).and.allpassed
     allpassed= &
     &test("abc", "********a********b********b********",.false.).and.allpassed
     allpassed= test("*abc*", "***a*b*c***", .true.) .and. allpassed
     ! A case-insensitive algorithm test.
     ! allpassed=test("mississippi", "*issip*PI", .true.) .and. allpassed
   enddo

   call unit_test_end("glob",msg="")
   contains
!===================================================================================================================================
   ! This is a test program for wildcard matching routines.  It can be used
   ! either to test a single routine for correctness, or to compare the timings
   ! of two (or more) different wildcard matching routines.
   !
   function test(tame, wild, bExpectedResult) result(bpassed)
      character(len=*) :: tame
      character(len=*) :: wild
      logical          :: bExpectedResult
      logical          :: bResult
      logical          :: bPassed
      bResult = .true.    ! We'll do "&=" cumulative checking.
      bPassed = .false.   ! Assume the worst.
      bResult = glob(tame, wild) ! Call a wildcard matching routine.

      ! To assist correctness checking, output the two strings in any failing scenarios.
      if (bExpectedResult .eqv. bResult) then
         bPassed = .true.
         !if(nReps == 1) write(stderr,g)"Passed match on ",tame," vs. ", wild
      else
         !if(nReps == 1) write(stderr,g)"Failed match on ",tame," vs. ", wild
      endif
      if(i==1)call unit_test('glob',bExpectedResult.eqv.bResult,'string',tame,'pattern',wild,'expected',bExpectedResult)
   end function test

end subroutine test_suite_glob                     
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_has_valid_custom_prefix()
   tmsg=''
   call unit_test_start("has_valid_custom_prefix",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("has_valid_custom_prefix  ", 0 .eq. 0, "checking",100)
   call unit_test_end("has_valid_custom_prefix",msg="")
end subroutine test_suite_has_valid_custom_prefix  
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_has_valid_standard_prefix()
   tmsg=''
   call unit_test_start("has_valid_standard_prefix",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("has_valid_standard_prefix", 0 .eq. 0, "checking",100)
   call unit_test_end("has_valid_standard_prefix",msg="")
end subroutine test_suite_has_valid_standard_prefix
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_is_fortran_name()
integer :: i
logical,parameter :: expect(*)= [F, T, T, F, T, T, F, F, F, F, F, F, T, F]
character(len=80),parameter :: names(*)=[character(len=80) ::  &
'_name',               &
'long_variable_name',  &
'name_',               &
'12L',                 &
'a__b__c',             &
'PropertyOfGas',       &
'3%3',                 &
'one two ',            &
'$NAME',               &
' ',                   &
'Variable-name',       &
'1234567890123456789012345678901234567890123456789012345678901234567890',       &
'A',                   &
'x@x'                  ]
   tmsg="determine whether a string is an acceptable Fortran entity name"
   call unit_test_start('is_fortran_name',msg=tmsg,matched=matched)
   if(.not.matched)return
!   call unit_test('is_fortran_name',all(is_fortran_name(names).eqv.expect),'elemental test')
   do i=1,size(names)
      call unit_test('is_fortran_name',is_fortran_name(names(i)).eqv.expect(i),names(i),'expected',expect(i))
   enddo
   call unit_test_end('is_fortran_name')
end subroutine test_suite_is_fortran_name          
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_is_valid_module_name()
   tmsg=''
   call unit_test_start("is_valid_module_name",msg=tmsg,matched=matched)
   !!call unit_test("is_valid_module_name     ", 0 .eq. 0, "checking",100)
   call unit_test_end("is_valid_module_name",msg="")
end subroutine test_suite_is_valid_module_name     
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_is_valid_module_prefix()
   tmsg=''
   call unit_test_start("is_valid_module_prefix",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("is_valid_module_prefix   ", 0 .eq. 0, "checking",100)
   call unit_test_end("is_valid_module_prefix",msg="")
end subroutine test_suite_is_valid_module_prefix   
   !!call unit_test("join                     ", 0 .eq. 0, "checking",100)
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_join()
character(len=:),allocatable  :: s(:)
   tmsg="append an array of CHARACTER variables into a single CHARACTER variable"
   call unit_test_start("join",msg=tmsg,matched=matched)
   if(.not.matched)return
   s=[character(len=10) :: 'United',' we',' stand,',' divided',' we fall.']

   call testit( join(s),                            'United we stand, divided we fall.')
   call testit( join(s,trm=.false.),                'United     we        stand,    divided   we fall.')
   call testit( join(s,trm=.false.,sep='|'),        'United    | we       | stand,   | divided  | we fall.')
   call testit( join(s,sep='<>'),                   'United<> we<> stand,<> divided<> we fall.')
   call testit( join(s,sep=';',left='[',right=']'), '[United];[ we];[ stand,];[ divided];[ we fall.]')
   call testit( join(s,left='[',right=']'),         '[United][ we][ stand,][ divided][ we fall.]')
   call testit( join(s,left='>>'),                  '>>United>> we>> stand,>> divided>> we fall.')
   s=[character(len=0) :: ]
   call testit( join(s),                            '')
   call testit( join(s,trm=.false.),                '')
   call testit( join(s,trm=.false.,sep='|'),        '')
   call testit( join(s,sep='<>'),                   '')
   call testit( join(s,sep=';',left='[',right=']'), '[]')
   call testit( join(s,left='[',right=']'),         '[]')
   call testit( join(s,left='>>'),                  '>>')
   call unit_test_end('join',msg='join array of strings into a single string controlling separators and white space')
contains
subroutine testit(generated,expected)
character(len=*),intent(in) :: generated
character(len=*),intent(in) :: expected
   if(unit_test_level > 0)then
      write(stderr,g)'JOIN(3F) TEST'
      write(stderr,g)'INPUT       ','['//s//']'
      write(stderr,g)'GENERATED   ',generated
      write(stderr,g)'EXPECTED    ',expected
   endif
   call unit_test('join',generated == expected,msg='output is '//generated)
end subroutine testit
end subroutine test_suite_join                     
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_len_trim()
   tmsg="Determine total trimmed length of STRING_T array"
   call unit_test_start("len_trim",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("len_trim                 ", 0 .eq. 0, "checking",100)
   call unit_test_end("len_trim",msg="")
end subroutine test_suite_len_trim                 
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_lower()
character(len=36),parameter :: rnge='ABCDEFGHijklmnOPQRSTUVWXYZ0123456789'

   call unit_test_start('lower',msg='[CASE] changes a string to lowercase over specified range' ,matched=matched)
   if(.not.matched)return
   call unit_test('lower',lower(uc) == lc,'lower',lower(uc),'expected',lc)
   call unit_test('lower',lower(uc,9,14) == rnge,'range',lower(uc,9,14),'expected',rnge)
   call unit_test_end("lower",msg="")
end subroutine test_suite_lower                    
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_module_prefix_template()
   tmsg=''
   call unit_test_start("module_prefix_template",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("module_prefix_template   ", 0 .eq. 0, "checking",100)
   call unit_test_end("module_prefix_template",msg="")
end subroutine test_suite_module_prefix_template   
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_module_prefix_type()
   tmsg=''
   call unit_test_start("module_prefix_type",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("module_prefix_type       ", 0 .eq. 0, "checking",100)
   call unit_test_end("module_prefix_type",msg="")
end subroutine test_suite_module_prefix_type       
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_notabs()
character(len=:),allocatable :: inline
character(len=:),allocatable :: expected
character(len=1024)          :: outline
integer                      :: iout
   tmsg="Expand tab characters assuming a tab space every eight characters"
   call unit_test_start("notabs",msg=tmsg,matched=matched)
   if(.not.matched)return
   inline= 'one '//char(9)//'and'//repeat(char(9),3)//'two'
   expected='one     and                     two'
   call notabs(inline,outline,iout)

   if(unit_test_level /= 0)then
      write(stderr,g)'*test_notabs*',inline
      write(stderr,g)'*test_notabs*',outline
      write(stderr,g)'*test_notabs*',len_trim(outline),iout
   endif

   call unit_test('notabs',outline == expected.and.iout == 35,msg='expand a line with tabs in it')

   call unit_test_end('notabs',msg='')

end subroutine test_suite_notabs                   
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_operator_in()
   tmsg="Check if array of TYPE(STRING_T) matches a particular CHARACTER string"
   call unit_test_start("operator_in",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("operator_in              ", 0 .eq. 0, "checking",100)
   call unit_test_end("operator_in",msg="")
end subroutine test_suite_operator_in              
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_remove_newline_characters()
   tmsg=''
   call unit_test_start("remove_newline_characters",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("remove_newline_characters", 0 .eq. 0, "checking",100)
   call unit_test_end("remove_newline_characters",msg="")
end subroutine test_suite_remove_newline_characters
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_replace()
   tmsg="Returns string with characters in charset replaced with target_char."
   call unit_test_start("replace",msg=tmsg,matched=matched)
   if(.not.matched)return
   call unit_test("replace", replace("abcdefg",["a","e"],"^").eq.'^bcd^fg','given abcdefg expect ^bcd^fg')
   call unit_test_end("replace",msg="")
end subroutine test_suite_replace                  
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_resize()
   tmsg="increase the size of a TYPE(STRING_T) array by N elements"
   call unit_test_start("resize",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("resize                   ", 0 .eq. 0, "checking",100)
   call unit_test_end("resize",msg="")
end subroutine test_suite_resize                   
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_split()
   tmsg="parse string on delimiter characters and store tokens into an allocatable array"
   call unit_test_start("split",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("split                    ", 0 .eq. 0, "checking",100)
   call unit_test_end("split",msg="")
end subroutine test_suite_split                    
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_str_begins_with_str()
   tmsg=''
   call unit_test_start("str_begins_with_str",msg=tmsg,matched=matched)
   if(.not.matched)return

   call unit_test("str_begins_with_str",str_begins_with_str("ABCDEF","") ,         'null')
   call unit_test("str_begins_with_str",str_begins_with_str("ABCDEF","A") ,        'single letter')
   call unit_test("str_begins_with_str",str_begins_with_str("ABCDEF","AB") ,       'multi-letter')
   call unit_test("str_begins_with_str",.not.str_begins_with_str(" ABCDEF","AB") , 'string has leading space')
   call unit_test("str_begins_with_str",.not.str_begins_with_str("ABCDEF","AB ") , 'prefix has training space')
   call unit_test("str_begins_with_str",str_begins_with_str(" ABCDEF"," A") ,      'prefix has leading space')

   call unit_test_end("str_begins_with_str",msg="")

end subroutine test_suite_str_begins_with_str      
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_str_ends_with()
        tmsg="test if a CHARACTER string ends with a specified prefix(s)"
   call unit_test_start('str_ends_with',msg=tmsg ,matched=matched)
   if(.not.matched)return

   call unit_test('str_ends_with',.not.str_ends_with('prog.a',['.o','.i','.s']),'test prog.a with [.o,.i,.s]')
   call unit_test('str_ends_with',str_ends_with('prog.f90',['.F90','.f90','.f  ','.F  ']),'test prog.f90 with .F90, .f90, .f, .F')
   call unit_test('str_ends_with',str_ends_with('prog.pdf','.pdf'),'test prog.pdf with .pdf')
   call unit_test('str_ends_with',.not.str_ends_with('prog.doc','.txt'),'test prog.doc with .txt')

   call unit_test_end("str_ends_with",msg="")

end subroutine test_suite_str_ends_with            
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_string_array_contains()
   tmsg="Check if array of TYPE(STRING_T) matches a particular CHARACTER string"
   call unit_test_start("string_array_contains",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("string_array_contains    ", 0 .eq. 0, "checking",100)
   call unit_test_end("string_array_contains",msg="")
end subroutine test_suite_string_array_contains    
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_string_cat()
type(string_t), allocatable :: list1(:), list2(:)

   tmsg="Concatenate an array of type(string_t) into a single CHARACTER variable"
   call unit_test_start("string_cat",msg=tmsg,matched=matched)
   if(.not.matched)return

   list1 = [string_t(''), string_t("a"), string_t(" b "), string_t("  ccc  "), string_t("DDDD"), string_t('')]
   list2 = [string_t("A"), string_t(" B "), string_t(" CCC  "), string_t("DDDD")]

   call unit_test('string_cat', string_cat(list1) ==           'a b   ccc  DDDD',                          'lacking')
   call unit_test('string_cat', string_cat(list1, '.f90 ') ==  '.f90 a.f90  b .f90   ccc  .f90 DDDD.f90 ', 'suffix')
   call unit_test('string_cat', string_cat(list1, '|') ==      '|a| b |  ccc  |DDDD|',                     'vertical')
   call unit_test('string_cat', string_cat(list1, '') ==       'a b   ccc  DDDD',                          'null')

   call unit_test('string_cat', string_cat(list2) ==           'A B  CCC  DDDD',                           'lacking')
   call unit_test('string_cat', string_cat(list2, '.f90 ') ==  'A.f90  B .f90  CCC  .f90 DDDD',            'suffix')
   call unit_test('string_cat', string_cat(list2, '|') ==      'A| B | CCC  |DDDD',                        'vertical')
   call unit_test('string_cat', string_cat(list2, '') ==       'A B  CCC  DDDD',                           'null')

   call unit_test_end("string_cat",msg="")
end subroutine test_suite_string_cat               
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_string_t()
   tmsg=''
   call unit_test_start("string_t",msg=tmsg,matched=matched)
   if(.not.matched)return
   !!call unit_test("string_t                 ", 0 .eq. 0, "checking",100)
   call unit_test_end("string_t",msg="")
end subroutine test_suite_string_t                 
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_to_fortran_name()
   tmsg="replace allowed special but unusuable characters in names with underscore"
   call unit_test_start("to_fortran_name",msg=tmsg,matched=matched)
   if(.not.matched)return
   call unit_test("to_fortran_name", to_fortran_name("cos")     == 'cos',     to_fortran_name("cos"))
   call unit_test("to_fortran_name", to_fortran_name("co-sine") == 'co_sine', to_fortran_name("co-sine"))
   call unit_test("to_fortran_name", to_fortran_name("-c-o-s-") == '_c_o_s_', to_fortran_name("-c-o-s-"))
   call unit_test("to_fortran_name", to_fortran_name("c -o- s") == 'c _o_ s', to_fortran_name("c -o- s"))
   call unit_test_end("to_fortran_name",msg="")
end subroutine test_suite_to_fortran_name          
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_dilate()
character(len=:),allocatable :: in
character(len=:),allocatable :: expected
integer                      :: i
   tmsg=''
   call unit_test_start('dilate','[NONALPHA] expand tab characters')
!        in='  this is my string  '
!        ! change spaces to tabs to make a sample input
!        do i=1,len(in)
!           if(in(i:i) == ' ')in(i:i)=char(9)
!        enddo
!        expected="                this    is      my      string"
!        call unit_test('dilate',dilate(in).eq.expected,'expected',expected,'got',dilate(in))
   call unit_test_end('dilate')
end subroutine test_suite_dilate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end module M_module_1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_framework, only : unit_test_stop, unit_test_mode
use M_framework, only : unit_test_level, unit_test_flags
use M_module_1
implicit none
! optional call to change default modes
   call unit_test_mode(        &
       keep_going=T,           &
       flags=[0],              &
       luns=[stdout],          &
       command='',             &
       brief=T,                &
       match='',               &
       interactive=F,          &
       CMDLINE=T,              &
       debug=F)

   unit_test_level=0

   call test_suite_remove_newline_characters()
   call test_suite_fnv_1a()
   call test_suite_f_string()
   call test_suite_glob()
   call test_suite_has_valid_custom_prefix()
   call test_suite_has_valid_standard_prefix()
   call test_suite_is_fortran_name()
   call test_suite_is_valid_module_name()
   call test_suite_is_valid_module_prefix()
   call test_suite_join()
   call test_suite_len_trim()
   call test_suite_lower()
   call test_suite_module_prefix_template()
   call test_suite_module_prefix_type()
   call test_suite_notabs()
   call test_suite_operator_in()
   call test_suite_replace()
   call test_suite_resize()
   call test_suite_split()
   call test_suite_str()
   call test_suite_str_begins_with_str()
   call test_suite_str_ends_with()
   call test_suite_string_array_contains()
   call test_suite_string_cat()
   call test_suite_string_t()
   call test_suite_to_fortran_name()
   call unit_test_stop()
end program runtest
