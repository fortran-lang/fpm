module fpm_strings
use iso_fortran_env, only: int64
implicit none

private
public :: f_string, lower, split, str_ends_with, string_t
public :: string_array_contains, string_cat, operator(.in.), fnv_1a
public :: resize
public :: join

type string_t
    character(len=:), allocatable :: s
end type

interface resize
  module procedure :: resize_string
end interface

interface operator(.in.)
    module procedure string_array_contains
end interface

interface fnv_1a
    procedure :: fnv_1a_char
    procedure :: fnv_1a_string_t
end interface fnv_1a

interface str_ends_with
    procedure :: str_ends_with_str
    procedure :: str_ends_with_any
end interface str_ends_with

contains

pure logical function str_ends_with_str(s, e) result(r)
    character(*), intent(in) :: s, e
    integer :: n1, n2
    n1 = len(s)-len(e)+1
    n2 = len(s)
    if (n1 < 1) then
        r = .false.
    else
        r = (s(n1:n2) == e)
    end if
end function str_ends_with_str

pure logical function str_ends_with_any(s, e) result(r)
    character(*), intent(in) :: s
    character(*), intent(in) :: e(:)

    integer :: i

    r = .true.
    do i=1,size(e)

        if (str_ends_with(s,trim(e(i)))) return

    end do
    r = .false.

end function str_ends_with_any

function f_string(c_string)
    use iso_c_binding
    character(len=1), intent(in) :: c_string(:)
    character(:), allocatable :: f_string

    integer :: i, n

    i = 0
    do while(c_string(i+1) /= C_NULL_CHAR)
      i = i + 1
    end do
    n = i

    allocate(character(n) :: f_string)
    do i=1,n
      f_string(i:i) = c_string(i)
    end do

end function f_string


!> Hash a character(*) string of default kind
pure function fnv_1a_char(input, seed) result(hash)
    character(*), intent(in) :: input
    integer(int64), intent(in), optional :: seed
    integer(int64) :: hash

    integer :: i
    integer(int64), parameter :: FNV_OFFSET_32 = 2166136261_int64
    integer(int64), parameter :: FNV_PRIME_32 = 16777619_int64

    if (present(seed)) then
        hash = seed
    else
        hash = FNV_OFFSET_32
    end if

    do i=1,len(input)
        hash = ieor(hash,iachar(input(i:i),int64)) * FNV_PRIME_32
    end do

end function fnv_1a_char


!> Hash a string_t array of default kind
pure function fnv_1a_string_t(input, seed) result(hash)
    type(string_t), intent(in) :: input(:)
    integer(int64), intent(in), optional :: seed
    integer(int64) :: hash

    integer :: i

    hash = fnv_1a(input(1)%s,seed)

    do i=2,size(input)
        hash = fnv_1a(input(i)%s,hash)
    end do

end function fnv_1a_string_t


elemental pure function lower(str,begin,end) result (string)
    ! Changes a string to lowercase over specified range
    ! Author: John S. Urban
    ! License: Public Domain

    character(*), intent(In)     :: str
    character(len(str))          :: string
    integer,intent(in),optional  :: begin, end
    integer                      :: i
    integer                      :: ibegin, iend
    string = str

    ibegin = 1
    if (present(begin))then
        ibegin = max(ibegin,begin)
    endif

    iend = len_trim(str)
    if (present(end))then
        iend= min(iend,end)
    endif

    do i = ibegin, iend                               ! step thru each letter in the string in specified range
        select case (str(i:i))
        case ('A':'Z')
            string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
        case default
        end select
    end do

end function lower


logical function string_array_contains(search_string,array)
    ! Check if array of string_t contains a particular string
    !
    character(*), intent(in) :: search_string
    type(string_t), intent(in) :: array(:)

    integer :: i

    string_array_contains = any([(array(i)%s==search_string, &
                                   i=1,size(array))])

end function string_array_contains

!> Concatenate an array of type(string_t) into
!>  a single character
function string_cat(strings,delim) result(cat)
    type(string_t), intent(in) :: strings(:)
    character(*), intent(in), optional :: delim
    character(:), allocatable :: cat

    integer :: i,n
    character(:), allocatable :: delim_str

    if (size(strings) < 1) then
        cat = ''
        return
    end if

    if (present(delim)) then
        delim_str = delim
    else
        delim_str = ''
    end if

    cat = strings(1)%s
    do i=2,size(strings)

        cat = cat//delim_str//strings(i)%s

    end do

end function string_cat

subroutine split(input_line,array,delimiters,order,nulls)
    ! parse string on delimiter characters and store tokens into an allocatable array"
    ! Author: John S. Urban
    ! License: Public Domain


    !  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
    !    o by default adjacent delimiters in the input string do not create an empty string in the output array
    !    o no quoting of delimiters is supported
    character(len=*),intent(in)              :: input_line  ! input string to tokenize
    character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
    character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
    character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
    character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens

    integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
    integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
    integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
    character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
    character(len=:),allocatable  :: ordr                   ! string containing order keyword
    character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
    integer                       :: ii,iiii                ! loop parameters used to control print order
    integer                       :: icount                 ! number of tokens found
    integer                       :: ilen                   ! length of input string with trailing spaces trimmed
    integer                       :: i10,i20,i30            ! loop counters
    integer                       :: icol                   ! pointer into input string as it is being parsed
    integer                       :: idlim                  ! number of delimiter characters
    integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
    integer                       :: inotnull               ! count strings not composed of delimiters
    integer                       :: ireturn                ! number of tokens returned
    integer                       :: imax                   ! length of longest token

    ! decide on value for optional DELIMITERS parameter
    if (present(delimiters)) then                                     ! optional delimiter list was present
        if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
            dlim=delimiters
        else                                                           ! DELIMITERS was specified on call as empty string
            dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
        endif
    else                                                              ! no delimiter value was specified
        dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
    endif
    idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string

    if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
    if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter

    n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
    allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
    allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
    ibegin(:)=1
    iterm(:)=1

    ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
    icount=0                                                       ! how many tokens found
    inotnull=0                                                     ! how many tokens found not composed of delimiters
    imax=0                                                         ! length of longest token found

    select case (ilen)

    case (0)                                                      ! command was totally blank

    case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
        icol=1                                                      ! initialize pointer into input line
        INFINITE: do i30=1,ilen,1                                   ! store into each array element
            ibegin(i30)=icol                                         ! assume start new token on the character
            if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
                ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
                IF(ifound.gt.0)then
                    iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
                endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
            else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
            endif
            imax=max(imax,iterm(i30)-ibegin(i30)+1)
            icount=i30                                               ! increment count of number of tokens found
            if(icol.gt.ilen)then                                     ! no text left
            exit INFINITE
            endif
        enddo INFINITE

    end select

    select case (trim(adjustl(nlls)))
    case ('ignore','','ignoreend')
        ireturn=inotnull
    case default
        ireturn=icount
    end select
    allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
    !allocate(array(ireturn))                                       ! allocate the array to turn

    select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
    case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
    case default             ; ii=1       ; iiii=1                 ! first to last
    end select

    do i20=1,icount                                                ! fill the array with the tokens that were found
        if(iterm(i20).lt.ibegin(i20))then
            select case (trim(adjustl(nlls)))
            case ('ignore','','ignoreend')
            case default
            array(ii)=' '
            ii=ii+iiii
            end select
        else
            array(ii)=input_line(ibegin(i20):iterm(i20))
            ii=ii+iiii
        endif
    enddo
end subroutine split

subroutine resize_string(list, n)
  !> Instance of the array to be resized
  type(string_t), allocatable, intent(inout) :: list(:)
  !> Dimension of the final array size
  integer, intent(in), optional :: n

  type(string_t), allocatable :: tmp(:)
  integer :: this_size, new_size, i
  integer, parameter :: initial_size = 16

  if (allocated(list)) then
    this_size = size(list, 1)
    call move_alloc(list, tmp)
  else
    this_size = initial_size
  end if

  if (present(n)) then
    new_size = n
  else
    new_size = this_size + this_size/2 + 1
  end if

  allocate(list(new_size))

  if (allocated(tmp)) then
    this_size = min(size(tmp, 1), size(list, 1))
    do i = 1, this_size
      call move_alloc(tmp(i)%s, list(i)%s)
    end do
    deallocate(tmp)
  end if

end subroutine resize_string

pure function join(str,sep,trm,left,right) result (string)

!> M_strings::join(3f): append an array of character variables with specified separator into a single CHARACTER variable
!>
!>##NAME
!>    join(3f) - [M_strings:EDITING] append CHARACTER variable array into
!>    a single CHARACTER variable with specified separator
!>    (LICENSE:PD)
!>
!>##SYNOPSIS
!>
!>    pure function join(str,sep,trm,left,right) result (string)
!>
!>     character(len=*),intent(in)          :: str(:)
!>     character(len=*),intent(in),optional :: sep
!>     logical,intent(in),optional          :: trm
!>     character(len=*),intent(in),optional :: right
!>     character(len=*),intent(in),optional :: left
!>     character(len=:),allocatable         :: string
!>
!>##DESCRIPTION
!>      JOIN(3f) appends the elements of a CHARACTER array into a single
!>      CHARACTER variable, with elements 1 to N joined from left to right.
!>      By default each element is trimmed of trailing spaces and the
!>      default separator is a null string.
!>
!>##OPTIONS
!>      STR(:)  array of CHARACTER variables to be joined
!>      SEP     separator string to place between each variable. defaults
!>              to a null string.
!>      LEFT    string to place at left of each element
!>      RIGHT   string to place at right of each element
!>      TRM     option to trim each element of STR of trailing
!>              spaces. Defaults to .TRUE.
!>
!>##RESULT
!>      STRING  CHARACTER variable composed of all of the elements of STR()
!>              appended together with the optional separator SEP placed
!>              between the elements and optional left and right elements.
!>
!>##EXAMPLE
!>
!>  Sample program:
!>
!>   program demo_join
!>   use M_strings, only: join
!>   implicit none
!>   character(len=:),allocatable  :: s(:)
!>   character(len=:),allocatable  :: out
!>   integer                       :: i
!>     s=[character(len=10) :: 'United',' we',' stand,', &
!>     & ' divided',' we fall.']
!>     out=join(s)
!>     write(*,'(a)') out
!>     write(*,'(a)') join(s,trm=.false.)
!>     write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
!>     write(*,'(a)') join(s,sep='<>')
!>     write(*,'(a)') join(s,sep=';',left='[',right=']')
!>     write(*,'(a)') join(s,left='[',right=']')
!>     write(*,'(a)') join(s,left='>>')
!>   end program demo_join
!>
!>  Expected output:
!>
!>   United we stand, divided we fall.
!>   United     we        stand,    divided   we fall.
!>   United    | we       | stand,   | divided  | we fall. |
!>   United    | we       | stand,   | divided  | we fall. |
!>   United    | we       | stand,   | divided  | we fall. |
!>   United<> we<> stand,<> divided<> we fall.<>
!>   [United];[ we];[ stand,];[ divided];[ we fall.];
!>   [United][ we][ stand,][ divided][ we fall.]
!>   >>United>> we>> stand,>> divided>> we fall.
!>
!>##AUTHOR
!>    John S. Urban
!>
!>##LICENSE
!>    Public Domain

character(len=*),intent(in)           :: str(:)
character(len=*),intent(in),optional  :: sep, right, left
logical,intent(in),optional           :: trm
character(len=:),allocatable          :: string
integer                               :: i
logical                               :: trm_local
character(len=:),allocatable          :: sep_local, left_local, right_local

   if(present(sep))then    ;  sep_local=sep      ;  else  ;  sep_local=''      ;  endif
   if(present(trm))then    ;  trm_local=trm      ;  else  ;  trm_local=.true.  ;  endif
   if(present(left))then   ;  left_local=left    ;  else  ;  left_local=''     ;  endif
   if(present(right))then  ;  right_local=right  ;  else  ;  right_local=''    ;  endif

   string=''
   do i = 1,size(str)
      if(trm_local)then
         string=string//left_local//trim(str(i))//right_local//sep_local
      else
         string=string//left_local//str(i)//right_local//sep_local
      endif
   enddo
end function join

end module fpm_strings
