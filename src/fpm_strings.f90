!> This module defines general procedures for **string operations** for both CHARACTER and
!! TYPE(STRING_T) variables
!
!>## general routines for performing __string operations__
!!
!!### Types
!! - **TYPE(STRING_T)** define a type to contain strings of variable length
!!### Type Conversions
!! - [[F_STRING]]  return Fortran **CHARACTER** variable when given a C-like array of
!!                 single characters terminated with a C_NULL_CHAR **CHARACTER**
!! - [[STR]]  Converts **INTEGER** or** LOGICAL** to **CHARACTER** string
!!### Case
!! - [[LOWER]]  Changes a string to lowercase over optional specified column range
!!### Parsing and joining
!! - [[SPLIT]]  parse string on delimiter characters and store tokens into an allocatable array
!! - [[SPLIT_FIRST_LAST]]  Computes the first and last indices of tokens in input string, delimited by the characters in set,
!!                         and stores them into first and last output arrays.
!! - [[STRING_CAT]]  Concatenate an array of **type(string_t)** into a single **CHARACTER** variable
!! - [[JOIN]]  append an array of **CHARACTER** variables into a single **CHARACTER** variable
!!### Testing
!! - [[STR_ENDS_WITH]]  test if a **CHARACTER** string or array ends with a specified suffix
!! - [[STRING_ARRAY_CONTAINS]]  Check if array of **TYPE(STRING_T)** matches a particular **CHARACTER** string
!! - **OPERATOR(.IN.)**  Check if array of **TYPE(STRING_T)** matches a particular **CHARACTER** string
!! - [[GLOB]]  function compares text strings, one of which can have wildcards ('*' or '?').
!! - [[IS_FORTRAN_NAME]]  determine whether a string is an acceptable Fortran entity name
!! - [[TO_FORTRAN_NAME]]  replace allowed special but unusuable characters in names with underscore
!!### Whitespace
!! - [[NOTABS]]  subroutine to expand tab characters assuming a tab space every eight characters
!! - [[DILATE]]  function to expand tab characters assuming a tab space every eight characters
!! - [[LEN_TRIM]]  Determine total trimmed length of **STRING_T** array
!!### Miscellaneous
!! - [[FNV_1A]]  Hash a **CHARACTER(*)** string of default kind or a **TYPE(STRING_T)** array
!! - [[REPLACE]]  Returns string with characters in charset replaced with target_char.
!! - [[RESIZE]]  increase the size of a **TYPE(STRING_T)** array by N elements
!!
module fpm_strings
use iso_fortran_env, only: int64
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
use iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_associated, c_f_pointer, c_size_t
implicit none

private
public :: f_string, lower, upper, split, split_first_last, split_lines_first_last, str_ends_with, string_t, str_begins_with_str
public :: to_fortran_name, is_fortran_name, is_valid_feature_name, add_strings
public :: string_array_contains, string_cat, len_trim, operator(.in.), fnv_1a
public :: replace, resize, str, join, glob
public :: notabs, dilate, remove_newline_characters, remove_characters_in_set
public :: operator(==)

!> Module naming
public :: is_valid_module_name, is_valid_module_prefix, &
          has_valid_custom_prefix, has_valid_standard_prefix, &
          module_prefix_template, module_prefix_type

type string_t
    character(len=:), allocatable :: s
end type

interface len_trim
    module procedure :: string_len_trim
    module procedure :: strings_len_trim
end interface len_trim

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
    procedure :: str_ends_with_any_string
end interface str_ends_with

interface str
    module procedure str_int, str_int64, str_logical
end interface

interface string_t
    module procedure new_string_t
end interface string_t

interface f_string
    module procedure f_string, f_string_cptr, f_string_cptr_n
end interface f_string

interface operator(==)
    module procedure string_is_same
    module procedure string_arrays_same
end interface

interface add_strings
    module procedure add_strings_one
    module procedure add_strings_many
end interface add_strings

contains

!> test if a CHARACTER string ends with a specified suffix
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

!> test if a CHARACTER string ends with any of an array of suffixs
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

!> Test if a CHARACTER string ends with any of an array of string suffixs
pure logical function str_ends_with_any_string(s, e) result(r)
    character(*), intent(in) :: s
    type(string_t), intent(in) :: e(:)

    integer :: i

    r = .true.
    do i=1,size(e)

        if (str_ends_with(s,trim(e(i)%s))) return

    end do
    r = .false.

end function str_ends_with_any_string

!> test if a CHARACTER string begins with a specified prefix
pure logical function str_begins_with_str(s, e, case_sensitive) result(r)
    character(*), intent(in) :: s, e
    logical, optional, intent(in) :: case_sensitive ! Default option: case sensitive
    integer :: n1, n2
    logical :: lower_case

    ! Check if case sensitive
    if (present(case_sensitive)) then
        lower_case = .not.case_sensitive
    else
        lower_case = .false.
    end if

    n1 = 1
    n2 = 1 + len(e)-1
    if (n2 > len(s)) then
        r = .false.
    elseif (lower_case) then
        r = lower(s(n1:n2)) == lower(e)
    else
        r = (s(n1:n2) == e)
    end if
end function str_begins_with_str

!> return Fortran character variable when given a C-like array of
!! single characters terminated with a C_NULL_CHAR character
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


!> return Fortran character variable when given a null-terminated c_ptr
function f_string_cptr(cptr) result(s)
    type(c_ptr), intent(in), value :: cptr
    character(len=:,kind=c_char), allocatable :: s

    interface
        function c_strlen(s) result(r) bind(c, name="strlen")
            import c_size_t, c_ptr
            type(c_ptr), intent(in), value :: s
            integer(kind=c_size_t) :: r
        end function
    end interface

    s = f_string_cptr_n(cptr, c_strlen(cptr))
end function

!> return Fortran character variable when given a null-terminated c_ptr and its length
function f_string_cptr_n(cptr, n) result(s)
    type(c_ptr), intent(in), value :: cptr
    integer(kind=c_size_t), intent(in) :: n
    character(len=n,kind=c_char) :: s
    character(len=n,kind=c_char), pointer :: sptr

    call c_f_pointer(cptr, sptr)
    s = sptr
end function

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


!>Author: John S. Urban
!!License: Public Domain
!! Changes a string to lowercase over optional specified column range
elemental pure function lower(str,begin,end) result (string)

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

  !!License: Public Domain
 !! Changes a string to upprtcase over optional specified column range
elemental pure function upper(str,begin,end) result (string)

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
        case ('a':'z')
            string(i:i) = char(iachar(str(i:i))-32)   ! change letter to capitalized
        case default
        end select
    end do

end function upper

!> Helper function to generate a new string_t instance
!>  (Required due to the allocatable component)
function new_string_t(s) result(string)
    character(*), intent(in) :: s
    type(string_t) :: string

    string%s = s

end function new_string_t

!> Check if array of TYPE(STRING_T) matches a particular CHARACTER string
!!
logical function string_array_contains(search_string,array)
    character(*), intent(in) :: search_string
    type(string_t), intent(in) :: array(:)

    integer :: i

    string_array_contains = any([(array(i)%s==search_string, &
                                   i=1,size(array))])

end function string_array_contains

!> Concatenate an array of type(string_t) into
!>  a single CHARACTER variable
function string_cat(strings,delim) result(cat)
    type(string_t), intent(in) :: strings(:)
    character(*), intent(in), optional :: delim
    character(:), allocatable :: cat

    integer :: i
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

!> Determine total trimmed length of `string_t` array
pure function strings_len_trim(strings) result(n)
    type(string_t), intent(in) :: strings(:)
    integer :: i, n

    n = 0
    do i=1,size(strings)
        n = n + len_trim(strings(i)%s)
    end do

end function strings_len_trim

!> Determine total trimmed length of `string_t` array
elemental integer function string_len_trim(string) result(n)
    type(string_t), intent(in) :: string

    if (allocated(string%s)) then
        n = len_trim(string%s)
    else
        n = 0
    end if

end function string_len_trim

!>Author: John S. Urban
!!License: Public Domain
!! parse string on delimiter characters and store tokens into an allocatable array
subroutine split(input_line,array,delimiters,order,nulls)
    !! given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
    !!
    !! * by default adjacent delimiters in the input string do not create an empty string in the output array
    !! * no quoting of delimiters is supported
    character(len=*),intent(in)              :: input_line  !! input string to tokenize
    character(len=*),optional,intent(in)     :: delimiters  !! list of delimiter characters
    character(len=*),optional,intent(in)     :: order       !! order of output array sequential|[reverse|right]
    character(len=*),optional,intent(in)     :: nulls       !! return strings composed of delimiters or not ignore|return|ignoreend
    character(len=:),allocatable,intent(out) :: array(:)    !! output array of tokens

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
        if(delimiters/='')then                                       ! if DELIMITERS was specified and not null use it
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
            if(index(dlim(1:idlim),input_line(icol:icol))==0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
                ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
                IF(ifound>0)then
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
            if(icol>ilen)then                                     ! no text left
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
        if(iterm(i20)<ibegin(i20))then
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

!! Author: Milan Curcic
!! Computes the first and last indices of tokens in input string, delimited
!! by the characters in set, and stores them into first and last output
!! arrays.
pure subroutine split_first_last(string, set, first, last)
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    integer, allocatable, dimension(:) :: istart, iend
    integer :: p, n, slen

    slen = len(string)
    allocate(istart(slen + 1), iend(slen + 1))

    n = 0
    if (slen > 0) then
        p = 0
        do while (p < slen)
            n = n + 1
            istart(n) = min(p + 1, slen)
            call split_pos(string, set, p)
            iend(n) = p - 1
        end do
    end if

    first = istart(:n)
    last = iend(:n)

end subroutine split_first_last

!! Author: Federico Perini
!! Computes the first and last indices of lines in input string, delimited
!! by either CR, LF, or CRLF, and stores them into first and last output
!! arrays.
pure subroutine split_lines_first_last(string, first, last)
    character(*), intent(in) :: string
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    integer, allocatable, dimension(:) :: istart, iend
    integer :: p, n, slen
    character, parameter :: CR = achar(13)
    character, parameter :: LF = new_line('A')

    slen = len(string)
    allocate(istart(slen + 1), iend(slen + 1))

    n = 0
    if (slen > 0) then
        p = 1
        do while (p <= slen)
            
            if (index(CR//LF, string(p:p)) == 0) then
                n = n + 1
                istart(n) = p
                do while (p <= slen)
                    if (index(CR//LF, string(p:p)) /= 0) exit
                    p = p + 1
                end do
                iend(n) = p - 1
            end if
            
            ! Handle Windows CRLF by skipping LF after CR
            if (p < slen) then 
               if (string(p:p) == CR .and. string(p+1:p+1) == LF) p = p + 1
            endif
            
            p = p + 1
        end do
    end if

    first = istart(:n)
    last = iend(:n)

end subroutine split_lines_first_last

!! Author: Milan Curcic
!! If back is absent, computes the leftmost token delimiter in string whose
!! position is > pos. If back is present and true, computes the rightmost
!! token delimiter in string whose position is < pos. The result is stored
!! in pos.
pure subroutine split_pos(string, set, pos, back)
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, intent(in out) :: pos
    logical, intent(in), optional :: back

    logical :: backward
    integer :: result_pos, bound

    if (len(string) == 0) then
        pos = 1
        return
    end if

    !TODO use optval when implemented in stdlib
    !backward = optval(back, .false.)
    backward = .false.
    if (present(back)) backward = back

    if (backward) then
        bound = min(len(string), max(pos - 1, 0))
        result_pos = scan(string(:bound), set, back=.true.)
    else
        result_pos = scan(string(min(pos + 1, len(string)):), set) + pos
        if (result_pos < pos + 1) result_pos = len(string) + 1
    end if

    pos = result_pos

end subroutine split_pos

!> Returns string with characters in charset replaced with target_char.
pure function replace(string, charset, target_char) result(res)
    character(*), intent(in) :: string
    character, intent(in) :: charset(:), target_char
    character(len(string)) :: res
    integer :: n
    res = string
    do n = 1, len(string)
        if (any(string(n:n) == charset)) then
            res(n:n) = target_char
        end if
    end do
end function replace

!> increase the size of a TYPE(STRING_T) array by N elements
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

!>AUTHOR: John S. Urban
!!LICENSE: Public Domain
!>
!!##NAME
!!    join(3f) - [M_strings:EDITING] append CHARACTER variable array into
!!    a single CHARACTER variable with specified separator
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function join(str,sep,trm,left,right,start,end) result (string)
!!
!!     character(len=*),intent(in)          :: str(:)
!!     character(len=*),intent(in),optional :: sep
!!     logical,intent(in),optional          :: trm
!!     character(len=*),intent(in),optional :: right
!!     character(len=*),intent(in),optional :: left
!!     character(len=*),intent(in),optional :: start
!!     character(len=*),intent(in),optional :: end
!!     character(len=:),allocatable         :: string
!!
!!##DESCRIPTION
!!   JOIN(3f) appends the elements of a CHARACTER array into a single
!!   CHARACTER variable, with elements 1 to N joined from left to right.
!!   By default each element is trimmed of trailing spaces and the
!!   default separator is a null string.
!!
!!##OPTIONS
!!      STR(:)  array of CHARACTER variables to be joined
!!      SEP     separator string to place between each variable. defaults
!!              to a null string.
!!      LEFT    string to place at left of each element
!!      RIGHT   string to place at right of each element
!!      START   prefix string
!!      END     suffix string
!!      TRM     option to trim each element of STR of trailing
!!              spaces. Defaults to .TRUE.
!!
!!##RESULT
!!      STRING  CHARACTER variable composed of all of the elements of STR()
!!              appended together with the optional separator SEP placed
!!              between the elements.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!   program demo_join
!!   use M_strings, only: join
!!   implicit none
!!   character(len=:),allocatable  :: s(:)
!!   character(len=:),allocatable  :: out
!!   integer                       :: i
!!     s=[character(len=10) :: 'United',' we',' stand,', &
!!     & ' divided',' we fall.']
!!     out=join(s)
!!     write(*,'(a)') out
!!     write(*,'(a)') join(s,trm=.false.)
!!     write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
!!     write(*,'(a)') join(s,sep='<>')
!!     write(*,'(a)') join(s,sep=';',left='[',right=']')
!!     write(*,'(a)') join(s,left='[',right=']')
!!     write(*,'(a)') join(s,left='>>')
!!   end program demo_join
!!
!!  Expected output:
!!
!!   United we stand, divided we fall.
!!   United     we        stand,    divided   we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United<> we<> stand,<> divided<> we fall.
!!   [United];[ we];[ stand,];[ divided];[ we fall.]
!!   [United][ we][ stand,][ divided][ we fall.]
!!   >>United>> we>> stand,>> divided>> we fall.
pure function join(str,sep,trm,left,right,start,end) result (string)

! @(#)M_strings::join(3f): merge string array into a single CHARACTER value adding specified separators, caps, prefix and suffix

character(len=*),intent(in)          :: str(:)
character(len=*),intent(in),optional :: sep, right, left, start, end
logical,intent(in),optional          :: trm
character(len=:),allocatable         :: sep_local, left_local, right_local
character(len=:),allocatable         :: string
logical                              :: trm_local
integer                              :: i
   if(present(sep))then   ; sep_local=sep     ; else ; sep_local=''     ; endif
   if(present(trm))then   ; trm_local=trm     ; else ; trm_local=.true. ; endif
   if(present(left))then  ; left_local=left   ; else ; left_local=''    ; endif
   if(present(right))then ; right_local=right ; else ; right_local=''   ; endif
   string=''
   if(size(str)==0)then
      string=string//left_local//right_local
   else
      do i = 1,size(str)-1
         if(trm_local)then
            string=string//left_local//trim(str(i))//right_local//sep_local
         else
            string=string//left_local//str(i)//right_local//sep_local
         endif
      enddo
      if(trm_local)then
         string=string//left_local//trim(str(i))//right_local
      else
         string=string//left_local//str(i)//right_local
      endif
   endif
   if(present(start))string=start//string
   if(present(end))string=string//end
end function join

!>AUTHOR: John S. Urban
!!LICENSE: Public Domain
!>
!!## NAME
!!    glob(3f) - [fpm_strings:COMPARE] compare given string for match to
!!    pattern which may contain wildcard characters
!!    (LICENSE:PD)
!!
!!## SYNOPSIS
!!
!!    logical function glob(string, pattern )
!!
!!     character(len=*),intent(in) :: string
!!     character(len=*),intent(in) :: pattern
!!
!!## DESCRIPTION
!!   glob(3f) compares given STRING for match to PATTERN which may
!!   contain wildcard characters.
!!
!!   In this version to get a match the entire string must be described
!!   by PATTERN. Trailing whitespace is significant, so trim the input
!!   string to have trailing whitespace ignored.
!!
!!## OPTIONS
!!    string   the input string to test to see if it contains the pattern.
!!    pattern  the following simple globbing options are available
!!
!!             o "?" matching any one character
!!             o "*" matching zero or more characters.
!!               Do NOT use adjacent asterisks.
!!             o Both strings may have trailing spaces which
!!               are ignored.
!!             o There is no escape character, so matching strings with
!!               literal question mark and asterisk is problematic.
!!
!!## EXAMPLES
!!
!!   Example program
!!
!!    program demo_glob
!!    implicit none
!!    ! This main() routine passes a bunch of test strings
!!    ! into the above code.  In performance comparison mode,
!!    ! it does that over and over. Otherwise, it does it just
!!    ! once. Either way, it outputs a passed/failed result.
!!    !
!!    integer :: nReps
!!    logical :: allpassed
!!    integer :: i
!!     allpassed = .true.
!!
!!     nReps = 10000
!!     ! Can choose as many repetitions as you're expecting
!!     ! in the real world.
!!     nReps = 1
!!
!!     do i=1,nReps
!!      ! Cases with repeating character sequences.
!!      allpassed=allpassed .and. test("a*abab", "a*b", .true.)
!!      !!cycle
!!      allpassed=allpassed .and. test("ab", "*?", .true.)
!!      allpassed=allpassed .and. test("abc", "*?", .true.)
!!      allpassed=allpassed .and. test("abcccd", "*ccd", .true.)
!!      allpassed=allpassed .and. test("bLah", "bLaH", .false.)
!!      allpassed=allpassed .and. test("mississippi", "*sip*", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. &
!!       & test("mississipissippi", "*issip*ss*", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. &
!!       & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.)
!!      allpassed=allpassed .and. test("xyxyxyzyxyz", "xy*z*xyz", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("mississippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("ababac", "*abac*", .true.)
!!      allpassed=allpassed .and. test("aaazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("a12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12b12", "a12b", .false.)
!!      allpassed=allpassed .and. test("a12b12", "*12*12*", .true.)
!!
!!      ! Additional cases where the '*' char appears in the tame string.
!!      allpassed=allpassed .and. test("*", "*", .true.)
!!      allpassed=allpassed .and. test("a*r", "a*", .true.)
!!      allpassed=allpassed .and. test("a*ar", "a*aar", .false.)
!!
!!      ! More double wildcard scenarios.
!!      allpassed=allpassed .and. test("XYXYXYZYXYz", "XY*Z*XYz", .true.)
!!      allpassed=allpassed .and. test("missisSIPpi", "*SIP*", .true.)
!!      allpassed=allpassed .and. test("mississipPI", "*issip*PI", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*Sip*", .false.)
!!      allpassed=allpassed .and. test("abAbac", "*Abac*", .true.)
!!      allpassed=allpassed .and. test("aAazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("A12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12B12", "*12*12*", .true.)
!!      allpassed=allpassed .and. test("oWn", "*oWn*", .true.)
!!
!!      ! Completely tame (no wildcards) cases.
!!      allpassed=allpassed .and. test("bLah", "bLah", .true.)
!!
!!      ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
!!      allpassed=allpassed .and. test("a", "*?", .true.)
!!
!!      ! More mixed wildcard tests including coverage for false positives.
!!      allpassed=allpassed .and. test("a", "??", .false.)
!!      allpassed=allpassed .and. test("ab", "?*?", .true.)
!!      allpassed=allpassed .and. test("ab", "*?*?*", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*?", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*&?", .false.)
!!      allpassed=allpassed .and. test("abcd", "?b*??", .true.)
!!      allpassed=allpassed .and. test("abcd", "?a*??", .false.)
!!      allpassed=allpassed .and. test("abcd", "?**?c?", .true.)
!!      allpassed=allpassed .and. test("abcd", "?**?d?", .false.)
!!      allpassed=allpassed .and. test("abcde", "?*b*?*d*?", .true.)
!!
!!      ! Single-character-match cases.
!!      allpassed=allpassed .and. test("bLah", "bL?h", .true.)
!!      allpassed=allpassed .and. test("bLaaa", "bLa?", .false.)
!!      allpassed=allpassed .and. test("bLah", "bLa?", .true.)
!!      allpassed=allpassed .and. test("bLaH", "?Lah", .false.)
!!      allpassed=allpassed .and. test("bLaH", "?LaH", .true.)
!!
!!      ! Many-wildcard scenarios.
!!      allpassed=allpassed .and. test(&
!!      &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!!      &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
!!      &"a*a*a*a*a*a*aa*aaa*a*a*b",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacac&
!!      &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacaca&
!!      &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacad&
!!      &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacad&
!!      &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("aaabbaabbaab", "*aabbaa*a*", .true.)
!!      allpassed=allpassed .and. &
!!      test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
!!      &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaaa",&
!!      &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaa",&
!!      &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!      &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
!!      &*abc*abc*abc*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!      &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc*abcd*abcd*abc*abcd",&
!!      &"abc*abc*abc*abc*abc", .false.)
!!      allpassed=allpassed .and. test( "abc*abcd*abcd*abc*abcd*abcd&
!!      &*abc*abcd*abc*abc*abcd", &
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc",&
!!      &"********a********b********c********", .true.)
!!      allpassed=allpassed .and.&
!!      &test("********a********b********c********", "abc", .false.)
!!      allpassed=allpassed .and. &
!!      &test("abc", "********a********b********b********", .false.)
!!      allpassed=allpassed .and. test("*abc*", "***a*b*c***", .true.)
!!
!!      ! A case-insensitive algorithm test.
!!      ! allpassed=allpassed .and. test("mississippi", "*issip*PI", .true.)
!!     enddo
!!
!!     if (allpassed)then
!!        write(*,'(a)')"Passed",nReps
!!     else
!!        write(*,'(a)')"Failed"
!!     endif
!!    contains
!!    ! This is a test program for wildcard matching routines.
!!    ! It can be used either to test a single routine for correctness,
!!    ! or to compare the timings of two (or more) different wildcard
!!    ! matching routines.
!!    !
!!    function test(tame, wild, bExpectedResult) result(bpassed)
!!    use fpm_strings, only : glob
!!       character(len=*) :: tame
!!       character(len=*) :: wild
!!       logical          :: bExpectedResult
!!       logical          :: bResult
!!       logical          :: bPassed
!!       bResult = .true.    ! We'll do "&=" cumulative checking.
!!       bPassed = .false.   ! Assume the worst.
!!       write(*,*)repeat('=',79)
!!       bResult = glob(tame, wild) ! Call a wildcard matching routine.
!!
!!       ! To assist correctness checking, output the two strings in any
!!       ! failing scenarios.
!!       if (bExpectedResult .eqv. bResult) then
!!          bPassed = .true.
!!          if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
!!       else
!!          if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
!!       endif
!!
!!    end function test
!!    end program demo_glob
!!
!!   Expected output
!!
!!
!!## REFERENCE
!!   The article "Matching Wildcards: An Empirical Way to Tame an Algorithm"
!!   in Dr Dobb's Journal, By Kirk J. Krauss, October 07, 2014
!!
function glob(tame,wild)

! @(#)fpm_strings::glob(3f): function compares text strings, one of which can have wildcards ('*' or '?').

logical                    :: glob       !! result of test
character(len=*)           :: tame       !! A string without wildcards to compare to the globbing expression
character(len=*)           :: wild       !! A (potentially) corresponding string with wildcards
character(len=len(tame)+1) :: tametext
character(len=len(wild)+1) :: wildtext
character(len=1),parameter :: NULL=char(0)
integer                    :: wlen
integer                    :: ti, wi
integer                    :: i
character(len=:),allocatable :: tbookmark, wbookmark
! These two values are set when we observe a wildcard character. They
! represent the locations, in the two strings, from which we start once we've observed it.
   tametext=tame//NULL
   wildtext=wild//NULL
   tbookmark = NULL
   wbookmark = NULL
   wlen=len(wild)
   wi=1
   ti=1
   do                                            ! Walk the text strings one character at a time.
      if(wildtext(wi:wi) == '*')then             ! How do you match a unique text string?
         do i=wi,wlen                            ! Easy: unique up on it!
            if(wildtext(wi:wi)=='*')then
               wi=wi+1
            else
               exit
            endif
         enddo
         if(wildtext(wi:wi)==NULL) then        ! "x" matches "*"
            glob=.true.
            return
         endif
         if(wildtext(wi:wi) /= '?') then
            ! Fast-forward to next possible match.
            do while (tametext(ti:ti) /= wildtext(wi:wi))
               ti=ti+1
               if (tametext(ti:ti)==NULL)then
                  glob=.false.
                  return                         ! "x" doesn't match "*y*"
               endif
            enddo
         endif
         wbookmark = wildtext(wi:)
         tbookmark = tametext(ti:)
      elseif(tametext(ti:ti) /= wildtext(wi:wi) .and. wildtext(wi:wi) /= '?') then
         ! Got a non-match. If we've set our bookmarks, back up to one or both of them and retry.
         if(wbookmark/=NULL) then
            if(wildtext(wi:)/= wbookmark) then
               wildtext = wbookmark;
               wlen=len_trim(wbookmark)
               wi=1
               ! Don't go this far back again.
               if (tametext(ti:ti) /= wildtext(wi:wi)) then
                  tbookmark=tbookmark(2:)
                  tametext = tbookmark
                  ti=1
                  cycle                          ! "xy" matches "*y"
               else
                  wi=wi+1
               endif
            endif
            if (tametext(ti:ti)/=NULL) then
               ti=ti+1
               cycle                             ! "mississippi" matches "*sip*"
            endif
         endif
         glob=.false.
         return                                  ! "xy" doesn't match "x"
      endif
      ti=ti+1
      wi=wi+1
      if (tametext(ti:ti)==NULL) then          ! How do you match a tame text string?
         if(wildtext(wi:wi)/=NULL)then
            do while (wildtext(wi:wi) == '*')    ! The tame way: unique up on it!
               wi=wi+1                           ! "x" matches "x*"
               if(wildtext(wi:wi)==NULL)exit
            enddo
         endif
         if (wildtext(wi:wi)==NULL)then
            glob=.true.
            return                               ! "x" matches "x"
         endif
         glob=.false.
         return                                  ! "x" doesn't match "xy"
      endif
   enddo
end function glob

!> Returns the length of the string representation of 'i'
pure integer function str_int_len(i) result(sz)
integer, intent(in) :: i
integer, parameter :: MAX_STR = 100
character(MAX_STR) :: s
! If 's' is too short (MAX_STR too small), Fortran will abort with:
! "Fortran runtime error: End of record"
write(s, '(i0)') i
sz = len_trim(s)
end function

!> Converts integer "i" to string
pure function str_int(i) result(s)
integer, intent(in) :: i
character(len=str_int_len(i)) :: s
write(s, '(i0)') i
end function

!> Returns the length of the string representation of 'i'
pure integer function str_int64_len(i) result(sz)
integer(int64), intent(in) :: i
integer, parameter :: MAX_STR = 100
character(MAX_STR) :: s
! If 's' is too short (MAX_STR too small), Fortran will abort with:
! "Fortran runtime error: End of record"
write(s, '(i0)') i
sz = len_trim(s)
end function

!> Converts integer "i" to string
pure function str_int64(i) result(s)
integer(int64), intent(in) :: i
character(len=str_int64_len(i)) :: s
write(s, '(i0)') i
end function

!> Returns the length of the string representation of 'l'
pure integer function str_logical_len(l) result(sz)
logical, intent(in) :: l
if (l) then
    sz = 6
else
    sz = 7
end if
end function

!> Converts logical "l" to string
pure function str_logical(l) result(s)
logical, intent(in) :: l
character(len=str_logical_len(l)) :: s
if (l) then
    s = ".true."
else
    s = ".false."
end if
end function

!> Returns string with special characters replaced with an underscore.
!! For now, only a hyphen is treated as a special character, but this can be
!! expanded to other characters if needed.
pure function to_fortran_name(string) result(res)
    character(*), intent(in) :: string
    character(len(string)) :: res
    character, parameter :: SPECIAL_CHARACTERS(*) = ['-']
    res = replace(string, SPECIAL_CHARACTERS, '_')
end function to_fortran_name

elemental function is_fortran_name(line) result (lout)
! determine if a string is a valid Fortran name ignoring trailing spaces
! (but not leading spaces)
    character(len=*),parameter   :: int='0123456789'
    character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=*),parameter   :: allowed=upper//lower//int//'_'
    character(len=*),intent(in)  :: line
    character(len=:),allocatable :: name
    logical                      :: lout
        name=trim(line)
        if(len(name)/=0)then
            lout = .true.                                  &
             & .and. verify(name(1:1), lower//upper) == 0  &
             & .and. verify(name,allowed) == 0             &
             & .and. len(name) <= 63
        else
            lout = .false.
        endif
end function is_fortran_name

!> Check that a module name fits the current naming rules:
!> 1) It must be a valid FORTRAN name (<=63 chars, begin with letter, "_" is only allowed non-alphanumeric)
!> 2) It must begin with the package name
!> 3) If longer, package name must be followed by default separator plus at least one char
logical function is_valid_module_name(module_name,package_name,custom_prefix,enforce_module_names) result(valid)

    type(string_t), intent(in) :: module_name
    type(string_t), intent(in) :: package_name
    type(string_t), intent(in) :: custom_prefix
    logical       , intent(in) :: enforce_module_names


    !> Basic check: check the name is Fortran-compliant
    valid = is_fortran_name(module_name%s); if (.not.valid) return

    !> FPM package enforcing: check that the module name begins with the package name
    if (enforce_module_names) then

        ! Default prefixing is always valid
        valid = has_valid_standard_prefix(module_name,package_name)

        ! If a custom prefix was validated, it provides additional naming options
        ! Because they never overlap with the default prefix, the former is always an option
        if (len_trim(custom_prefix)>0 .and. .not.valid) &
            valid = has_valid_custom_prefix(module_name,custom_prefix)

    end if

end function is_valid_module_name

!> Check that a custom module prefix fits the current naming rules:
!> 1) Only alphanumeric characters (no spaces, dashes, underscores or other characters)
!> 2) Does not begin with a number (Fortran-compatible syntax)
logical function is_valid_module_prefix(module_prefix) result(valid)

    type(string_t), intent(in) :: module_prefix

    character(len=*),parameter :: num='0123456789'
    character(len=*),parameter :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=*),parameter :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=*),parameter :: alpha  =upper//lower
    character(len=*),parameter :: allowed=alpha//num

    character(len=:),allocatable :: name

    name = trim(module_prefix%s)

    if (len(name)>0 .and. len(name)<=63) then
        valid = verify(name(1:1), alpha) == 0 .and. &
                verify(name,allowed)     == 0
    else
        valid = .false.
    endif

end function is_valid_module_prefix


type(string_t) function module_prefix_template(project_name,custom_prefix) result(prefix)
    type(string_t), intent(in) :: project_name
    type(string_t), intent(in) :: custom_prefix

    if (is_valid_module_prefix(custom_prefix)) then

        prefix = string_t(trim(custom_prefix%s)//"_")

    else

        prefix = string_t(to_fortran_name(project_name%s)//"__")

    end if

end function module_prefix_template

type(string_t) function module_prefix_type(project_name,custom_prefix) result(ptype)
    type(string_t), intent(in) :: project_name
    type(string_t), intent(in) :: custom_prefix

    if (is_valid_module_prefix(custom_prefix)) then
        ptype = string_t("custom")
    else
        ptype = string_t("default")
    end if

end function module_prefix_type

!> Check that a module name is prefixed with a custom prefix:
!> 1) It must be a valid FORTRAN name subset (<=63 chars, begin with letter, only alphanumeric allowed)
!> 2) It must begin with the prefix
!> 3) If longer, package name must be followed by default separator ("_") plus at least one char
logical function has_valid_custom_prefix(module_name,custom_prefix) result(valid)

    type(string_t), intent(in) :: module_name
    type(string_t), intent(in) :: custom_prefix

    !> custom_module separator: single underscore
    character(*), parameter :: SEP = "_"

    logical :: is_same,has_separator,same_beginning
    integer :: lpkg,lmod,lsep

    !> Basic check: check that both names are individually valid
    valid = is_fortran_name(module_name%s) .and. &
            is_valid_module_prefix(custom_prefix)

    !> FPM package enforcing: check that the module name begins with the custom prefix
    if (valid) then

        !> Query string lengths
        lpkg  = len_trim(custom_prefix)
        lmod  = len_trim(module_name)
        lsep  = len_trim(SEP)

        same_beginning = str_begins_with_str(module_name%s,custom_prefix%s,case_sensitive=.false.)

        is_same = lpkg==lmod .and. same_beginning

        if (lmod>=lpkg+lsep) then
           has_separator = str_begins_with_str(module_name%s(lpkg+1:lpkg+lsep),SEP)
        else
           has_separator = .false.
        endif

        !> 2) It must begin with the package name.
        !> 3) It can be equal to the package name, or, if longer, must be followed by the
        !     default separator plus at least one character
        !> 4) Package name must not end with an underscore
        valid = same_beginning .and. (is_same .or. (lmod>lpkg+lsep .and. has_separator))

    end if

end function has_valid_custom_prefix


!> Check that a module name is prefixed with the default package prefix:
!> 1) It must be a valid FORTRAN name (<=63 chars, begin with letter, "_" is only allowed non-alphanumeric)
!> 2) It must begin with the package name
!> 3) If longer, package name must be followed by default separator plus at least one char
logical function has_valid_standard_prefix(module_name,package_name) result(valid)

    type(string_t), intent(in) :: module_name
    type(string_t), intent(in) :: package_name

    !> Default package__module separator: two underscores
    character(*), parameter :: SEP = "__"

    character(len=:), allocatable :: fortranized_pkg
    logical :: is_same,has_separator,same_beginning
    integer :: lpkg,lmod,lsep

    !> Basic check: check the name is Fortran-compliant
    valid = is_fortran_name(module_name%s)

    !> FPM package enforcing: check that the module name begins with the package name
    if (valid) then

        fortranized_pkg = to_fortran_name(package_name%s)

        !> Query string lengths
        lpkg  = len_trim(fortranized_pkg)
        lmod  = len_trim(module_name)
        lsep  = len_trim(SEP)

        same_beginning = str_begins_with_str(module_name%s,fortranized_pkg,case_sensitive=.false.)

        is_same = lpkg==lmod .and. same_beginning

        if (lmod>=lpkg+lsep) then
           has_separator = str_begins_with_str(module_name%s(lpkg+1:lpkg+lsep),SEP)
        else
           has_separator = .false.
        endif

        !> 2) It must begin with the package name.
        !> 3) It can be equal to the package name, or, if longer, must be followed by the
        !     default separator plus at least one character
        !> 4) Package name must not end with an underscore
        valid = is_fortran_name(fortranized_pkg) .and. &
                fortranized_pkg(lpkg:lpkg)/='_' .and. &
                (same_beginning .and. (is_same .or. (lmod>lpkg+lsep .and. has_separator)))

    end if

end function has_valid_standard_prefix

!> Check that two string _objects_ are exactly identical
pure logical function string_is_same(this,that)
   !> two strings to be compared
   type(string_t), intent(in) :: this, that

   integer :: i

   string_is_same = .false.

   if (allocated(this%s).neqv.allocated(that%s)) return
   if (allocated(this%s)) then
      if (.not.len(this%s)==len(that%s)) return
      if (.not.len_trim(this%s)==len_trim(that%s)) return
      do i=1,len_trim(this%s)
         if (.not.(this%s(i:i)==that%s(i:i))) return
      end do
   end if

   ! All checks passed
   string_is_same = .true.

end function string_is_same

!> Check that two allocatable string _object_ arrays are exactly identical
pure logical function string_arrays_same(this,that)
   !> two string arrays to be compared
   type(string_t), allocatable, intent(in) :: this(:), that(:)

   integer :: i

   string_arrays_same = .false.

   if (allocated(this).neqv.allocated(that)) return
   if (allocated(this)) then
      if (.not.(size(this)==size(that))) return
      if (.not.(ubound(this,1)==ubound(that,1))) return
      if (.not.(lbound(this,1)==lbound(that,1))) return
      do i=lbound(this,1),ubound(this,1)
         if (.not.string_is_same(this(i),that(i))) return
      end do
   end if

   ! All checks passed
   string_arrays_same = .true.

end function string_arrays_same

! Remove all characters from a set from a string
subroutine remove_characters_in_set(string,set,replace_with)
    character(len=:), allocatable, intent(inout) :: string
    character(*), intent(in) :: set
    character, optional, intent(in) :: replace_with ! Replace with this character instead of removing

    integer :: feed,length

    if (.not.allocated(string)) return
    if (len(set)<=0) return

    length = len(string)
    feed   = scan(string,set)

    do while (length>0 .and. feed>0)

        ! Remove heading
        if (length==1) then
            string = ""

        elseif (feed==1) then
            string = string(2:length)

        ! Remove trailing
        elseif (feed==length) then
            string = string(1:length-1)

        ! In between: replace with given character
        elseif (present(replace_with)) then
            string(feed:feed) = replace_with
        ! Or just remove
        else
            string = string(1:feed-1)//string(feed+1:length)
        end if

        length = len(string)
        feed   = scan(string,set)

    end do

end subroutine remove_characters_in_set

! Remove all new line characters from the current string, replace them with spaces
subroutine remove_newline_characters(string)
    type(string_t), intent(inout) :: string

    integer :: feed,length

    character(*), parameter :: CRLF  = achar(13)//new_line('a')
    character(*), parameter :: SPACE = ' '

    call remove_characters_in_set(string%s,set=CRLF,replace_with=SPACE)

end subroutine remove_newline_characters

!>AUTHOR: John S. Urban
!!LICENSE: Public Domain
!>
!!### NAME
!!   notabs(3f) - [fpm_strings:NONALPHA] expand tab characters
!!   (LICENSE:PD)
!!
!!### SYNOPSIS
!!
!!    subroutine notabs(INSTR,OUTSTR,ILEN)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=*),intent=(out) :: OUTSTR
!!     integer,intent=(out)          :: ILEN
!!
!!### DESCRIPTION
!!   NOTABS() converts tabs in INSTR to spaces in OUTSTR while maintaining
!!   columns. It assumes a tab is set every 8 characters. Trailing spaces
!!   are removed.
!!
!!   In addition, trailing carriage returns and line feeds are removed
!!   (they are usually a problem created by going to and from MSWindows).
!!
!!   What are some reasons for removing tab characters from an input line?
!!   Some Fortran compilers have problems with tabs, as tabs are not
!!   part of the Fortran character set. Some editors and printers will
!!   have problems with tabs. It is often useful to expand tabs in input
!!   files to simplify further processing such as tokenizing an input line.
!!
!!### OPTIONS
!!     instr     Input line to remove tabs from
!!
!!### RESULTS
!!     outstr    Output string with tabs expanded. Assumed to be of sufficient
!!               length
!!     ilen      Significant length of returned string
!!
!!### EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_notabs
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use fpm_strings, only : notabs
!!    character(len=1024) :: in,out
!!    integer             :: ios,iout
!!       do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit
!!          call notabs(in,out,iout)
!!          write(*,'(a)')out(:iout)
!!       enddo
!!    end program demo_notabs
!!
!!### SEE ALSO
!!   GNU/Unix commands expand(1) and unexpand(1)
!!
elemental impure subroutine notabs(instr,outstr,ilen)

! ident_31="@(#)fpm_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"

character(len=*),intent(in)   :: instr        ! input line to scan for tab characters
character(len=*),intent(out)  :: outstr       ! tab-expanded version of INSTR produced
integer,intent(out)           :: ilen         ! column position of last character put into output string
                                              ! that is, ILEN holds the position of the last non-blank character in OUTSTR

integer,parameter             :: tabsize=8    ! assume a tab stop is set every 8th column
integer                       :: ipos         ! position in OUTSTR to put next character of INSTR
integer                       :: lenin        ! length of input string trimmed of trailing spaces
integer                       :: lenout       ! number of characters output string can hold
integer                       :: istep        ! counter that advances thru input string INSTR one character at a time
character(len=1)              :: c            ! character in input line being processed
integer                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested

   ipos=1                                     ! where to put next character in output string OUTSTR
   lenin=len_trim(instr( 1:len(instr) ))      ! length of INSTR trimmed of trailing spaces
   lenout=len(outstr)                         ! number of characters output string OUTSTR can hold
   outstr=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters

      SCAN_LINE: do istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ichar(c)                        ! get ADE of the character
         EXPAND_TABS : select case (iade)     ! take different actions depending on which character was found
         case(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (mod(ipos-1,tabsize)))
         case(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         case default                         ! c is anything else other than a tab,newline,or return  insert it in output string
            if(ipos > lenout)then
               write(stderr,*)"*notabs* output string overflow"
               exit
            else
               outstr(ipos:ipos)=c
               ipos=ipos+1
            endif
         end select EXPAND_TABS
      enddo SCAN_LINE

      ipos=min(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=len_trim(outstr(:ipos))            ! trim trailing spaces

end subroutine notabs

!>AUTHOR: John S. Urban
!!LICENSE: Public Domain
!>
!!##NAME
!!    dilate(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dilate(INSTR) result(OUTSTR)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=:),allocatable  :: OUTSTR
!!
!!##DESCRIPTION
!!     dilate() converts tabs in INSTR to spaces in OUTSTR.  It assumes a
!!     tab is set every 8 characters. Trailing spaces are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dilate
!!
!!    use M_strings, only : dilate
!!    implicit none
!!    character(len=:),allocatable :: in
!!    integer                      :: i
!!       in='  this is my string  '
!!       ! change spaces to tabs to make a sample input
!!       do i=1,len(in)
!!          if(in(i:i) == ' ')in(i:i)=char(9)
!!       enddo
!!       write(*,'(a)')in,dilate(in)
!!    end program demo_dilate
!!
function dilate(instr) result(outstr)

   character(len=*), intent(in)  :: instr        ! input line to scan for tab characters
   character(len=:), allocatable :: outstr       ! tab-expanded version of INSTR produced
   integer                       :: i
   integer                       :: icount
   integer                       :: lgth
   icount = 0
   do i = 1, len(instr)
      if (instr(i:i) == char(9)) icount = icount + 1
   end do
   allocate (character(len=(len(instr) + 8*icount)) :: outstr)
   call notabs(instr, outstr, lgth)
   outstr = outstr(:lgth)

end function dilate

!> Check if feature name is valid (alphanumeric, underscore, dash allowed)
logical function is_valid_feature_name(name) result(valid)
    character(len=*), intent(in) :: name
    integer :: i
    character :: c
    
    valid = .false.
    
    ! Must not be empty and not too long
    if (len_trim(name) == 0 .or. len_trim(name) > 64) return
    
    ! First character must be alphabetic or underscore
    c = name(1:1)
    if (.not. ((c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. c == '_')) return
    
    ! Remaining characters can be alphanumeric, underscore, or dash
    do i = 2, len_trim(name)
        c = name(i:i)
        if (.not. ((c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. &
                  (c >= '0' .and. c <= '9') .or. c == '_' .or. c == '-')) then
            return
        end if
    end do
    
    valid = .true.
    
end function is_valid_feature_name

!> Add one element to a string array with a loop (gcc-15 bug on array initializer)
pure subroutine add_strings_one(list,new)
    type(string_t), allocatable, intent(inout) :: list(:)
    type(string_t), intent(in) :: new

    integer :: i,n
    type(string_t), allocatable :: tmp(:)

    if (allocated(list)) then 
       n = size(list)
    else
       n = 0
    endif     

    allocate(tmp(n+1))
    do i=1,n
       tmp(i) = list(i)
    end do   
    tmp(n+1) = new
    call move_alloc(from=tmp,to=list)

end subroutine add_strings_one       

!> Add elements to a string array with a loop (gcc-15 bug on array initializer)
pure subroutine add_strings_many(list,new)
    type(string_t), allocatable, intent(inout) :: list(:)
    type(string_t), intent(in) :: new(:)

    integer :: i,n,add
    type(string_t), allocatable :: tmp(:)

    if (allocated(list)) then 
       n = size(list)
    else
       n = 0
    endif     

    add = size(new)
    if (add<=0) return

    allocate(tmp(n+add))
    do i=1,n
       tmp(i) = list(i)
    end do   
    do i=1,add
       tmp(n+i) = new(i)
    end do   
    call move_alloc(from=tmp,to=list)

end subroutine add_strings_many  

end module fpm_strings
