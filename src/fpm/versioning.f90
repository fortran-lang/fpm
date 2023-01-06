!> Implementation of versioning data for comparing packages
module fpm_versioning
  use fpm_error, only: error_t, syntax_error
  implicit none
  private

  public :: version_t, new_version, char

  type :: version_t
    private

    !> Version numbers found
    integer, allocatable :: num(:)

  contains

    generic :: operator(==) => equals
    procedure, private :: equals

    generic :: operator(/=) => not_equals
    procedure, private :: not_equals

    generic :: operator(>) => greater
    procedure, private :: greater

    generic :: operator(<) => less
    procedure, private :: less

    generic :: operator(>=) => greater_equals
    procedure, private :: greater_equals

    generic :: operator(<=) => less_equals
    procedure, private :: less_equals

    !> Compare a version against a version constraint (x.x.0 <= v < x.x.HUGE)
    generic :: operator(.match.) => match
    procedure, private :: match

    !> Create a printable string from a version data type
    procedure :: to_string

  end type version_t

  !> Arbitrary internal limit of the version parser
  integer, parameter :: max_limit = 3

  interface char
    module procedure :: as_string
  end interface char

  interface new_version
    module procedure :: new_version_from_string
    module procedure :: new_version_from_int
  end interface new_version

contains

  !> Create a new version from a string
  subroutine new_version_from_int(self, num)

    !> Instance of the versioning data
    type(version_t), intent(out) :: self

    !> Subversion numbers to define version data
    integer, intent(in) :: num(:)

    self%num = num

  end subroutine new_version_from_int

  !> Create a new version from a string
  subroutine new_version_from_string(self, string, error)

    !> Instance of the versioning data
    type(version_t), intent(out) :: self

    !> String describing the version information
    character(len=*), intent(in) :: string

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: istart, iend, stat, nn
    integer :: num(max_limit)
    logical :: is_number

    nn = 0
    iend = 0
    istart = 0
    is_number = .false.

    do while (iend < len(string))
      call next(string, istart, iend, is_number, error)
      if (allocated(error)) exit
      if (is_number) then
        if (nn >= max_limit) then
          call token_error(error, string, istart, iend, &
              & "Too many subversions found")
          exit
        end if
        nn = nn + 1
        read (string(istart:iend), *, iostat=stat) num(nn)
        if (stat /= 0) then
          call token_error(error, string, istart, iend, &
              & "Failed to parse version number")
          exit
        end if
      end if
    end do
    if (allocated(error)) return
    if (.not. is_number) then
      call token_error(error, string, istart, iend, &
          & "Expected version number, but no characters are left")
      return
    end if

    call new_version(self, num(:nn))

  end subroutine new_version_from_string

  !> Tokenize a version string
  subroutine next(string, istart, iend, is_number, error)

    !> String describing the version information
    character(len=*), intent(in) :: string

    !> Start of last token, start of next token on exit
    integer, intent(inout) :: istart

    !> End of last token on entry, end of next token on exit
    integer, intent(inout) :: iend

    !> Token produced is a number
    logical, intent(inout) :: is_number

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii, nn
    logical :: was_number
    character :: tok

    was_number = is_number
    nn = len(string)

    if (iend >= nn) then
      istart = nn
      iend = nn
      return
    end if

    ii = min(iend + 1, nn)
    tok = string(ii:ii)

    is_number = tok /= '.'
    if (is_number .eqv. was_number) then
      call token_error(error, string, istart, ii, &
          & "Unexpected token found")
      return
    end if

    if (.not. is_number) then
      is_number = .false.
      istart = ii
      iend = ii
      return
    end if

    istart = ii
    do ii = min(iend + 1, nn), nn
      tok = string(ii:ii)
      select case (tok)
      case default
        call token_error(error, string, istart, ii, &
            & "Invalid character in version number")
        exit
      case ('.')
        exit
      case ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
        iend = ii
        cycle
      end select
    end do

  end subroutine next

  !> Create an error on an invalid token, provide some visual context as well
  subroutine token_error(error, string, istart, iend, message)

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    !> String describing the version information
    character(len=*), intent(in) :: string

    !> Start of last token, start of next token on exit
    integer, intent(in) :: istart

    !> End of last token on entry, end of next token on exit
    integer, intent(in) :: iend

    !> Error message
    character(len=*), intent(in) :: message

    character(len=*), parameter :: nl = new_line('a')

    allocate (error)
    error%message = message//nl//"  | "//string//nl// &
        & "  |"//repeat('-', istart)//repeat('^', iend - istart + 1)

  end subroutine token_error

  subroutine to_string(self, string)

    !> Version number
    class(version_t), intent(in) :: self

    !> Character representation of the version
    character(len=:), allocatable, intent(out) :: string

    integer, parameter :: buffersize = 64
    character(len=buffersize) :: buffer
    integer :: ii

    do ii = 1, size(self%num)
      if (allocated(string)) then
        write (buffer, '(".", i0)') self%num(ii)
        string = string//trim(buffer)
      else
        write (buffer, '(i0)') self%num(ii)
        string = trim(buffer)
      end if
    end do

    if (.not. allocated(string)) then
      string = '0'
    end if

  end subroutine to_string

  function as_string(self) result(string)

    !> Version number
    class(version_t), intent(in) :: self

    !> Character representation of the version
    character(len=:), allocatable :: string

    call self%to_string(string)

  end function as_string

  !> Check to version numbers for equality
  elemental function equals(lhs, rhs) result(is_equal)

    !> First version number
    class(version_t), intent(in) :: lhs

    !> Second version number
    class(version_t), intent(in) :: rhs

    !> Version match
    logical :: is_equal

    is_equal = .not. (lhs > rhs)
    if (is_equal) then
      is_equal = .not. (rhs > lhs)
    end if

  end function equals

  !> Check two versions for inequality
  elemental function not_equals(lhs, rhs) result(not_equal)

    !> First version number
    class(version_t), intent(in) :: lhs

    !> Second version number
    class(version_t), intent(in) :: rhs

    !> Version mismatch
    logical :: not_equal

    not_equal = lhs > rhs
    if (.not. not_equal) then
      not_equal = rhs > lhs
    end if

  end function not_equals

  !> Relative comparison of two versions
  elemental function greater(lhs, rhs) result(is_greater)

    !> First version number
    class(version_t), intent(in) :: lhs

    !> Second version number
    class(version_t), intent(in) :: rhs

    !> First version is greater
    logical :: is_greater

    integer :: ii

    do ii = 1, min(size(lhs%num), size(rhs%num))
      is_greater = lhs%num(ii) > rhs%num(ii)
      if (is_greater) exit
    end do
    if (is_greater) return

    is_greater = size(lhs%num) > size(rhs%num)
    if (is_greater) then
      do ii = size(rhs%num) + 1, size(lhs%num)
        is_greater = lhs%num(ii) > 0
        if (is_greater) exit
      end do
    end if

  end function greater

  !> Relative comparison of two versions
  elemental function less(lhs, rhs) result(is_less)

    !> First version number
    class(version_t), intent(in) :: lhs

    !> Second version number
    class(version_t), intent(in) :: rhs

    !> First version is less
    logical :: is_less

    is_less = rhs > lhs

  end function less

  !> Relative comparison of two versions
  elemental function greater_equals(lhs, rhs) result(is_greater_equal)

    !> First version number
    class(version_t), intent(in) :: lhs

    !> Second version number
    class(version_t), intent(in) :: rhs

    !> First version is greater or equal
    logical :: is_greater_equal

    is_greater_equal = .not. (rhs > lhs)

  end function greater_equals

  !> Relative comparison of two versions
  elemental function less_equals(lhs, rhs) result(is_less_equal)

    !> First version number
    class(version_t), intent(in) :: lhs

    !> Second version number
    class(version_t), intent(in) :: rhs

    !> First version is less or equal
    logical :: is_less_equal

    is_less_equal = .not. (lhs > rhs)

  end function less_equals

  !> Try to match first version against second version
  elemental function match(lhs, rhs)

    !> First version number
    class(version_t), intent(in) :: lhs

    !> Second version number
    class(version_t), intent(in) :: rhs

    !> Version match following semantic versioning rules
    logical :: match

    type(version_t) :: tmp

    match = .not. (rhs > lhs)
    if (match) then
      tmp%num = rhs%num
      tmp%num(size(tmp%num)) = tmp%num(size(tmp%num)) + 1
      match = tmp > lhs
    end if

  end function match

end module fpm_versioning
