!># Module name mangling
!>
!> This module contains a public class for handling module names which may have conflicts
!> across packages.
!>
module fpm_module
use fpm_strings
implicit none
private

public :: module_t
public :: assignment(=)
public :: operator(==),operator(.in.)
public :: string_t

!> Temporary: flag to decide whether namespaces should be used
logical,      parameter :: USE_NAMESPACES    = .true.
character(*), parameter :: SEPARATOR         = "_FPM_"
character(*), parameter :: DEFAULT_NAMESPACE = "default"


!> Type for describing a Fortran module
type, extends(string_t) :: module_t

    type(string_t) :: namespace

end type module_t

interface operator(==)
    module procedure module_equals
end interface

interface assignment(=)
    module procedure module_assign_string
    module procedure module_assign_char
end interface

interface operator(.in.)
    module procedure array_contains_module
    module procedure array_contains_string
end interface

interface string_t
    module procedure namespaced_module_string
end interface

interface module_t
    module procedure new_from_string
    module procedure new_from_char
end interface

contains

!> Check if a module has a special namespace (not the default one)
elemental logical function has_namespace(this)
    type(module_t), intent(in) :: this

    if (len(this%namespace%s)>0) then
        has_namespace = this%namespace%s/=DEFAULT_NAMESPACE
    else
        has_namespace = .false.
    end if

end function has_namespace

!> Return a unique module name string
elemental type(string_t) function namespaced_module_string(this)
    type(module_t), intent(in) :: this

    integer :: ls

    ! Temporary: return same string
    if (allocated(this%s)) then

        if (USE_NAMESPACES .and. has_namespace(this)) then

           ls = len(this%s)+len(this%namespace%s)+len(SEPARATOR)
           allocate(character(len=ls) :: namespaced_module_string%s)
           namespaced_module_string%s(1:ls) = this%namespace%s//SEPARATOR//this%s

        else

           ls = len(this%s)
           allocate(character(len=ls) :: namespaced_module_string%s)
           if (ls>0) namespaced_module_string%s(1:ls) = this%s(1:ls)

        endif

    else
        if (allocated(namespaced_module_string%s)) deallocate(namespaced_module_string%s)
    endif

end function

!> Compare two namespaced modules
elemental logical function module_equals(this,that)
    type(module_t), intent(in) :: this,that

    module_equals =       this%s == that%s &
                    .and. this%namespace%s == that%namespace%s

end function module_equals

!> Check if array of type(module_t) matches a particular instance
!!
pure logical function array_contains_module(search_module,array)
    type(module_t), intent(in) :: search_module
    type(module_t), intent(in) :: array(:)

    integer :: i

    array_contains_module = any([(module_equals(array(i),search_module), &
                          i=1,size(array))])

end function array_contains_module

!> Check if array of type(module_t) matches a particular instance
!!
pure logical function array_contains_string(search_module,array)
    character(*), intent(in) :: search_module
    type(module_t), intent(in) :: array(:)

    type(module_t) :: search_module_t

    search_module_t = module_t(search_module)
    array_contains_string = array_contains_module(search_module_t,array)
end function array_contains_string

!> Initialize a module from a string
pure subroutine module_assign_char(this,s)
    type(module_t), intent(out) :: this
    character(*), intent(in) :: s

    this = new_from_char(s)

end subroutine module_assign_char

pure subroutine module_assign_string(this,s)
    type(module_t), intent(out) :: this
    type(string_t), intent(in) :: s
    this = new_from_char(s%s)
end subroutine module_assign_string

!> Return a module type from a namespaced string
pure type(module_t) function new_from_char(s) result(this)
    character(*), intent(in) :: s

    integer :: sep

    !> Parse optional namespace
    sep = index(s,SEPARATOR)

    if (sep>1) then
        this%namespace%s = s(:sep-1)
        this%s = s(sep+len(SEPARATOR):)
    else
        this%namespace%s = DEFAULT_NAMESPACE
        this%s = s
    end if

end function new_from_char

pure type(module_t) function new_from_string(s) result(this)
    type(string_t), intent(in) :: s
    this = s
end function new_from_string

end module fpm_module
