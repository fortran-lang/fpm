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

type, extends(string_t) :: module_t

    type(string_t) :: namespace

    contains

end type module_t

interface operator(==)
    module procedure module_equals
end interface

interface assignment(=)
    module procedure module_assign_string
    module procedure module_assign_char
end interface

interface operator(.in.)
    module procedure array_contains
end interface

interface string_t
    module procedure namespaced_module_string
end interface

contains

!> Return a unique module name string
elemental type(string_t) function namespaced_module_string(this)
    type(module_t), intent(in) :: this
    ! Temporary: return same string
    namespaced_module_string%s = this%s

end function

!> Compare two namespaced modules
elemental logical function module_equals(this,that)
    type(module_t), intent(in) :: this,that

    module_equals =       this%s == that%s &
                    .and. this%namespace%s == that%namespace%s

end function module_equals

!> Check if array of type(module_t) matches a particular instance
!!
pure logical function array_contains(search_module,array)
    type(module_t), intent(in) :: search_module
    type(module_t), intent(in) :: array(:)

    integer :: i

    array_contains = any([(module_equals(array(i),search_module), &
                          i=1,size(array))])

end function array_contains

subroutine module_assign_char(this,s)
    type(module_t), intent(out) :: this
    character(*), intent(in) :: s

    integer :: lt

    lt = len_trim(s)
    allocate(character(lt) :: this%s)
    if (lt>0) this%s(1:lt) = s(1:lt)

    this%namespace%s = ''
end subroutine module_assign_char

subroutine module_assign_string(this,s)
    type(module_t), intent(out) :: this
    type(string_t), intent(in) :: s
    this%s = s%s
    this%namespace%s = ''
end subroutine module_assign_string


end module fpm_module
