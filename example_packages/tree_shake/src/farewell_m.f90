! This module is not used by any other sources
!  and only contains a module (no non-module subprograms),
!  therefore it should be dropped during tree-shaking/pruning
module farewell_m
    use subdir_constants, only: FAREWELL_STR
    implicit none
    private

    public :: make_farewell
contains
    function make_farewell(name) result(greeting)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: greeting

        greeting = FAREWELL_STR // name // "!"
    end function make_farewell
end module farewell_m
