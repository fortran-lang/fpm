! This module is not used by any other sources, 
!  however because it also contains an external function
!  it cannot be dropped during tree-shaking/pruning 
module extra_m
    use subdir_constants, only: FAREWELL_STR
    implicit none
    private

    integer, parameter :: m = 0
end

function external_function() result(i)
    integer :: i
    i = 1
end function external_function