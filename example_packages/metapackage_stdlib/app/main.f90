! fortran-lang stdlib test case
! This test program will only run if stdlib is properly built and linked to this project. 
program test_stdlib_metapackage

    ! These USEs would not be possible if stdlib is not found 
    use stdlib_kinds, only: int32, int64, dp, sp
    use stdlib_math
    implicit none

    real(dp), allocatable :: indices(:)

    indices = linspace(1.0_dp,5.0_dp,5)

    if (.not.allocated(indices)) then 
       stop 1
    elseif (size(indices)/=5) then 
       stop 2
    elseif (any(nint(indices)/=[1,2,3,4,5])) then 
       stop 3
    else
       stop 0
    endif

end program test_stdlib_metapackage
