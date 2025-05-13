! fortran-lang stdlib + external BLAS test case
! Program will fail if an external BLAS has not been linked against.
program test_stdlib_metapackage
    use stdlib_linalg_constants, only: external_blas_ilp32,external_lapack_ilp32, &
                                       external_blas_ilp64,external_lapack_ilp64
    implicit none

    if (.not.(external_blas_ilp32 .or. external_blas_ilp64)) then 
        stop 1
    elseif (.not.(external_lapack_ilp32 .or. external_lapack_ilp64)) then 
        stop 2
    else
        stop 0
    end if

end program test_stdlib_metapackage
