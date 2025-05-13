! fortran-lang stdlib test case
! This test program will only run if stdlib is properly built and linked to this project. 
program test_stdlib_metapackage

    ! These USEs would not be possible if stdlib is not found 
    use metapackage_stdlib, only: external_blas_test,external_lapack_test
    implicit none

    logical :: ext_blas,ext_lapack

    call external_blas_test(ext_blas)
    call external_lapack_test(ext_lapack)
    
    if (.not.ext_blas) then 
        stop 1
    elseif (.not.ext_lapack) then 
        stop 2
    else
        stop 0
    end if

end program test_stdlib_metapackage
