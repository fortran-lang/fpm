module metapackage_stdlib
    use stdlib_linalg_constants, only: sp,dp,ilp
    implicit none
    private
    
    public :: external_blas_test
    public :: external_lapack_test

    contains
  
    !> Test availability of the external BLAS interface
    subroutine external_blas_test(external_blas)
        !> Error handling
        logical, intent(out) :: external_blas

#ifdef STDLIB_EXTERNAL_BLAS           
        interface 
            subroutine saxpy(n,sa,sx,incx,sy,incy)
                 import sp,ilp 
                 implicit none(type,external) 
                 real(sp), intent(in) :: sa,sx(*)
                 integer(ilp), intent(in) :: incx,incy,n
                 real(sp), intent(inout) :: sy(*)
            end subroutine saxpy        
        end interface
        
        integer(ilp), parameter :: n = 5, inc=1
        real(sp) :: a,x(n),y(n)
             
        x = 1.0_sp
        y = 2.0_sp
        a = 3.0_sp
        
        call saxpy(n,a,x,inc,y,inc)
        
        ! Result must also be correct
        external_blas = all(abs(y-5.0_sp)<sqrt(epsilon(0.0_sp)))
        
#else   
        external_blas = .false.
#endif        
        
    end subroutine external_blas_test        
                
    !> Test availability of the external BLAS interface
    subroutine external_lapack_test(external_lapack)
        !> Error handling
        logical, intent(out) :: external_lapack

#ifdef STDLIB_EXTERNAL_LAPACK       
        interface 
           subroutine dgetrf( m, n, a, lda, ipiv, info )
                import dp,ilp
                implicit none(type,external) 
                integer(ilp), intent(out) :: info,ipiv(*)
                integer(ilp), intent(in) :: lda,m,n
                real(dp), intent(inout) :: a(lda,*)
           end subroutine dgetrf       
        end interface
        
        integer(ilp), parameter :: n = 3
        real(dp) :: A(n,n)
        integer(ilp) :: ipiv(n),info


        A = reshape([1,0,0, 0,1,0, 0,0,1],[3,3])
        info = 123

        ! Factorize matrix 
        call dgetrf(n,n,A,n,ipiv,info)

        ! Result must be correct
        external_lapack = info==0
        
#else
        external_lapack = .false.
#endif        
        
    end subroutine external_lapack_test  
  
end module metapackage_stdlib
