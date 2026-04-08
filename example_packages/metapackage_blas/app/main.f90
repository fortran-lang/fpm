program metapackage_blas
    implicit none

    interface
    subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer, intent(in) :: n, nrhs, lda, ldb
      double precision, intent(in out) :: a(lda,*), b(ldb,*)
      integer, intent(out) :: ipiv(*), info
    end subroutine dgesv
    end interface

    integer, parameter :: dp = kind(1.0d0)
    real(dp), dimension(:,:), allocatable :: a
    real(dp), dimension(:),   allocatable :: b
    integer :: info

    allocate(a(3,3), b(3))
    a = reshape([1.0_dp, 2.0_dp, 3.0_dp, &
                 4.0_dp, 5.0_dp, 6.0_dp, &
                 7.0_dp, 8.0_dp, 9.0_dp], [3,3])
    b = [1.0_dp, 2.0_dp, 3.0_dp]

    call solve_eqsys(a, b, info)
    if (info /= 0) error stop

    stop 0

    contains

    !> simple wrapper for solvers for real system of linear
    !> equations  A * X = B
    subroutine solve_eqsys(a, b, info)

        real(dp), dimension(:,:), intent(inout) :: a
        real(dp), dimension(:),   intent(inout) :: b
        integer,             intent(out)   :: info
        integer :: i_alloc
        integer :: n, nrhs, lda, ldb
        integer, dimension(:), allocatable :: ipiv
        ! ------------------------------------------------------------------

        lda  = size(a,1)
        n    = size(a,2)
        ldb  = size(b,1)
        nrhs = 1

        allocate(ipiv(n),  stat = i_alloc)
        if (i_alloc /= 0) stop 'solve_eqsys: Allocation for array failed!'

        call dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)

        info = 0

        deallocate(ipiv,  stat = i_alloc)
        if (i_alloc /= 0) stop 'solve_eqsys: Deallocation for array failed!'

    end subroutine solve_eqsys
end program metapackage_blas
