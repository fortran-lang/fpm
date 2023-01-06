!> Performs one of the matrix-vector operations
!>
!>    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
!>
!> where alpha and beta are scalars, x and y are vectors and A is an
!> m by n matrix.
module wrapped_gemv
  implicit none
  private

  public :: sp, dp, gemv

  integer, parameter :: sp = selected_real_kind(6)
  integer, parameter :: dp = selected_real_kind(15)

  interface gemv
    module procedure :: wrap_sgemv
    module procedure :: wrap_dgemv
  end interface gemv

  interface blas_gemv
    subroutine sgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
      import :: sp
      real(sp), intent(in) :: a(lda, *)
      real(sp), intent(in) :: x(*)
      real(sp), intent(inout) :: y(*)
      real(sp), intent(in) :: alpha
      real(sp), intent(in) :: beta
      character(len=1), intent(in) :: trans
      integer, intent(in) :: incx
      integer, intent(in) :: incy
      integer, intent(in) :: m
      integer, intent(in) :: n
      integer, intent(in) :: lda
    end subroutine sgemv
    subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
      import :: dp
      real(dp), intent(in) :: a(lda, *)
      real(dp), intent(in) :: x(*)
      real(dp), intent(inout) :: y(*)
      real(dp), intent(in) :: alpha
      real(dp), intent(in) :: beta
      character(len=1), intent(in) :: trans
      integer, intent(in) :: incx
      integer, intent(in) :: incy
      integer, intent(in) :: m
      integer, intent(in) :: n
      integer, intent(in) :: lda
    end subroutine dgemv
  end interface blas_gemv

contains

  subroutine wrap_sgemv(amat, xvec, yvec, alpha, beta, trans)
    real(sp), intent(in) :: amat(:, :)
    real(sp), intent(in) :: xvec(:)
    real(sp), intent(inout) :: yvec(:)
    real(sp), intent(in), optional :: alpha
    real(sp), intent(in), optional :: beta
    character(len=1), intent(in), optional :: trans
    real(sp) :: a, b
    character(len=1) :: tra
    integer :: incx, incy, m, n, lda
    if (present(alpha)) then
      a = alpha
    else
      a = 1.0_sp
    end if
    if (present(beta)) then
      b = beta
    else
      b = 0
    end if
    if (present(trans)) then
      tra = trans
    else
      tra = 'n'
    end if
    incx = 1
    incy = 1
    lda = max(1, size(amat, 1))
    m = size(amat, 1)
    n = size(amat, 2)
    call blas_gemv(tra, m, n, a, amat, lda, xvec, incx, b, yvec, incy)
  end subroutine wrap_sgemv

  subroutine wrap_dgemv(amat, xvec, yvec, alpha, beta, trans)
    real(dp), intent(in) :: amat(:, :)
    real(dp), intent(in) :: xvec(:)
    real(dp), intent(inout) :: yvec(:)
    real(dp), intent(in), optional :: alpha
    real(dp), intent(in), optional :: beta
    character(len=1), intent(in), optional :: trans
    real(dp) :: a, b
    character(len=1) :: tra
    integer :: incx, incy, m, n, lda
    if (present(alpha)) then
      a = alpha
    else
      a = 1.0_dp
    end if
    if (present(beta)) then
      b = beta
    else
      b = 0
    end if
    if (present(trans)) then
      tra = trans
    else
      tra = 'n'
    end if
    incx = 1
    incy = 1
    lda = max(1, size(amat, 1))
    m = size(amat, 1)
    n = size(amat, 2)
    call blas_gemv(tra, m, n, a, amat, lda, xvec, incx, b, yvec, incy)
  end subroutine wrap_dgemv

end module wrapped_gemv
