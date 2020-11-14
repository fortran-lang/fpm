program test_blas
   use wrapped_gemv, only : sp, gemv
   implicit none

   integer :: i, j
   real(sp) :: mat(4, 4), vec(4), res(4)

   do i = 1, size(vec)
      vec(i) = sqrt(real(i, sp))
   end do

   do i = 1, size(mat, 2)
      do j = 1, size(mat, 1)
         mat(j, i) = sqrt(real(j * i, sp))
      end do
   end do

   call gemv(mat, vec, res, alpha=-1.0_sp, trans='t')

end program test_blas

