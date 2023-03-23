! OpenMP test case
! This test program will only run if openmp is properly enabled in the compiler flags. 
! Otherwise, the omp_lib module won't be found and the code cannot be built.
program openmp_test
  use test_openmp, only: say_hello
  use omp_lib
  implicit none

!$omp parallel
    call say_hello(thread_ID=OMP_GET_THREAD_NUM())
!$omp end parallel

! Successful return
stop 0

end program openmp_test
