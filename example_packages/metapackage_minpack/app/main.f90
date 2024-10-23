program main
  use metapackage_minpack, only: simple_test
  implicit none
  logical :: success
  call simple_test(success)
  stop merge(0,1,success) 
end program main
