program check
  use cpp_files
  implicit none

  integer :: i, max_element
  integer, parameter :: array(*) = [(i,i=-50,10)]
  
  max_element = intvec_maxval(array,size(array,1))

  if (max_element == maxval(array)) then
    write(*,*) ' PASSED: Max element is ',max_element
  else
    write(*,*) ' (!) FAILED: Incorrect max element returned'
    stop 1
  end if

end program check
