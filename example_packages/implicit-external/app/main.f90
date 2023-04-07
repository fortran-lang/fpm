program test
  integer :: ijk
  call impl(ijk)
  if (ijk /= 1) error stop
end program test
