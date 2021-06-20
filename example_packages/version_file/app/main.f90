program stub
  implicit none
  logical :: exist
  integer :: unit
  character(len=100) :: line
  inquire(file="VERSION", exist=exist)
  if (.not.exist) error stop
  open(file="VERSION", newunit=unit)
  read(unit, '(a)') line
  close(unit)

  print '(*(a))', "File VERSION contains '", trim(line), "'"
end program stub
