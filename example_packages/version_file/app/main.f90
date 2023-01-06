program stub
  implicit none
  logical :: exists
  integer :: unit
  character(len=100) :: line
  inquire (file="VERSION", exist=exists)
  if (.not. exists) error stop "File VERSION does not exist."
  open (file="VERSION", newunit=unit)
  read (unit, '(a)') line
  close (unit)

  print '(*(a))', "File VERSION contains '", trim(line), "'"
end program stub
