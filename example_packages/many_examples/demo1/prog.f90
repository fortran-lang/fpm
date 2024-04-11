program demo
  integer :: i
  open(newunit=i,file="demo1.txt",form="formatted",action="write")
  write(i, '(a)') "DEMO1"
  close(i)
  stop 0
end program demo
