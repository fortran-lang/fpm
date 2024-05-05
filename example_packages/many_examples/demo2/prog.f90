program demo
  integer :: i
  open(newunit=i,file="demo2.txt",form="formatted",action="write")
  write(i, '(a)') "DEMO2"
  close(i)
  stop 0
end program demo
