module file_mod
   implicit none
   public
   contains
   subroutine print_file(name,id)
      character(*), intent(in) :: name
      integer, intent(in) :: id
      integer :: i
      character(len(name)+1) :: nm
      write(nm,1)name,id
      open(newunit=i,file=nm//'.txt',form="formatted",action="write")
      write(i, '(a)') nm
      close(i)
      1 format(a,i1)
   end subroutine print_file
end module file_mod
      
