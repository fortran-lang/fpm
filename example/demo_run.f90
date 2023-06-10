program demo_run
use fpm_filesystem, only: run
implicit none
integer :: exitstat
character(len=:), allocatable :: cmd
logical, parameter ::  T = .true., F = .false.
cmd = 'ls -ltrasd *.md'

   call run(cmd)
   call paws('default options (ie. echo=T verbose=T)')

   call run(cmd, exitstat=exitstat)
   write (*, *) 'exitstat=', exitstat
   call paws('exitstat')

   call run(cmd, echo=F)
   call paws('echo=F')

   call run(cmd, verbose=F)
   call paws('verbose=F')

   call run(cmd, verbose=F, echo=F)
   call paws('verbose=F echo=F')

   call run(cmd, redirect='_scratch')
   call paws('redirect="_scratch"')

   call run(cmd, redirect='_scratch', verbose=F)
   call paws('redirect="_scratch" verbose=F')

   call run(cmd, redirect='_scratch', verbose=T)
   call paws('redirect="_scratch" verbose=T')

contains

subroutine paws(str)
character(len=*), intent(in) :: str
character(len=1) :: chr
integer :: iostat

   write (*, '(a,": ")', advance='no') str
   read (*, '(a)', iostat=iostat) chr
   write (*, '(a)') repeat('-', 60)

end subroutine paws

end program demo_run
