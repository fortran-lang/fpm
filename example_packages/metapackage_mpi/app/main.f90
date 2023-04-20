program with_mpi
 
  include 'mpif.h'

  integer, parameter :: INIT_ERROR = 1
  integer, parameter :: RANK_ERROR = 2

  integer :: ierror,ncpus,cpuid

  ! Initialize MPI argument
  call MPI_INIT(ierror);  
  if (ierror/=0) stop INIT_ERROR

  ! Get number of processes and current rank 
  call MPI_Comm_size(MPI_COMM_WORLD, ncpus, ierror)
  if (ierror/=0) stop RANK_ERROR

  call MPI_Comm_rank(MPI_COMM_WORLD, cpuid, ierror) 
  if (ierror/=0) stop RANK_ERROR

  print "('Hello, mpi world from rank ',i0,' of ',i0,'!')", cpuid+1,ncpus

  ! Finalize MPI environment.
  call MPI_FINALIZE(ierror)
  if (ierror/=0) stop INIT_ERROR
  
  stop 0

end program with_mpi 

