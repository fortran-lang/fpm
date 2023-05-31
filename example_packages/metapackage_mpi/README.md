# test_mpi
This test program prints the running thread ID using MPI.
PLEASE NOTE: 
- Test app uses 'mpif.h' and not 'use mpi' or 'use mpi_f08' because the latter are compiler-dependent, 
  and the MPI implementation on the local machine may not offer an implementation for them with the same 
  compiler that fpm is using.
- Using mpif.h will be the most backward compatible and platform agnostic
