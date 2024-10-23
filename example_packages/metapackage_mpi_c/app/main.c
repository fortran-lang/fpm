// Test MPI linking from a C main program
#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{

  int ierror,ncpus,cpuid;

  // Initialize MPI argument	
  ierror = MPI_Init(&argc, &argv);
  if (ierror) {
     printf("MPI_Init failed with error %d \n",ierror);
     return 1;
  } 

  // Get number of processes and current rank
  MPI_Comm_size(MPI_COMM_WORLD, &ncpus);

  //  Get Rank of the current process
  MPI_Comm_rank(MPI_COMM_WORLD, &cpuid);

  printf("Hello, MPI C World from rank %d of %d! \n",cpuid+1,ncpus);

  // Finalize MPI environment.
  ierror = MPI_Finalize();
  if (ierror) {
     printf("MPI_Finalize failed with error %d \n",ierror);
     return 1;
  } else {
     return 0;
  }
}

