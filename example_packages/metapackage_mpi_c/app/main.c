// Test MPI linking from a C main program
#include <mpi.h>
#include <stdio.h>

int main(int argc, char** argv)
{

  int ierror,ncpus,cpuid;

  // Initialize MPI argument	
  MPI_Init(&argc, &argv);

  // Get number of processes and current rank
  MPI_Comm_size(MPI_COMM_WORLD, &ncpus);

  //  Get Rank of the current process
  MPI_Comm_rank(MPI_COMM_WORLD, &cpuid);

  printf("Hello, MPI C World from rank %d of %d! \n",cpuid+1,ncpus);

  // Finalize MPI environment.
  MPI_Finalize();
  return 0;
}

