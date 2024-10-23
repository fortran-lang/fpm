// Test MPI linking from a C main program
#include <iostream>
#include <mpi.h>

using namespace std;

int main(int argc, char** argv)
{

  int ierror,ncpus,cpuid;

  // Initialize MPI argument	
  ierror = MPI_Init(&argc, &argv);
  if (ierror) {
     cout << "MPI_Init failed with error " << ierror << endl; 
     return 1;
  } 

  // Get number of processes and current rank
  MPI_Comm_size(MPI_COMM_WORLD, &ncpus);

  //  Get Rank of the current process
  MPI_Comm_rank(MPI_COMM_WORLD, &cpuid);

  cout << "Hello, MPI C++ World from rank " << cpuid << " of " << ncpus << "!" << endl;

  // Finalize MPI environment.
  ierror = MPI_Finalize();
  if (ierror) {
     cout << "MPI_Finalize failed with error " << ierror << endl; 
     return 1;
  } else {
     return 0;
  }
}

