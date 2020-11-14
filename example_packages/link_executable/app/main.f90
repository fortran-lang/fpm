program gomp_example
  implicit none

  interface
    integer function omp_get_num_procs()
    end function
  end interface

  print *, omp_get_num_procs()

end program gomp_example
