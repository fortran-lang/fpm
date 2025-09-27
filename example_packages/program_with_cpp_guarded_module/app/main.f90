program program_with_module
#if defined(HAVE_MODULE)
  use greet_m, only: greeting
#endif  
  implicit none

#ifndef HAVE_MODULE
  print *, 'OK without module'
#else  
  print *, greeting
#endif  
end program program_with_module
