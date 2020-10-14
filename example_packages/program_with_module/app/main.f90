module greet_m
  implicit none
  character(*), parameter :: greeting = 'Hello, fpm!' 
end module greet_m

program program_with_module
  use greet_m, only: greeting
  implicit none
  print *, greeting
end program program_with_module
