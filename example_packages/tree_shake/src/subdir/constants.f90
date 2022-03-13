! This module is used indirectly by the executables
!  and hence should not be dropped during tree-shaking/pruning
module subdir_constants
implicit none

character(*), parameter :: GREET_STR = 'Hello, '
character(*), parameter :: FAREWELL_STR = 'Goodbye, '

end module subdir_constants
