program test_many_folders
use mod1
use mod2
use test1
use test2
implicit none

call say_hello()
call say_hello2()
print *, "All source modules found!"

call print_test1()
call print_test2()

stop 0

end program check
