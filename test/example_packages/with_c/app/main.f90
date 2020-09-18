program with_c_app
use with_c
implicit none

write(*,*) "isdir('app') = ", system_isdir('app')
write(*,*) "isdir('src') = ", system_isdir('src')
write(*,*) "isdir('test') = ", system_isdir('test')
write(*,*) "isdir('bench') = ", system_isdir('bench')

end program with_c_app