program main
use fpm, only: print_help, cmd_build
implicit none
character(100) :: cmdarg

if (command_argument_count() == 0) then
    call print_help()
else if (command_argument_count() == 1) then
    call get_command_argument(1, cmdarg)
    if (cmdarg == "build") then
        call cmd_build()
    else
        print *, "Unknown command: ", cmdarg
        error stop
    end if
else
    print *, "Too many arguments"
    error stop
end if
end program main
