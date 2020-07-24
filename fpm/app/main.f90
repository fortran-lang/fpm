program main
use fpm, only: print_help, cmd_build, cmd_install, cmd_new, cmd_run, cmd_test
implicit none
character(100) :: cmdarg

if (command_argument_count() == 0) then
    call print_help()
else if (command_argument_count() == 1) then
    call get_command_argument(1, cmdarg)
    select case(trim(cmdarg))
        case("build")
            call cmd_build()
        case("install")
            call cmd_install()
        case("new")
            call cmd_new()
        case("run")
            call cmd_run()
        case default
            print *, "fpm error: No such command " // trim(cmdarg)
            error stop 1
    end select
else
    print *, "Too many arguments"
    error stop 1
end if
end program main
