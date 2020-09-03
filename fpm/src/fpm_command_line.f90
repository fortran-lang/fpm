module fpm_command_line
    use fpm_environment, only: get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS

    implicit none
    
    private
    public :: fpm_cmd_settings, &
              fpm_build_settings, &
              fpm_install_settings, &
              fpm_new_settings, &
              fpm_run_settings, &
              fpm_test_settings, &
              get_command_line_settings        

    type, abstract :: fpm_cmd_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_new_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_build_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_run_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_test_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_install_settings
    end type

contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

        character(len=100) :: cmdarg

        if (command_argument_count() == 0) then
            call print_help()
        else if (command_argument_count() == 1) then
            call get_command_argument(1, cmdarg)
            select case(trim(cmdarg))
                case("new")
                    allocate(fpm_new_settings :: cmd_settings)
                case("build")
                    allocate(fpm_build_settings :: cmd_settings)
                case("run")
                    allocate(fpm_run_settings :: cmd_settings)
                case("test")
                    allocate(fpm_test_settings :: cmd_settings)
                case("install")
                    allocate(fpm_install_settings :: cmd_settings)
                case default
                    print *, "fpm error: No such command " // trim(cmdarg)
                    error stop 1
            end select
        else
            print *, "Too many arguments"
            error stop 1
        end if
    end subroutine

    subroutine print_help()
        print *, "fpm - A Fortran package manager and build system"
        select case (get_os_type())
            case (OS_LINUX)
                print *, "OS Type: Linux"
            case (OS_MACOS)
                print *, "OS Type: macOS"
            case (OS_WINDOWS)
                print *, "OS Type: Windows"
        end select
        print *
        print *, "Usage:"
        print *, "    fpm [COMMAND]"
        print *
        print *, "Valid fpm commands are:"
        print *, "    build    Compile the current package"
        print *, "    install  Install a Fortran binary or library (not implemented)"
        print *, "    new      Create a new Fortran package (not implemented)"
        print *, "    run      Run a binary of the local package (not implemented)"
        print *, "    test     Run the tests (not implemented)"
    end subroutine
end module fpm_command_line
