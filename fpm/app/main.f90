program main
use fpm_command_line, only: &
        fpm_cmd_settings, &
        fpm_new_settings, &
        fpm_build_settings, &
        fpm_run_settings, &
        fpm_test_settings, &
        fpm_install_settings, &
        get_command_line_settings
use fpm, only: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test

implicit none

class(fpm_cmd_settings), allocatable :: cmd_settings

call get_command_line_settings(cmd_settings)

select type(cmd_settings)
type is (fpm_new_settings)
    call cmd_new()
type is (fpm_build_settings)
    call cmd_build()
type is (fpm_run_settings)
    call cmd_run()
type is (fpm_test_settings)
    call cmd_test()
type is (fpm_install_settings)
    call cmd_install()
end select

end program main
