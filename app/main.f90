program main
use fpm_command_line, only: &
        fpm_cmd_settings, &
        fpm_new_settings, &
        fpm_build_settings, &
        fpm_run_settings, &
        fpm_test_settings, &
        fpm_install_settings, &
        fpm_update_settings, &
        get_command_line_settings
use fpm, only: cmd_build, cmd_run
use fpm_cmd_install, only: cmd_install
use fpm_cmd_new, only: cmd_new
use fpm_cmd_update, only : cmd_update

implicit none

class(fpm_cmd_settings), allocatable :: cmd_settings

call get_command_line_settings(cmd_settings)

select type(settings=>cmd_settings)
type is (fpm_new_settings)
    call cmd_new(settings)
type is (fpm_build_settings)
    call cmd_build(settings)
type is (fpm_run_settings)
    call cmd_run(settings,test=.false.)
type is (fpm_test_settings)
    call cmd_run(settings,test=.true.)
type is (fpm_install_settings)
    call cmd_install(settings)
type is (fpm_update_settings)
    call cmd_update(settings)
end select

end program main
