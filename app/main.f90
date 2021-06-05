program main
use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
use fpm_command_line, only: &
        fpm_cmd_settings, &
        fpm_new_settings, &
        fpm_build_settings, &
        fpm_run_settings, &
        fpm_test_settings, &
        fpm_install_settings, &
        fpm_update_settings, &
        get_command_line_settings
use fpm_error, only: error_t
use fpm_filesystem, only: exists, parent_dir, join_path
use fpm, only: cmd_build, cmd_run
use fpm_cmd_install, only: cmd_install
use fpm_cmd_new, only: cmd_new
use fpm_cmd_update, only : cmd_update
use fpm_os,  only: change_directory, get_current_directory

implicit none

class(fpm_cmd_settings), allocatable :: cmd_settings
type(error_t), allocatable :: error
character(len=:), allocatable :: pwd_start, pwd_working, working_dir, project_root

call get_command_line_settings(cmd_settings)

call get_current_directory(pwd_start, error)
call handle_error(error)

call get_working_dir(cmd_settings, working_dir)
if (allocated(working_dir)) then
    ! Change working directory if requested
    if (len_trim(working_dir) > 0) then
        call change_directory(working_dir, error)
        call handle_error(error)

        call get_current_directory(pwd_working, error)
        call handle_error(error)
        write(output_unit, '(*(a))') "fpm: Entering directory '"//pwd_working//"'"
    else
        pwd_working = pwd_start
    end if
else
    pwd_working = pwd_start
end if

if (.not.has_manifest(pwd_working)) then
    project_root = pwd_working
    do while(.not.has_manifest(project_root))
        working_dir = parent_dir(project_root)
        if (len(working_dir) == 0) exit
        project_root = working_dir
    end do

    if (has_manifest(project_root)) then
        call change_directory(project_root, error)
        call handle_error(error)
        write(output_unit, '(*(a))') "fpm: Entering directory '"//project_root//"'"
    end if
end if

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

if (allocated(project_root)) then
    write(output_unit, '(*(a))') "fpm: Leaving directory '"//project_root//"'"
end if

if (pwd_start /= pwd_working) then
    write(output_unit, '(*(a))') "fpm: Leaving directory '"//pwd_working//"'"
end if

contains

    function has_manifest(dir)
        character(len=*), intent(in) :: dir
        logical :: has_manifest

        character(len=:), allocatable :: manifest

        has_manifest = exists(join_path(dir, "fpm.toml"))
    end function has_manifest

    subroutine handle_error(error)
        type(error_t), optional, intent(in) :: error
        if (present(error)) then
            write(error_unit, '("[Error]", 1x, a)') error%message
            stop 1
        end if
    end subroutine handle_error

    !> Save access to working directory in settings, in case setting have not been allocated
    subroutine get_working_dir(settings, working_dir)
        class(fpm_cmd_settings), optional, intent(in) :: settings
        character(len=:), allocatable, intent(out) :: working_dir
        if (present(settings)) then
            working_dir = settings%working_dir
        end if
    end subroutine get_working_dir

end program main
