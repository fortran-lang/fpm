module test_pkg_config
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_pkg_config, only: run_wrapper
    use fpm_strings, only: string_t
    use fpm_environment, only: os_is_unix
    implicit none
    private

    public :: collect_pkg_config

contains

    subroutine collect_pkg_config(tests)
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest("run-wrapper-nonzero-exit-is-failure", test_run_wrapper_nonzero_exit_is_failure) &
            & ]
    end subroutine collect_pkg_config

    subroutine test_run_wrapper_nonzero_exit_is_failure(error)
        type(error_t), allocatable, intent(out) :: error

        logical :: success
        integer :: exitcode
        type(string_t) :: output

        if (os_is_unix()) then
            call run_wrapper(wrapper=string_t("sh"), &
                             args=[string_t("-c"), string_t("exit 7")], &
                             exitcode=exitcode, cmd_success=success, screen_output=output)
        else
            call run_wrapper(wrapper=string_t("cmd"), &
                             args=[string_t("/c"), string_t("exit 7")], &
                             exitcode=exitcode, cmd_success=success, screen_output=output)
        end if

        if (success) then
            call test_failed(error, "run_wrapper marked a non-zero exit command as successful")
            return
        end if

        if (exitcode == 0) then
            call test_failed(error, "test command unexpectedly returned zero exit status")
            return
        end if

    end subroutine test_run_wrapper_nonzero_exit_is_failure

end module test_pkg_config
