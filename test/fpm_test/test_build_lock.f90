module test_build_lock
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_build_lock, only: build_lock_t
    use fpm_filesystem, only: exists, join_path, get_temp_filename, os_delete_dir
    use fpm_environment, only: os_is_unix
    implicit none
    private

    public :: collect_build_lock

contains

    !> Collect all exported unit tests
    subroutine collect_build_lock(tests)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest("acquire-release", test_acquire_release) &
            ]

    end subroutine collect_build_lock

    !> A lock creates the lock file, does not wait for itself when acquired again
    !> while held, and can be acquired again after it is released
    subroutine test_acquire_release(error)
        type(error_t), allocatable, intent(out) :: error

        type(build_lock_t) :: lock
        character(len=:), allocatable :: dir

        dir = get_temp_filename()

        if (lock%held()) then
            call test_failed(error, "A new lock is held")
            return
        end if

        call lock%acquire(dir)
        if (.not. lock%held()) then
            call test_failed(error, "The lock is not held after acquire")
            return
        end if
        if (.not. exists(join_path(dir, '.fpm-lock'))) then
            call test_failed(error, "acquire did not create "//join_path(dir, '.fpm-lock'))
            return
        end if

        call lock%acquire(dir)
        call lock%release()
        if (lock%held()) then
            call test_failed(error, "The lock is held after release")
            return
        end if

        call lock%acquire(dir)
        if (.not. lock%held()) then
            call test_failed(error, "The lock is not held after acquire following release")
            return
        end if
        call lock%release()

        call os_delete_dir(os_is_unix(), dir)

    end subroutine test_acquire_release

end module test_build_lock
