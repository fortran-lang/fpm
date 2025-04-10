module test_lock

    use testsuite, only : new_unittest, unittest_t, test_failed
    use fpm_error, only : error_t, fatal_error
    use fpm_filesystem, only : run, exists
    use fpm_lock, only : fpm_lock_acquire, fpm_lock_acquire_noblock, &
                         fpm_lock_release

    implicit none
    private
    public :: collect_lock

contains

    !> Collect unit tests.
    subroutine collect_lock(tests)

        !> Unit tests to collect.
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest('acquire-leaves-lockfile', acquire_leaves_lockfile), &
            & new_unittest('aquire-release-leaves-nothing', acquire_release_leaves_nothing), &
            & new_unittest('acquire-release-acquire-release', acquire_release_acquire_release), &
            & new_unittest('double-acquire', double_acquire), &
            & new_unittest('release', release, should_fail=.true.), &
            & new_unittest('acquire-release-release', acquire_release_release, should_fail=.true.), &
            & new_unittest('acquire-existing-lockfile-valid', acquire_existing_lockfile_valid), &
            & new_unittest('acquire-blocks', acquire_blocks), &
            & new_unittest('release-rouge-remove', release_rouge_remove) &

        ]
    end subroutine collect_lock

    !> Setup before each unittest
    subroutine setup()
        type(error_t), allocatable :: dummy_error
        call fpm_lock_release(dummy_error)
        call run ('rm -f .fpm-package-lock')
    end subroutine setup

    !> Cleanup after each unit test
    subroutine cleanup()
        type(error_t), allocatable :: dummy_error
        call fpm_lock_release(dummy_error)
        call run ('touch .fpm-package-lock')
    end subroutine cleanup

    !> Helper function to acquire a lock, and if that fails an error is raised.
    subroutine acquire_lock(error)
        type(error_t), allocatable, intent(out) :: error
        logical :: success

        call fpm_lock_acquire_noblock(error, success)
        if (allocated(error)) return
        if (.not. success) then
            call test_failed(error, "lock-file acquire failed")
        end if
    end subroutine acquire_lock

    !> A simple fpm_lock_acquire_noblock creates a lock-file.
    subroutine acquire_leaves_lockfile(error)
        type(error_t), allocatable, intent(out) :: error
        logical :: success

        call setup()

        call acquire_lock(error)
        if (allocated(error)) return
        if (.not. exists('.fpm-package-lock')) then
            call test_failed(error, "lock-file wasn't created")
        end if

        call cleanup()

    end subroutine acquire_leaves_lockfile

    !> fpm_lock_release removes the lock-file.
    subroutine acquire_release_leaves_nothing(error)
        type(error_t), allocatable, intent(out) :: error
        logical :: success

        call setup()

        call acquire_lock(error)
        if (allocated(error)) return

        call fpm_lock_release(error)
        if (allocated(error)) return
        if (exists('.fpm-package-lock')) then
            call test_failed(error, "lock-file wasn't removed")
        end if

        call cleanup()
    end subroutine acquire_release_leaves_nothing

    !> subsequent locks and releases work.
    subroutine acquire_release_acquire_release (error)
        type(error_t), allocatable, intent(out) :: error

        call setup()

        call acquire_lock(error)
        if (allocated(error)) return

        call fpm_lock_release(error)
        if (allocated(error)) return

        call acquire_lock(error)
        if (allocated(error)) return

        call fpm_lock_release(error)
        if (allocated(error)) return

        call cleanup()
    end subroutine acquire_release_acquire_release

    !> Double acquire should not work the second time.
    subroutine double_acquire(error)
        type(error_t), allocatable, intent(out) :: error
        logical :: success

        call setup()

        call fpm_lock_acquire_noblock(error)
        if (allocated(error)) return

        call fpm_lock_acquire_noblock(error, success)
        if (allocated(error)) return

        if (success) then
            call test_failed(error, "Expected lock to not succeed.")
        end if

        call cleanup()
    end subroutine double_acquire

    !> Release without acquire should cause an error.
    subroutine release(error)
        type(error_t), allocatable, intent(out) :: error

        call setup()

        call fpm_lock_release(error)

        call cleanup()
    end subroutine release

    !> One release to much should cause and error
    subroutine acquire_release_release(error)
        type(error_t), allocatable, intent(out) :: error

        call setup()

        call fpm_lock_acquire_noblock(error)
        call fpm_lock_release(error)
        call fpm_lock_release(error)
    end subroutine acquire_release_release

    !> If a lock-file already exists then we shoudln't acquire a lock.
    subroutine acquire_existing_lockfile_valid(error)
        type(error_t), allocatable, intent(out) :: error
        logical :: success

        call setup()

        ! Some other process acquires a lock.
        call run('touch .fpm-package-lock')

        ! We expect this to not succeed, (but no errors should be raised).
        call fpm_lock_acquire_noblock(error, success=success)
        if (allocated(error)) return
        if (success) then
            call test_failed(error, "Expected package lock to fail")
        end if

        call cleanup()
    end subroutine acquire_existing_lockfile_valid

    !> A blocking acquire should resume when the lock-file is deleted.
    subroutine acquire_blocks(error)
        type(error_t), allocatable, intent(out) :: error

        call setup()

        ! Some other process acquires a lock to work on the package briefly.
        call run('touch .fpm-package-lock && sleep 1 && rm .fpm-package-lock &')

        ! Our blocking acquire should wait for a bit and then go through
        call fpm_lock_acquire(error)
        if (allocated(error)) return

        call cleanup()
    end subroutine

    !> If some other process removes our lock-file then fpm_lock_release should
    !> give an error.
    subroutine release_rouge_remove(error)
        type(error_t), allocatable, intent(out) :: error
        type(error_t), allocatable :: dummy_error
        logical success

        call setup()

        call fpm_lock_acquire_noblock(error, success)
        if (allocated(error)) return
        if (.not. success) then
            call test_failed(error, "lock-file acquire failed")
        end if

        ! Some reouge process removes the lock-file
        call run('rm .fpm-package-lock')

        call fpm_lock_release(dummy_error)
        if (.not. allocated(dummy_error)) then
            call test_failed(error, &
                "Expected fpm_lock_release to fail, but it succeeded")
        end if

        call cleanup()
    end subroutine
end module test_lock
