module test_git
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: join_path, mkdir, is_dir, exists, os_delete_dir, &
                              command_not_found, warnwrite
    use fpm_environment, only: os_is_unix
    use fpm_git, only: git_target_t, git_target_default, git_checkout_is_valid
    implicit none
    private

    public :: collect_git

    character(len=*), parameter :: source_repo = "tmp_git_source"
    character(len=*), parameter :: target_repo = "tmp_git_target"

contains

    !> Collect all exported unit tests
    subroutine collect_git(tests)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest("broken-checkout-recovery", test_broken_checkout_recovery), &
            & new_unittest("failed-fetch-recovery", test_failed_fetch_recovery) &
            & ]

    end subroutine collect_git

    !> A repository left behind by an interrupted fetch is detected and refetched
    subroutine test_broken_checkout_recovery(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: stat
        type(git_target_t) :: target

        if (command_not_found('git')) return

        call cleanup()
        call create_source_repo(error)
        if (allocated(error)) return

        ! Simulate a half initialized repository: initialized but without any
        ! commit and with a stale lock file from an interrupted fetch
        call mkdir(target_repo)
        call execute_command_line("git init "//target_repo, exitstat=stat)
        call warnwrite(join_path(join_path(target_repo, ".git"), "shallow.lock"), [" "])

        if (git_checkout_is_valid(target_repo)) then
            call test_failed(error, "A half initialized repository was reported as valid")
            return
        end if

        target = git_target_default(source_repo)
        call target%checkout(target_repo, error)
        if (allocated(error)) return

        if (.not. git_checkout_is_valid(target_repo)) then
            call test_failed(error, "Checkout was not recovered from the broken repository")
            return
        end if

        if (.not. exists(join_path(target_repo, "README.md"))) then
            call test_failed(error, "Checkout does not contain the fetched files")
            return
        end if

        call cleanup()

    end subroutine test_broken_checkout_recovery

    !> A failed fetch cleans up the partial checkout so that the next attempt succeeds
    subroutine test_failed_fetch_recovery(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(git_target_t) :: target

        if (command_not_found('git')) return

        call cleanup()
        call create_source_repo(error)
        if (allocated(error)) return

        ! The first checkout fails because the remote does not exist
        target = git_target_default("tmp_git_missing_source")
        call target%checkout(target_repo, error)
        if (.not. allocated(error)) then
            call test_failed(error, "Checkout of a missing repository did not fail")
            return
        end if

        if (is_dir(target_repo)) then
            call test_failed(error, "A failed checkout was not cleaned up")
            return
        end if

        ! The second attempt with the valid remote must succeed
        target = git_target_default(source_repo)
        call target%checkout(target_repo, error)
        if (allocated(error)) return

        call cleanup()

    end subroutine test_failed_fetch_recovery

    !> Create a local git repository with a single commit
    subroutine create_source_repo(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: stat

        call execute_command_line("git init "//source_repo, exitstat=stat)
        if (stat /= 0) then
            call test_failed(error, "Cannot initialize the test repository")
            return
        end if

        call warnwrite(join_path(source_repo, "README.md"), ["test"])

        call execute_command_line("git -C "//source_repo//" add README.md", exitstat=stat)
        if (stat /= 0) then
            call test_failed(error, "Cannot stage files in the test repository")
            return
        end if

        call execute_command_line("git -C "//source_repo// &
            & " -c user.name=fpm -c user.email=fpm@example.com commit -m initial", exitstat=stat)
        if (stat /= 0) then
            call test_failed(error, "Cannot commit to the test repository")
        end if

    end subroutine create_source_repo

    !> Remove the directories used by the tests
    subroutine cleanup()
        if (is_dir(source_repo)) call os_delete_dir(os_is_unix(), source_repo)
        if (is_dir(target_repo)) call os_delete_dir(os_is_unix(), target_repo)
    end subroutine cleanup

end module test_git
