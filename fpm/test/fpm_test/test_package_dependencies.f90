module test_package_dependencies
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, check
    use fpm_dependency
    use fpm_toml
    use fpm_manifest_dependency
    use fpm_git
    implicit none
    private

    public :: collect_package_dependencies


    character(len=*), parameter :: dep1 = "toml-f"
    character(len=*), parameter :: url1 = "https://github.com/toml-f/toml-f"
    character(len=*), parameter :: url2 = "https://github.com/awvwgk/toml-f"
    character(len=*), parameter :: rev1 = "c0ffee"
    character(len=*), parameter :: rev2 = "abcdef"


contains


    subroutine collect_package_dependencies(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
          & new_unittest("refetch-uncached", test_refetch_uncached), &
          & new_unittest("refetch-git-head", test_refetch_git_head), &
          & new_unittest("refetch-git-url", test_refetch_git_url), &
          & new_unittest("refetch-git-rev", test_refetch_git_rev), &
          & new_unittest("refetch-git-broken", test_refetch_git_broken), &
          & new_unittest("refetch-policy", test_refetch_policy), &
          & new_unittest("nofetch-policy", test_nofetch_policy), &
          & new_unittest("nofetch-git-rev", test_nofetch_git_rev), &
          & new_unittest("check-update-cache", test_check_update_deps), &
          & new_unittest("check-missing-cache", test_check_missing_deps, should_fail=.true.) &
          & ]

    end subroutine collect_package_dependencies


    subroutine test_refetch_uncached(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        config = new_dependency_walker(prefix="build", update=[dep1])
        dependency%name = dep1
        dependency%git = git_target_default(url1)

        call check(error, config%require_refetch(table, dependency), .true., &
            "Un-cached dependencies must always be refetched")

    end subroutine test_refetch_uncached


    subroutine test_refetch_git_head(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        call add_table(table, dep1, ptr)
        call set_value(ptr, "git-url", url1)
        call set_value(ptr, "git-obj", rev1)
        config = new_dependency_walker(prefix="build", update=[dep1])
        dependency%name = dep1
        dependency%git = git_target_default(url1)

        call check(error, config%require_refetch(table, dependency), .true., &
            "Un-pinned dependencies most always be refetched")

    end subroutine test_refetch_git_head


    !> Could also count as identical references and not refetch
    subroutine test_refetch_git_url(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        call add_table(table, dep1, ptr)
        call set_value(ptr, "git-url", url1)
        call set_value(ptr, "git-obj", rev1)
        config = new_dependency_walker(prefix="build", update=[dep1])
        dependency%name = dep1
        dependency%git = git_target_revision(url2, rev1)

        call check(error, config%require_refetch(table, dependency), .true., &
            "URL change requires dependency refetch")

    end subroutine test_refetch_git_url


    subroutine test_refetch_git_rev(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        call add_table(table, dep1, ptr)
        call set_value(ptr, "git-url", url1)
        call set_value(ptr, "git-obj", rev1)
        config = new_dependency_walker(prefix="build", update=[dep1])
        dependency%name = dep1
        dependency%git = git_target_revision(url1, rev2)

        call check(error, config%require_refetch(table, dependency), .true., &
            "Revision change requires dependency refetch")

    end subroutine test_refetch_git_rev


    !> Can only happen for broken cache files, in this case we should still refetch
    subroutine test_refetch_git_broken(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        call add_table(table, dep1, ptr)
        call set_value(ptr, "git-obj", rev1)
        config = new_dependency_walker(prefix="build", update=[dep1])
        dependency%name = dep1
        dependency%git = git_target_revision(url1, rev1)

        call check(error, config%require_refetch(table, dependency), .true., &
            "Broken cache file, should better trigger a refetch")

    end subroutine test_refetch_git_broken


    subroutine test_refetch_policy(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        call add_table(table, dep1, ptr)
        call set_value(ptr, "git-url", url1)
        call set_value(ptr, "git-obj", rev1)
        config = new_dependency_walker(prefix="build", update_all=.true.)
        dependency%name = dep1
        dependency%git = git_target_default(url1)

        call check(error, config%require_refetch(table, dependency), .true., &
            "Don't refetch for fetch-only policy")

    end subroutine test_refetch_policy


    subroutine test_nofetch_policy(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        call add_table(table, dep1, ptr)
        call set_value(ptr, "git-url", url1)
        call set_value(ptr, "git-obj", rev1)
        config = new_dependency_walker(prefix="build")
        dependency%name = dep1
        dependency%git = git_target_default(url1)

        call check(error, config%require_refetch(table, dependency), .false., &
            "Don't refetch for fetch-only policy")

    end subroutine test_nofetch_policy


    subroutine test_nofetch_git_rev(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        call add_table(table, dep1, ptr)
        call set_value(ptr, "git-url", url1)
        call set_value(ptr, "git-obj", rev1)
        config = new_dependency_walker(prefix="build", update=[dep1])
        dependency%name = dep1
        dependency%git = git_target_revision(url1, rev1)

        call check(error, config%require_refetch(table, dependency), .false., &
            "Matching revision are not updated")

    end subroutine test_nofetch_git_rev


    subroutine test_check_update_deps(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        call add_table(table, dep1, ptr)
        config = new_dependency_walker(prefix="build", update=[dep1])
        call check_update_deps(config, table, error)

    end subroutine test_check_update_deps


    subroutine test_check_missing_deps(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_config_t) :: dependency
        type(dependency_walker_t) :: config

        table = toml_table()
        config = new_dependency_walker(prefix="build", update=[dep1])
        call check_update_deps(config, table, error)

    end subroutine test_check_missing_deps


end module test_package_dependencies
