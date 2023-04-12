!> Define tests for the `fpm_toml` modules
module test_toml
    use testsuite, only : new_unittest, unittest_t, error_t
    use fpm_toml
    use fpm_git
    use fpm_dependency, only: dependency_node_t, destroy_dependency_node
    use fpm_manifest_dependency, only: dependency_config_t, dependency_destroy
    use fpm_versioning, only: new_version

    implicit none
    private

    public :: collect_toml


contains


    !> Collect all exported unit tests
    subroutine collect_toml(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("valid-toml", test_valid_toml), &
            & new_unittest("invalid-toml", test_invalid_toml, should_fail=.true.), &
            & new_unittest("missing-file", test_missing_file, should_fail=.true.), &
            & new_unittest("serialize-git-target", git_target_roundtrip), &
            & new_unittest("serialize-dependency-config", dependency_config_roundtrip), &
            & new_unittest("serialize-dependency-node", dependency_node_roundtrip)]

    end subroutine collect_toml


    !> Try to read some unnecessary obscure and convoluted but not invalid package file
    subroutine test_valid_toml(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table
        character(len=*), parameter :: manifest = 'fpm-valid-toml.toml'
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[dependencies.fpm]', &
            & 'git = "https://github.com/fortran-lang/fpm"', &
            & '[[executable]]', &
            & 'name = "example-1" # comment', &
            & 'source-dir = "prog"', &
            & '[dependencies]', &
            & 'toml-f.git = "git@github.com:toml-f/toml-f.git"', &
            & '"toml..f" = { path = ".." }', &
            & '[["executable"]]', &
            & 'name = "example-2"', &
            & 'source-dir = "prog"', &
            & '[executable.dependencies]', &
            & '[''library'']', &
            & 'source-dir = """', &
            & 'lib""" # comment'
        close(unit)

        call read_package_file(table, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')

    end subroutine test_valid_toml


    !> Try to read an invalid TOML document
    subroutine test_invalid_toml(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table
        character(len=*), parameter :: manifest = 'fpm-invalid-toml.toml'
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & '# INVALID TOML DOC', &
            & 'name = "example"', &
            & 'dependencies.fpm.git = "https://github.com/fortran-lang/fpm"', &
            & '[dependencies]', &
            & 'toml-f.git = "git@github.com:toml-f/toml-f.git"', &
            & '"toml..f" = { path = ".." }'
        close(unit)

        call read_package_file(table, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')

    end subroutine test_invalid_toml


    !> Try to read configuration from a non-existing file
    subroutine test_missing_file(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table

        call read_package_file(table, 'low+chance+of+existing.toml', error)

    end subroutine test_missing_file

    !> Test git_target_t serialization
    subroutine git_target_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table

        type(git_target_t) :: git

        ! Revision type
        git = git_target_revision(url="https://github.com/urbanjost/M_CLI2.git", &
                                  sha1="7264878cdb1baff7323cc48596d829ccfe7751b8")
        call git%test_serialization("revision git type",error)
        if (allocated(error)) return

        ! Branch type
        git = git_target_branch(url="https://github.com/urbanjost/M_CLI2.git", &
                                branch="main")
        call git%test_serialization("branch git type",error)
        if (allocated(error)) return

        ! Tag type
        git = git_target_tag(url="https://github.com/urbanjost/M_CLI2.git", &
                             tag="1.0.0")
        call git%test_serialization("target git type",error)
        if (allocated(error)) return

        ! Incomplete type
        if (allocated(git%object)) deallocate(git%object)
        call git%test_serialization("incomplete git type 1/2",error)
        if (allocated(error)) return

        ! Incomplete type
        if (allocated(git%url)) deallocate(git%url)
        call git%test_serialization("incomplete git type 2/2",error)
        if (allocated(error)) return

    end subroutine git_target_roundtrip


    !> Test git_target_t serialization
    subroutine dependency_config_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table

        type(dependency_config_t) :: dep

        call dependency_destroy(dep)

        dep%name = "M_CLI2"
        dep%path = "~/./some/dummy/path"
        dep%namespace = "urbanjost"
        allocate(dep%requested_version)
        call new_version(dep%requested_version, "3.2.0",error); if (allocated(error)) return

        allocate(dep%git)
        dep%git = git_target_revision(url="https://github.com/urbanjost/M_CLI2.git", &
                                      sha1="7264878cdb1baff7323cc48596d829ccfe7751b8")

        ! Test full object
        call dep%test_serialization("full object",error)
        if (allocated(error)) return

        ! Remove namespace
        deallocate(dep%namespace)
        call dep%test_serialization("no namespace",error)
        if (allocated(error)) return

        ! Remove git
        deallocate(dep%git)
        call dep%test_serialization("no git",error)
        if (allocated(error)) return

        ! Remove version
        deallocate(dep%requested_version)
        call dep%test_serialization("no requested_version",error)
        if (allocated(error)) return

        ! Remove name
        deallocate(dep%name)
        call dep%test_serialization("no name",error)
        if (allocated(error)) return

        ! Remove path
        deallocate(dep%path)
        call dep%test_serialization("no path",error)
        if (allocated(error)) return

    end subroutine dependency_config_roundtrip

    !> Test dependency_node_t serialization
    subroutine dependency_node_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table

        type(dependency_node_t) :: dep

        call destroy_dependency_node(dep)

        dep%name = "M_CLI2"
        dep%path = "~/./some/dummy/path"
        dep%proj_dir = "~/./"
        dep%namespace = "urbanjost"
        dep%revision = "7264878cdb1baff7323cc48596d829ccfe7751b8"
        dep%cached = .true.
        dep%done = .false.
        dep%update = .true.
        allocate(dep%requested_version)
        call new_version(dep%requested_version, "3.2.0",error); if (allocated(error)) return
        allocate(dep%version)
        call new_version(dep%version, "4.53.2",error); if (allocated(error)) return

        allocate(dep%git)
        dep%git = git_target_revision(url="https://github.com/urbanjost/M_CLI2.git", &
                                      sha1="7264878cdb1baff7323cc48596d829ccfe7751b8")

        ! Test full object
        call dep%test_serialization("full object",error)
        if (allocated(error)) return

        ! Remove namespace
        deallocate(dep%namespace)
        call dep%test_serialization("no namespace",error)
        if (allocated(error)) return

        ! Remove git
        deallocate(dep%git)
        call dep%test_serialization("no git",error)
        if (allocated(error)) return

        ! Remove version
        deallocate(dep%requested_version)
        call dep%test_serialization("no requested_version",error)
        if (allocated(error)) return

        ! Remove name
        deallocate(dep%name)
        call dep%test_serialization("no name",error)
        if (allocated(error)) return

        ! Remove path
        deallocate(dep%path)
        call dep%test_serialization("no path",error)
        if (allocated(error)) return

        ! Remove revision
        deallocate(dep%revision)
        call dep%test_serialization("no revision",error)
        if (allocated(error)) return

        ! Remove proj_dir
        deallocate(dep%proj_dir)
        call dep%test_serialization("no proj_dir",error)
        if (allocated(error)) return

        ! Remove version
        deallocate(dep%version)
        call dep%test_serialization("no version",error)
        if (allocated(error)) return

    end subroutine dependency_node_roundtrip

end module test_toml
