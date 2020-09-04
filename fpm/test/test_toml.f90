!> Define tests for the `fpm_toml` modules
module test_toml
    use testsuite, only : new_unittest, unittest_t, error_t
    use fpm_toml
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
            & new_unittest("missing-file", test_missing_file, should_fail=.true.)]

    end subroutine collect_toml


    !> Try to read some unnecessary obscure and convoluted but not invalid package file
    subroutine test_valid_toml(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table
        character(len=*), parameter :: manifest = 'fpm-valid-toml.toml'
        character(len=:), allocatable :: string
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[dependencies.fpm]', &
            & 'git = "https://github.com/fortran-lang/fpm"', &
            & '[[executable]]', &
            & 'name = "example-#1" # comment', &
            & 'source-dir = "prog"', &
            & '[dependencies]', &
            & 'toml-f.git = "git@github.com:toml-f/toml-f.git"', &
            & '"toml..f" = { path = ".." }', &
            & '[["executable"]]', &
            & 'name = "example-#2"', &
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
        character(len=:), allocatable :: string
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
        character(len=:), allocatable :: string
        integer :: unit

        call read_package_file(table, 'low+chance+of+existing.toml', error)

    end subroutine test_missing_file


end module test_toml
