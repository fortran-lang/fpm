!> Define tests for the `fpm_manifest` modules
module test_manifest
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_manifest
    implicit none
    private

    public :: collect_manifest


contains


    !> Collect all exported unit tests
    subroutine collect_manifest(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("valid-manifest", test_valid_manifest), &
            & new_unittest("invalid-manifest", test_invalid_manifest, should_fail=.true.), &
            & new_unittest("default-library", test_default_library), &
            & new_unittest("default-executable", test_default_executable), &
            & new_unittest("dependency-empty", test_dependency_empty, should_fail=.true.), &
            & new_unittest("dependencies-empty", test_dependencies_empty), &
            & new_unittest("executable-empty", test_executable_empty, should_fail=.true.), &
            & new_unittest("library-empty", test_library_empty), &
            & new_unittest("package-empty", test_package_empty, should_fail=.true.), &
            & new_unittest("test-empty", test_test_empty, should_fail=.true.)]

    end subroutine collect_manifest


    !> Try to read some unnecessary obscure and convoluted but not invalid package file
    subroutine test_valid_manifest(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_t) :: package
        character(len=*), parameter :: manifest = 'fpm-valid-manifest.toml'
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

        call get_package_data(package, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')

        if (allocated(error)) return

        if (package%name /= "example") then
            call test_failed(error, "Package name is "//package%name//" but should be example")
            return
        end if

        if (.not.allocated(package%library)) then
            call test_failed(error, "library is not present in package data")
            return
        end if

        if (.not.allocated(package%executable)) then
            call test_failed(error, "executable is not present in package data")
            return
        end if

        if (size(package%executable) /= 2) then
            call test_failed(error, "Number of executables in package is not two")
            return
        end if

        if (.not.allocated(package%dependency)) then
            call test_failed(error, "dependency is not present in package data")
            return
        end if

        if (size(package%dependency) /= 3) then
            call test_failed(error, "Number of dependencies in package is not three")
            return
        end if

        if (allocated(package%test)) then
            call test_failed(error, "test is present in package but not in package file")
            return
        end if

    end subroutine test_valid_manifest


    !> Try to read a valid TOML document which represent an invalid package file
    subroutine test_invalid_manifest(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_t) :: package
        character(len=*), parameter :: manifest = 'fpm-invalid-manifest.toml'
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & '[package]', &
            & 'name = "example"', &
            & 'version = "0.1.0"'
        close(unit)

        call get_package_data(package, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')

    end subroutine test_invalid_manifest


    !> Create a default library
    subroutine test_default_library(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_t) :: package

        allocate(package%library)
        call default_library(package%library)

        if (.not.allocated(package%library%source_dir)) then
            call test_failed(error, "Default library source-dir is not set")
            return
        end if

        if (package%library%source_dir /= "src") then
            call test_failed(error, "Default library source-dir is "// &
                & package%library%source_dir//" but should be src")
            return
        end if

    end subroutine test_default_library


    !> Create a default executable
    subroutine test_default_executable(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_t) :: package
        character(len=*), parameter :: name = "default"

        allocate(package%executable(1))
        call default_executable(package%executable(1), name)

        if (.not.allocated(package%executable(1)%source_dir)) then
            call test_failed(error, "Default executable source-dir is not set")
            return
        end if

        if (package%executable(1)%source_dir /= "app") then
            call test_failed(error, "Default executable source-dir is "// &
                & package%executable(1)%source_dir//" but should be app")
            return
        end if

        if (package%executable(1)%name /= name) then
            call test_failed(error, "Default executable name is "// &
                & package%executable(1)%name//" but should be "//name)
            return
        end if

    end subroutine test_default_executable


    !> Dependencies cannot be created from empty tables
    subroutine test_dependency_empty(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(dependency_t) :: dependency

        call new_table(table)
        table%key = "example"

        call new_dependency(dependency, table, error)

    end subroutine test_dependency_empty


    !> Dependency tables can be empty
    subroutine test_dependencies_empty(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(dependency_t), allocatable :: dependencies(:)

        call new_table(table)

        call new_dependencies(dependencies, table, error)
        if (allocated(error)) return

        if (allocated(dependencies)) then
            call test_failed(error, "Found dependencies in empty table")
        end if

    end subroutine test_dependencies_empty


    !> Executables cannot be created from empty tables
    subroutine test_executable_empty(error)
        use fpm_manifest_executable
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(executable_t) :: executable

        call new_table(table)

        call new_executable(executable, table, error)

    end subroutine test_executable_empty


    !> Libraries can be created from empty tables
    subroutine test_library_empty(error)
        use fpm_manifest_library
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(library_t) :: library

        call new_table(table)

        call new_library(library, table, error)
        if (allocated(error)) return

        if (.not.allocated(library%source_dir)) then
            call test_failed(error, "Default library source-dir is not set")
            return
        end if

        if (library%source_dir /= "src") then
            call test_failed(error, "Default library source-dir is "// &
                & library%source_dir//" but should be src")
            return
        end if

    end subroutine test_library_empty


    !> Packages cannot be created from empty tables
    subroutine test_package_empty(error)
        use fpm_manifest_package
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(package_t) :: package

        call new_table(table)

        call new_package(package, table, error)

    end subroutine test_package_empty


    !> Tests cannot be created from empty tables
    subroutine test_test_empty(error)
        use fpm_manifest_test
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(test_t) :: test

        call new_table(table)

        call new_test(test, table, error)

    end subroutine test_test_empty


end module test_manifest
