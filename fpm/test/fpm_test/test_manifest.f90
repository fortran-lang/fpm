!> Define tests for the `fpm_manifest` modules
module test_manifest
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, &
        & check_string
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
            & new_unittest("dependency-pathtag", test_dependency_pathtag, should_fail=.true.), &
            & new_unittest("dependency-gitpath", test_dependency_gitpath, should_fail=.true.), &
            & new_unittest("dependency-nourl", test_dependency_nourl, should_fail=.true.), &
            & new_unittest("dependency-gitconflict", test_dependency_gitconflict, should_fail=.true.), &
            & new_unittest("dependency-wrongkey", test_dependency_wrongkey, should_fail=.true.), &
            & new_unittest("dependencies-empty", test_dependencies_empty), &
            & new_unittest("dependencies-typeerror", test_dependencies_typeerror, should_fail=.true.), &
            & new_unittest("executable-empty", test_executable_empty, should_fail=.true.), &
            & new_unittest("executable-typeerror", test_executable_typeerror, should_fail=.true.), &
            & new_unittest("executable-noname", test_executable_noname, should_fail=.true.), &
            & new_unittest("executable-wrongkey", test_executable_wrongkey, should_fail=.true.), &
            & new_unittest("library-empty", test_library_empty), &
            & new_unittest("library-wrongkey", test_library_wrongkey, should_fail=.true.), &
            & new_unittest("package-simple", test_package_simple), &
            & new_unittest("package-empty", test_package_empty, should_fail=.true.), &
            & new_unittest("package-typeerror", test_package_typeerror, should_fail=.true.), &
            & new_unittest("package-noname", test_package_noname, should_fail=.true.), &
            & new_unittest("package-wrongexe", test_package_wrongexe, should_fail=.true.), &
            & new_unittest("package-wrongtest", test_package_wrongtest, should_fail=.true.), &
            & new_unittest("test-simple", test_test_simple), &
            & new_unittest("test-empty", test_test_empty, should_fail=.true.), &
            & new_unittest("test-typeerror", test_test_typeerror, should_fail=.true.), &
            & new_unittest("test-noname", test_test_noname, should_fail=.true.), &
            & new_unittest("test-wrongkey", test_test_wrongkey, should_fail=.true.)]

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

        call check_string(error, package%library%source_dir, "src", &
            & "Default library source-dir")
        if (allocated(error)) return

    end subroutine test_default_library


    !> Create a default executable
    subroutine test_default_executable(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_t) :: package
        character(len=*), parameter :: name = "default"

        allocate(package%executable(1))
        call default_executable(package%executable(1), name)

        call check_string(error, package%executable(1)%source_dir, "app", &
            & "Default executable source-dir")
        if (allocated(error)) return

        call check_string(error, package%executable(1)%name, name, &
            & "Default executable name")
        if (allocated(error)) return

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


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_pathtag(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'path', '"package"', stat)
        call set_value(table, 'tag', '"v20.1"', stat)

        call new_dependency(dependency, table, error)

    end subroutine test_dependency_pathtag


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_nourl(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'tag', '"v20.1"', stat)

        call new_dependency(dependency, table, error)

    end subroutine test_dependency_nourl


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_gitpath(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'path', '"package"', stat)
        call set_value(table, 'git', '"https://gitea.com/fortran-lang/pack"', stat)

        call new_dependency(dependency, table, error)

    end subroutine test_dependency_gitpath


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_gitconflict(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'git', '"https://gitea.com/fortran-lang/pack"', stat)
        call set_value(table, 'branch', '"latest"', stat)
        call set_value(table, 'tag', '"v20.1"', stat)

        call new_dependency(dependency, table, error)

    end subroutine test_dependency_gitconflict


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_wrongkey(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'not-available', '"anywhere"', stat)

        call new_dependency(dependency, table, error)

    end subroutine test_dependency_wrongkey


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


    !> Add a dependency as an array, which is not supported
    subroutine test_dependencies_typeerror(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, add_array, toml_table, toml_array

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_array), pointer :: children
        integer :: stat
        type(dependency_t), allocatable :: dependencies(:)

        call new_table(table)
        call add_array(table, 'dep1', children, stat)

        call new_dependencies(dependencies, table, error)

    end subroutine test_dependencies_typeerror


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


    !> Pass a wrong TOML type to the name field of the executable
    subroutine test_executable_typeerror(error)
        use fpm_manifest_executable
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(executable_t) :: executable

        call new_table(table)
        call add_table(table, 'name', child, stat)

        call new_executable(executable, table, error)

    end subroutine test_executable_typeerror


    !> Pass a TOML table with insufficient entries to the executable constructor
    subroutine test_executable_noname(error)
        use fpm_manifest_executable
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(executable_t) :: executable

        call new_table(table)
        call add_table(table, 'dependencies', child, stat)

        call new_executable(executable, table, error)

    end subroutine test_executable_noname


    !> Pass a TOML table with not allowed keys
    subroutine test_executable_wrongkey(error)
        use fpm_manifest_executable
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(executable_t) :: executable

        call new_table(table)
        call add_table(table, 'wrong-field', child, stat)

        call new_executable(executable, table, error)

    end subroutine test_executable_wrongkey


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

        call check_string(error, library%source_dir, "src", &
            & "Default library source-dir")
        if (allocated(error)) return

    end subroutine test_library_empty


    !> Pass a TOML table with not allowed keys
    subroutine test_library_wrongkey(error)
        use fpm_manifest_library
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(library_t) :: library

        call new_table(table)
        call add_table(table, 'not-allowed', child, stat)

        call new_library(library, table, error)

    end subroutine test_library_wrongkey


    !> Packages cannot be created from empty tables
    subroutine test_package_simple(error)
        use fpm_manifest_package
        use fpm_toml, only : new_table, add_table, add_array, set_value, &
            & toml_table, toml_array

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child, child2
        type(toml_array), pointer :: children
        integer :: stat
        type(package_t) :: package

        call new_table(table)
        call set_value(table, 'name', '"example"', stat)
        call set_value(table, 'license', '"MIT"', stat)
        call add_table(table, 'dev-dependencies', child, stat)
        call add_table(child, 'pkg1', child2, stat)
        call set_value(child2, 'git', '"https://github.com/fortran-lang/pkg1"', stat)
        call add_table(child, 'pkg2', child2)
        call set_value(child2, 'git', '"https://gitlab.com/fortran-lang/pkg2"', stat)
        call set_value(child2, 'branch', '"devel"', stat)
        call add_table(child, 'pkg3', child2)
        call set_value(child2, 'git', '"https://bitbucket.org/fortran-lang/pkg3"', stat)
        call set_value(child2, 'rev', '"9fceb02d0ae598e95dc970b74767f19372d61af8"', stat)
        call add_table(child, 'pkg4', child2)
        call set_value(child2, 'git', '"https://gitea.com/fortran-lang/pkg4"', stat)
        call set_value(child2, 'tag', '"v1.8.5-rc3"', stat)
        call add_array(table, 'test', children, stat)
        call add_table(children, child, stat)
        call set_value(child, 'name', '"tester"', stat)

        call new_package(package, table, error)

    end subroutine test_package_simple


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


    !> Create an array in the package name, which should cause an error
    subroutine test_package_typeerror(error)
        use fpm_manifest_package
        use fpm_toml, only : new_table, add_array, toml_table, toml_array

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_array), pointer :: child
        integer :: stat
        type(package_t) :: package

        call new_table(table)
        call add_array(table, "name", child, stat)

        call new_package(package, table, error)

    end subroutine test_package_typeerror


    !> Try to create a new package without a name field
    subroutine test_package_noname(error)
        use fpm_manifest_package
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(package_t) :: package

        call new_table(table)
        call add_table(table, "library", child, stat)
        call add_table(table, "dev-dependencies", child, stat)
        call add_table(table, "dependencies", child, stat)

        call new_package(package, table, error)

    end subroutine test_package_noname


    !> Try to read executables from a mixed type array
    subroutine test_package_wrongexe(error)
        use fpm_manifest_package
        use fpm_toml, only : new_table, set_value, add_array, toml_table, toml_array

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_array), pointer :: children, children2
        integer :: stat
        type(package_t) :: package

        call new_table(table)
        call set_value(table, 'name', '"example"', stat)
        call add_array(table, 'executable', children, stat)
        call add_array(children, children2, stat)

        call new_package(package, table, error)

    end subroutine test_package_wrongexe


    !> Try to read tests from a mixed type array
    subroutine test_package_wrongtest(error)
        use fpm_manifest_package
        use fpm_toml, only : new_table, set_value, add_array, toml_table, toml_array

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_array), pointer :: children, children2
        integer :: stat
        type(package_t) :: package

        call new_table(table)
        call set_value(table, 'name', '"example"', stat)
        call add_array(table, 'test', children, stat)
        call add_array(children, children2, stat)

        call new_package(package, table, error)

    end subroutine test_package_wrongtest


    !> Tests cannot be created from empty tables
    subroutine test_test_simple(error)
        use fpm_manifest_test
        use fpm_toml, only : new_table, set_value, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(test_t) :: test

        call new_table(table)
        call set_value(table, 'name', '"example"', stat)
        call set_value(table, 'source-dir', '"tests"', stat)
        call set_value(table, 'main', '"tester.f90"', stat)
        call add_table(table, 'dependencies', child, stat)

        call new_test(test, table, error)
        if (allocated(error)) return

        call check_string(error, test%main, "tester.f90", "Test main")
        if (allocated(error)) return

    end subroutine test_test_simple


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


    !> Pass a wrong TOML type to the name field of the test
    subroutine test_test_typeerror(error)
        use fpm_manifest_test
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(test_t) :: test

        call new_table(table)
        call add_table(table, 'name', child, stat)

        call new_test(test, table, error)

    end subroutine test_test_typeerror


    !> Pass a TOML table with insufficient entries to the test constructor
    subroutine test_test_noname(error)
        use fpm_manifest_test
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(test_t) :: test

        call new_table(table)
        call add_table(table, 'dependencies', child, stat)

        call new_test(test, table, error)

    end subroutine test_test_noname


    !> Pass a TOML table with not allowed keys
    subroutine test_test_wrongkey(error)
        use fpm_manifest_test
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(test_t) :: test

        call new_table(table)
        call add_table(table, 'not-supported', child, stat)

        call new_test(test, table, error)

    end subroutine test_test_wrongkey


end module test_manifest
