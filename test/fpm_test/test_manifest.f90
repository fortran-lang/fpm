!> Define tests for the `fpm_manifest` modules
module test_manifest
    use fpm_filesystem, only: get_temp_filename
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, check_string
    use fpm_manifest
    use fpm_manifest_profile, only: profile_config_t, find_profile
    use fpm_strings, only: operator(.in.)
    use fpm_error, only: fatal_error, error_t
    implicit none
    private
    public :: collect_manifest

contains

    !> Collect all exported unit tests
    subroutine collect_manifest(tests)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest("valid-manifest", test_valid_manifest), &
            & new_unittest("invalid-manifest", test_invalid_manifest, should_fail=.true.), &
            & new_unittest("default-library", test_default_library), &
            & new_unittest("default-executable", test_default_executable), &
            & new_unittest("dependency-empty", test_dependency_empty, should_fail=.true.), &
            & new_unittest("dependency-pathtag", test_dependency_pathtag, should_fail=.true.), &
            & new_unittest("dependency-gitpath", test_dependency_gitpath, should_fail=.true.), &
            & new_unittest("dependency-nourl", test_dependency_nourl, should_fail=.true.), &
            & new_unittest("dependency-gitconflict", test_dependency_gitconflict, should_fail=.true.), &
            & new_unittest("dependency-invalid-git", test_dependency_invalid_git, should_fail=.true.), &
            & new_unittest("dependency-no-namespace", test_dependency_no_namespace, should_fail=.true.), &
            & new_unittest("dependency-redundant-v", test_dependency_redundant_v, should_fail=.true.), &
            & new_unittest("dependency-wrongkey", test_dependency_wrongkey, should_fail=.true.), &
            & new_unittest("dependencies-empty", test_dependencies_empty), &
            & new_unittest("dependencies-typeerror", test_dependencies_typeerror, should_fail=.true.), &
            & new_unittest("profiles", test_profiles), &
            & new_unittest("profiles-keyvalue-table", test_profiles_keyvalue_table, should_fail=.true.), &
            & new_unittest("executable-empty", test_executable_empty, should_fail=.true.), &
            & new_unittest("executable-typeerror", test_executable_typeerror, should_fail=.true.), &
            & new_unittest("executable-noname", test_executable_noname, should_fail=.true.), &
            & new_unittest("executable-wrongkey", test_executable_wrongkey, should_fail=.true.), &
            & new_unittest("build-config-valid", test_build_valid), &
            & new_unittest("build-config-empty", test_build_empty), &
            & new_unittest("build-config-invalid-values", test_build_invalid_values, should_fail=.true.), &
            & new_unittest("build-key-invalid", test_build_invalid_key), &
            & new_unittest("library-empty", test_library_empty), &
            & new_unittest("library-wrongkey", test_library_wrongkey, should_fail=.true.), &
            & new_unittest("package-simple", test_package_simple), &
            & new_unittest("package-empty", test_package_empty, should_fail=.true.), &
            & new_unittest("package-typeerror", test_package_typeerror, should_fail=.true.), &
            & new_unittest("package-noname", test_package_noname, should_fail=.true.), &
            & new_unittest("package-wrongexe", test_package_wrongexe, should_fail=.true.), &
            & new_unittest("package-wrongtest", test_package_wrongtest, should_fail=.true.), &
            & new_unittest("package-duplicate", test_package_duplicate, should_fail=.true.), &
            & new_unittest("test-simple", test_test_simple), &
            & new_unittest("test-empty", test_test_empty, should_fail=.true.), &
            & new_unittest("test-typeerror", test_test_typeerror, should_fail=.true.), &
            & new_unittest("test-noname", test_test_noname, should_fail=.true.), &
            & new_unittest("test-wrongkey", test_test_wrongkey, should_fail=.true.), &
            & new_unittest("link-string", test_link_string), &
            & new_unittest("link-array", test_link_array), &
            & new_unittest("link-error", test_invalid_link, should_fail=.true.), &
            & new_unittest("example-simple", test_example_simple), &
            & new_unittest("example-empty", test_example_empty, should_fail=.true.), &
            & new_unittest("install-library", test_install_library), &
            & new_unittest("install-empty", test_install_empty), &
            & new_unittest("install-wrongkey", test_install_wrongkey, should_fail=.true.), &
            & new_unittest("preprocess-empty", test_preprocess_empty), &
            & new_unittest("preprocess-wrongkey", test_preprocess_wrongkey, should_fail=.true.), &
            & new_unittest("preprocessors-empty", test_preprocessors_empty, should_fail=.true.), &
            & new_unittest("macro-parsing", test_macro_parsing, should_fail=.false.), &
            & new_unittest("macro-parsing-dependency", test_macro_parsing_dependency, should_fail=.false.) &
            & ]

    end subroutine collect_manifest


    !> Try to read some unnecessary obscure and convoluted but not invalid package file
    subroutine test_valid_manifest(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(len=*), parameter :: manifest = 'fpm-valid-manifest.toml'
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[build]', &
            & 'auto-executables = false', &
            & 'auto-tests = false', &
            & 'module-naming = false', &
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
            & 'lib""" # comment', &
            & '[preprocess]', &
            & '[preprocess.cpp]', &
            & 'suffixes = ["F90", "f90"]', &
            & 'directories = ["src/feature1", "src/models"]', &
            & 'macros = ["FOO", "BAR"]'
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

        if (.not.allocated(package%preprocess)) then
            call test_failed(error, "Preprocessor is not present in package data")
            return
        end if

        if (size(package%preprocess) /= 1) then
            call test_failed(error, "Number of preprocessors in package is not one")
            return
        end if

    end subroutine test_valid_manifest


    !> Try to read a valid TOML document which represent an invalid package file
    subroutine test_invalid_manifest(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
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

        type(package_config_t) :: package

        allocate(package%library)
        call default_library(package%library)

        call check_string(error, package%library%source_dir, "src", &
            & "Default library source-dir")
        if (allocated(error)) return

        if (.not.allocated(package%library%include_dir)) then
            call test_failed(error,"Default include-dir list not allocated")
            return
        end if

        if (.not.("include".in.package%library%include_dir)) then
            call test_failed(error,"'include' not in default include-dir list")
            return
        end if

    end subroutine test_default_library


    !> Create a default executable
    subroutine test_default_executable(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
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
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = "example"

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_empty


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_pathtag(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'path', 'package', stat)
        call set_value(table, 'tag', 'v20.1', stat)

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_pathtag


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_nourl(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'tag', 'v20.1', stat)

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_nourl


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_gitpath(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'path', 'package', stat)
        call set_value(table, 'git', 'https://gitea.com/fortran-lang/pack', stat)

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_gitpath


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_gitconflict(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'git', 'https://gitea.com/fortran-lang/pack', stat)
        call set_value(table, 'branch', 'latest', stat)
        call set_value(table, 'tag', 'v20.1', stat)

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_gitconflict


    !> Try to create a git dependency with an invalid source format.
    subroutine test_dependency_invalid_git(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'git', 123) ! Not a string

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_invalid_git

    !> Namespace is necessary if a dependency is not a git or path dependency
    subroutine test_dependency_no_namespace(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'v', 'abc')

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_no_namespace

    !> Do not specify version with a git or path dependency
    subroutine test_dependency_redundant_v(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'v', '0.0.0')
        call set_value(table, 'path', 'abc')

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_redundant_v


    !> Try to create a dependency with conflicting entries
    subroutine test_dependency_wrongkey(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(dependency_config_t) :: dependency

        call new_table(table)
        table%key = 'example'
        call set_value(table, 'not-available', 'anywhere', stat)

        call new_dependency(dependency, table, error=error)

    end subroutine test_dependency_wrongkey


    !> Dependency tables can be empty
    subroutine test_dependencies_empty(error)
        use fpm_manifest_dependency
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(dependency_config_t), allocatable :: dependencies(:)

        call new_table(table)

        call new_dependencies(dependencies, table, error=error)
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
        type(dependency_config_t), allocatable :: dependencies(:)

        call new_table(table)
        call add_array(table, 'dep1', children, stat)

        call new_dependencies(dependencies, table, error=error)

    end subroutine test_dependencies_typeerror

    !> Include a table of profiles in toml, check whether they are parsed correctly and stored in package
    subroutine test_profiles(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(len=*), parameter :: manifest = 'fpm-profiles.toml'
        integer :: unit
        character(:), allocatable :: profile_name, compiler
        logical :: profile_found
        type(profile_config_t) :: chosen_profile

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[profiles.release.gfortran.linux]', &
            & 'flags = "1" #release.gfortran.linux', &
            & '[profiles.release.gfortran]', &
            & 'flags = "2" #release.gfortran.all', &
            & '[profiles.gfortran.linux]', &
            & 'flags = "3" #all.gfortran.linux', &
            & '[profiles.gfortran]', &
            & 'flags = "4" #all.gfortran.all', &
            & '[profiles.release.ifort]', &
            & 'flags = "5" #release.ifort.all'
        close(unit)

        call get_package_data(package, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')

        if (allocated(error)) return

        profile_name = 'release'
        compiler = 'gfortran'
        call find_profile(package%profiles, profile_name, compiler, 1, profile_found, chosen_profile)
        if (.not.(chosen_profile%flags.eq.'1 3')) then
            call test_failed(error, "Failed to append flags from profiles named 'all'")
            return
        end if

        profile_name = 'release'
        compiler = 'gfortran'
        call find_profile(package%profiles, profile_name, compiler, 3, profile_found, chosen_profile)
        if (.not.(chosen_profile%flags.eq.'2 4')) then
            call test_failed(error, "Failed to choose profile with OS 'all'")
            return
        end if

        profile_name = 'publish'
        compiler = 'gfortran'
        call find_profile(package%profiles, profile_name, compiler, 1, profile_found, chosen_profile)
        if (allocated(chosen_profile%flags)) then
            call test_failed(error, "Profile named "//profile_name//" should not exist")
            return
        end if

        profile_name = 'debug'
        compiler = 'ifort'
        call find_profile(package%profiles, profile_name, compiler, 3, profile_found, chosen_profile)
        if (.not.(chosen_profile%flags.eq.&
            ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl /standard-semantics /traceback')) then
            call test_failed(error, "Failed to load built-in profile "//profile_name)
            return
        end if

        profile_name = 'release'
        compiler = 'ifort'
        call find_profile(package%profiles, profile_name, compiler, 1, profile_found, chosen_profile)
        if (.not.(chosen_profile%flags.eq.'5')) then
            call test_failed(error, "Failed to overwrite built-in profile")
            return
        end if
    end subroutine test_profiles

    !> 'flags' is a key-value entry, test should fail as it is defined as a table
    subroutine test_profiles_keyvalue_table(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(len=*), parameter :: manifest = 'fpm-profiles-error.toml'
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[profiles.linux.flags]'
        close(unit)

        call get_package_data(package, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')
    end subroutine test_profiles_keyvalue_table

    !> Executables cannot be created from empty tables
    subroutine test_executable_empty(error)
        use fpm_manifest_executable
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(executable_config_t) :: executable

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
        type(executable_config_t) :: executable

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
        type(executable_config_t) :: executable

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
        type(executable_config_t) :: executable

        call new_table(table)
        call add_table(table, 'wrong-field', child, stat)

        call new_executable(executable, table, error)

    end subroutine test_executable_wrongkey


    !> Try to read values from the [build] table
    subroutine test_build_valid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[build]', &
            & 'auto-executables = false', &
            & 'auto-tests = false', &
            & 'module-naming = true'
        close(unit)

        call get_package_data(package, temp_file, error)

        if (allocated(error)) return

        if (package%build%auto_executables) then
            call test_failed(error, "Wrong value of 'auto-executables' read, expecting .false.")
            return
        end if

        if (package%build%auto_tests) then
            call test_failed(error, "Wrong value of 'auto-tests' read, expecting .false.")
            return
        end if

        if (.not. package%build%module_naming) then
            call test_failed(error, "Wrong value of 'module-naming' read, expecting .true.")
            return
        end if

    end subroutine test_build_valid


    !> Try to read values from the [build] table
    subroutine test_build_invalid_key(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(error_t), allocatable :: build_error

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[build]', &
            & 'auto-executables = false', &
            & 'auto-tests = false ', &
            & 'module-naming = true ', &
            & 'this-will-fail = true '
        close(unit)

        call get_package_data(package, temp_file, build_error)

        ! Error message should contain both package name and key name
        if (allocated(build_error)) then

            if (.not.index(build_error%message,'this-will-fail')>0) then
                call fatal_error(error, 'no invalid key name is printed to output')
                return
            end if

            if (.not.index(build_error%message,'example')>0) then
                call fatal_error(error, 'no package name is printed to output')
                return
            end if

        else
            call fatal_error(error, 'no error allocated on invalid [build] section key ')
            return
        end if

    end subroutine test_build_invalid_key


    !> Try to read values from an empty [build] table
    subroutine test_build_empty(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[build]', &
            & '[library]'
        close(unit)

        call get_package_data(package, temp_file, error)

        if (allocated(error)) return

        if (.not.package%build%auto_executables) then
            call test_failed(error, "Wrong default value of 'auto-executables' read, expecting .true.")
            return
        end if

        if (.not.package%build%auto_tests) then
            call test_failed(error, "Wrong default value of 'auto-tests' read, expecting .true.")
            return
        end if

        if (package%build%module_naming) then
            call test_failed(error, "Wrong default value of 'module-naming' read, expecting .false.")
            return
        end if

    end subroutine test_build_empty


    !> Try to read values from a [build] table with invalid values
    subroutine test_build_invalid_values(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[build]', &
            & 'auto-executables = "false"'
        close(unit)

        call get_package_data(package, temp_file, error)

    end subroutine test_build_invalid_values


    !> Libraries can be created from empty tables
    subroutine test_library_empty(error)
        use fpm_manifest_library
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(library_config_t) :: library

        call new_table(table)

        call new_library(library, table, error)
        if (allocated(error)) return

        call check_string(error, library%source_dir, "src", &
            & "Default library source-dir")
        if (allocated(error)) return

        if (.not.allocated(library%include_dir)) then
            call test_failed(error,"Default include-dir list not allocated")
            return
        end if

        if (.not.("include".in.library%include_dir)) then
            call test_failed(error,"'include' not in default include-dir list")
            return
        end if

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
        type(library_config_t) :: library

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
        type(package_config_t) :: package

        call new_table(table)
        call set_value(table, 'name', 'example', stat)
        call set_value(table, 'license', 'MIT', stat)
        call add_table(table, 'dev-dependencies', child, stat)
        call add_table(child, 'pkg1', child2, stat)
        call set_value(child2, 'git', 'https://github.com/fortran-lang/pkg1', stat)
        call add_table(child, 'pkg2', child2)
        call set_value(child2, 'git', 'https://gitlab.com/fortran-lang/pkg2', stat)
        call set_value(child2, 'branch', 'devel', stat)
        call add_table(child, 'pkg3', child2)
        call set_value(child2, 'git', 'https://bitbucket.org/fortran-lang/pkg3', stat)
        call set_value(child2, 'rev', '9fceb02d0ae598e95dc970b74767f19372d61af8', stat)
        call add_table(child, 'pkg4', child2)
        call set_value(child2, 'git', 'https://gitea.com/fortran-lang/pkg4', stat)
        call set_value(child2, 'tag', 'v1.8.5-rc3', stat)
        call add_array(table, 'test', children, stat)
        call add_table(children, child, stat)
        call set_value(child, 'name', 'tester', stat)

        call new_package(package, table, error=error)

    end subroutine test_package_simple


    !> Packages cannot be created from empty tables
    subroutine test_package_empty(error)
        use fpm_manifest_package
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(package_config_t) :: package

        call new_table(table)

        call new_package(package, table, error=error)

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
        type(package_config_t) :: package

        call new_table(table)
        call add_array(table, "name", child, stat)

        call new_package(package, table, error=error)

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
        type(package_config_t) :: package

        call new_table(table)
        call add_table(table, "library", child, stat)
        call add_table(table, "dev-dependencies", child, stat)
        call add_table(table, "dependencies", child, stat)

        call new_package(package, table, error=error)

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
        type(package_config_t) :: package

        call new_table(table)
        call set_value(table, 'name', 'example', stat)
        call add_array(table, 'executable', children, stat)
        call add_array(children, children2, stat)

        call new_package(package, table, error=error)

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
        type(package_config_t) :: package

        call new_table(table)
        call set_value(table, 'name', 'example', stat)
        call add_array(table, 'test', children, stat)
        call add_array(children, children2, stat)

        call new_package(package, table, error=error)

    end subroutine test_package_wrongtest


    !> Try to read tests from a mixed type array
    subroutine test_package_duplicate(error)
        use fpm_manifest_package
        use fpm_toml, only : set_value, add_table, add_array, toml_table, toml_array

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        type(toml_array), pointer :: children
        integer :: stat
        type(package_config_t) :: package

        table = toml_table()
        call set_value(table, 'name', 'example', stat)
        call add_array(table, 'test', children, stat)
        call add_table(children, child, stat)
        call set_value(child, 'name', 'prog', stat)
        call add_table(children, child, stat)
        call set_value(child, 'name', 'prog', stat)

        call new_package(package, table, error=error)

    end subroutine test_package_duplicate


    !> Tests cannot be created from empty tables
    subroutine test_test_simple(error)
        use fpm_manifest_test
        use fpm_toml, only : new_table, set_value, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(test_config_t) :: test

        call new_table(table)
        call set_value(table, 'name', 'example', stat)
        call set_value(table, 'source-dir', 'tests', stat)
        call set_value(table, 'main', 'tester.f90', stat)
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
        type(test_config_t) :: test

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
        type(test_config_t) :: test

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
        type(test_config_t) :: test

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
        type(test_config_t) :: test

        call new_table(table)
        call add_table(table, 'not-supported', child, stat)

        call new_test(test, table, error)

    end subroutine test_test_wrongkey


    !> Create a simple example entry
    subroutine test_example_simple(error)
        use fpm_manifest_example
        use fpm_toml, only : new_table, set_value, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(example_config_t) :: example

        call new_table(table)
        call set_value(table, 'name', 'example', stat)
        call set_value(table, 'source-dir', 'demos', stat)
        call set_value(table, 'main', 'demo.f90', stat)
        call add_table(table, 'dependencies', child, stat)

        call new_example(example, table, error)
        if (allocated(error)) return

        call check_string(error, example%main, "demo.f90", "Example main")
        if (allocated(error)) return

    end subroutine test_example_simple


    !> Examples cannot be created from empty tables
    subroutine test_example_empty(error)
        use fpm_manifest_example
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(example_config_t) :: example

        call new_table(table)

        call new_example(example, table, error)

    end subroutine test_example_empty


    !> Test link options
    subroutine test_link_string(error)
        use fpm_manifest_build
        use fpm_toml, only : set_value, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(build_config_t) :: build

        table = toml_table()
        call set_value(table, "link", "z", stat=stat)

        call new_build_config(build, table, 'test_link_string', error)

    end subroutine test_link_string


    !> Test link options
    subroutine test_link_array(error)
        use fpm_manifest_build
        use fpm_toml, only : add_array, set_value, toml_table, toml_array

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_array), pointer :: children
        integer :: stat
        type(build_config_t) :: build

        table = toml_table()
        call add_array(table, "link", children, stat=stat)
        call set_value(children, 1, "blas", stat=stat)
        call set_value(children, 2, "lapack", stat=stat)

        call new_build_config(build, table, 'test_link_array', error)

    end subroutine test_link_array


    !> Test link options
    subroutine test_invalid_link(error)
        use fpm_manifest_build
        use fpm_toml, only : add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(build_config_t) :: build

        table = toml_table()
        call add_table(table, "link", child, stat=stat)

        call new_build_config(build, table, 'test_invalid_link', error)

    end subroutine test_invalid_link


    subroutine test_install_library(error)
        use fpm_manifest_install
        use fpm_toml, only : toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(install_config_t) :: install

        table = toml_table()
        call set_value(table, "library", .true.)

        call new_install_config(install, table, error)
        if (allocated(error)) return

        if (.not.install%library) then
            call test_failed(error, "Library entry should be true")
            return
        end if

    end subroutine test_install_library


    subroutine test_install_empty(error)
        use fpm_manifest_install
        use fpm_toml, only : toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(install_config_t) :: install

        table = toml_table()

        call new_install_config(install, table, error)
        if (allocated(error)) return

        if (install%library) then
            call test_failed(error, "Library default should be false")
            return
        end if

    end subroutine test_install_empty


    subroutine test_install_wrongkey(error)
        use fpm_manifest_install
        use fpm_toml, only : toml_table, set_value

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(install_config_t) :: install

        table = toml_table()
        call set_value(table, "prefix", "/some/install/path")

        call new_install_config(install, table, error)

    end subroutine test_install_wrongkey

    subroutine test_preprocess_empty(error)
        use fpm_manifest_preprocess
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(preprocess_config_t) :: preprocess

        call new_table(table)
        table%key = "example"

        call new_preprocess_config(preprocess, table, error)

    end subroutine test_preprocess_empty

    !> Pass a TOML table with not allowed keys
    subroutine test_preprocess_wrongkey(error)
        use fpm_manifest_preprocess
        use fpm_toml, only : new_table, add_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(preprocess_config_t) :: preprocess

        call new_table(table)
        table%key = 'example'
        call add_table(table, 'wrong-field', child, stat)

        call new_preprocess_config(preprocess, table, error)

    end subroutine test_preprocess_wrongkey

    !> Preprocess table cannot be empty.
    subroutine test_preprocessors_empty(error)
        use fpm_manifest_preprocess
        use fpm_toml, only : new_table, toml_table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(preprocess_config_t), allocatable :: preprocessors(:)

        call new_table(table)

        call new_preprocessors(preprocessors, table, error)
        if (allocated(error)) return

    end subroutine test_preprocessors_empty

    !> Test macro parsing function get_macros_from_manifest
    subroutine test_macro_parsing(error)
        use fpm_compiler, only: get_macros, compiler_enum

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file,pkg_ver
        integer :: unit
        integer(compiler_enum)  :: id

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & 'version = "0.1.0"', &
            & '[preprocess]', &
            & '[preprocess.cpp]', &
            & 'macros = ["FOO", "BAR=2", "VERSION={version}"]'
        close(unit)

        call get_package_data(package, temp_file, error)

        if (allocated(error)) return

        pkg_ver = package%version%s()

        if (get_macros(id, package%preprocess(1)%macros, pkg_ver) /= " -DFOO -DBAR=2 -DVERSION=0.1.0") then
            call test_failed(error, "Macros were not parsed correctly")
        end if

    end subroutine test_macro_parsing

    !> Test macro parsing of the package and its dependency.
    subroutine test_macro_parsing_dependency(error)
        use fpm_compiler, only: get_macros, compiler_enum

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: macros_package, macros_dependency

        type(package_config_t) :: package, dependency

        character(:), allocatable :: toml_file_package
        character(:), allocatable :: toml_file_dependency
        character(:), allocatable :: pkg_ver,dep_ver

        integer :: unit
        integer(compiler_enum)  :: id

        allocate(toml_file_package, source=get_temp_filename())
        allocate(toml_file_dependency, source=get_temp_filename())

        open(file=toml_file_package, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & 'version = "0.1.0"', &
            & '[dependencies]', &
            & '[dependencies.dependency-name]', &
            & 'git = "https://github.com/fortran-lang/dependency-name"', &
            & '[preprocess]', &
            & '[preprocess.cpp]', &
            & 'macros = ["FOO", "BAR=2", "VERSION={version}"]'
        close(unit)

        open(file=toml_file_dependency, newunit=unit)
        write(unit, '(a)') &
            & 'name = "dependency-name"', &
            & 'version = "0.2.0"', &
            & '[preprocess]', &
            & '[preprocess.cpp]', &
            & 'macros = ["FOO1", "BAR2=2", "VERSION={version}"]'
        close(unit)

        call get_package_data(package, toml_file_package, error)

        if (allocated(error)) return

        call get_package_data(dependency, toml_file_dependency, error)

        if (allocated(error)) return

        pkg_ver = package%version%s()
        dep_ver = dependency%version%s()

        macros_package = get_macros(id, package%preprocess(1)%macros, pkg_ver)
        macros_dependency = get_macros(id, dependency%preprocess(1)%macros, dep_ver)

        if (macros_package == macros_dependency) then
            call test_failed(error, "Macros of package and dependency should not be equal")
        end if

    end subroutine test_macro_parsing_dependency

end module test_manifest
