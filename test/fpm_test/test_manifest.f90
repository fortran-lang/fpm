!> Define tests for the `fpm_manifest` modules
module test_manifest
    use fpm_filesystem, only: get_temp_filename
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, check_string
    use fpm_manifest
    use fpm_manifest_profile, only: profile_config_t
    use fpm_manifest_platform, only: platform_config_t
    use fpm_compiler, only: id_gcc, id_intel_classic_nix
    use fpm_environment, only: OS_LINUX
    use fpm_manifest_feature, only: feature_config_t
    use fpm_strings, only: operator(.in.), string_t
    use fpm_error, only: fatal_error, error_t
    use tomlf, only : new_table, toml_table, toml_array
    use fpm_toml, only : add_table, add_array, get_value, get_list, set_value, set_list
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
            & new_unittest("default-library-type", test_default_library_type), &
            & new_unittest("default-executable", test_default_executable), &
            & new_unittest("dependency-empty", test_dependency_empty, should_fail=.true.), &
            & new_unittest("dependency-pathtag", test_dependency_pathtag, should_fail=.true.), &
            & new_unittest("dependency-gitpath", test_dependency_gitpath, should_fail=.true.), &
            & new_unittest("dependency-nourl", test_dependency_nourl, should_fail=.true.), &
            & new_unittest("dependency-gitconflict", test_dependency_gitconflict, should_fail=.true.), &
            & new_unittest("dependency-invalid-git", test_dependency_invalid_git, should_fail=.true.), &
            & new_unittest("dependency-no-namespace", test_dependency_no_namespace, should_fail=.true.), &
            & new_unittest("dependency-redundant-v", test_dependency_redundant_v, should_fail=.true.), &
            & new_unittest("dependency-features-present", test_dependency_features_present), &
            & new_unittest("dependency-features-absent",  test_dependency_features_absent),  &
            & new_unittest("dependency-features-empty",   test_dependency_features_empty),   &
            & new_unittest("dependency-profile-present",  test_dependency_profile_present),  &
            & new_unittest("dependency-profile-absent",   test_dependency_profile_absent),   &
            & new_unittest("dependency-profile-features-conflict", &
            &              test_dependency_profile_features_conflict, should_fail=.true.), &
            & new_unittest("dependency-wrongkey", test_dependency_wrongkey, should_fail=.true.), &
            & new_unittest("dependencies-empty", test_dependencies_empty), &
            & new_unittest("dependencies-typeerror", test_dependencies_typeerror, should_fail=.true.), &
            & new_unittest("profiles", test_profiles), &
            & new_unittest("profiles-invalid", test_profiles_invalid, should_fail=.true.), &
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
            & new_unittest("library-list", test_library_list, should_fail=.true.), &
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
            & new_unittest("install-module-dir", test_install_module_dir), &
            & new_unittest("install-wrongkey", test_install_wrongkey, should_fail=.true.), &
            & new_unittest("preprocess-empty", test_preprocess_empty), &
            & new_unittest("preprocess-wrongkey", test_preprocess_wrongkey, should_fail=.true.), &
            & new_unittest("preprocessors-empty", test_preprocessors_empty, should_fail=.true.), &
            & new_unittest("macro-parsing", test_macro_parsing, should_fail=.false.), &
            & new_unittest("macro-parsing-dependency", &
            &              test_macro_parsing_dependency, should_fail=.false.), &
            & new_unittest("features-demo-serialization", test_features_demo_serialization) &
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

        ! Test package serialization
        call package%test_serialization('test_valid_manifest',error)
        if (allocated(error)) return

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

        call package%test_serialization('test_default_library',error)
        if (allocated(error)) return

    end subroutine test_default_library


    !> Test that a package with non-specified library returns monolithic and not shared/static
    subroutine test_default_library_type(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(len=*), parameter :: manifest = 'fpm-default-library-type.toml'
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[library]'
        close(unit)

        call get_package_data(package, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')

        if (allocated(error)) return

        if (.not.allocated(package%library)) then
            call test_failed(error, "Default library is not present in package data")
            return
        end if

        if (.not.package%library%monolithic()) then
            call test_failed(error, "Default library should be monolithic")
            return
        end if

        if (package%library%shared()) then
            call test_failed(error, "Default library should not be shared")
            return
        end if

        if (package%library%static()) then
            call test_failed(error, "Default library should not be static")
            return
        end if

        call package%test_serialization('test_default_library_type',error)
        if (allocated(error)) return

    end subroutine test_default_library_type


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
        
        call package%feature_config_t%test_serialization('test_default_executable (feature only)',error)
        if (allocated(error)) return

        call package%test_serialization('test_default_executable',error)
        if (allocated(error)) return

    end subroutine test_default_executable
    
    !> Dependencies cannot be created from empty tables
    subroutine test_dependency_empty(error)
        use fpm_manifest_dependency

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

    !> Test profile parsing and storage in package
    subroutine test_profiles(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(len=*), parameter :: manifest = 'fpm-profiles.toml'
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[profiles]', &
            & 'development = ["debug", "testing"]', &
            & 'release = ["optimized"]', &
            & 'full-test = ["debug", "testing", "benchmarks"]', &
            & '[features]', &
            & 'testing.flags = " -g"', &            
            & 'optimized.flags = " -O2"', &            
            & 'benchmarks.flags = " -O3"'
        close(unit)

        call get_package_data(package, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')

        if (allocated(error)) return

        ! Check that profiles were parsed correctly
        if (.not. allocated(package%profiles)) then
            call test_failed(error, "No profiles found in package")
            return
        end if

        ! debug, release, development, full-test
        if (size(package%profiles) /= 4) then
            call test_failed(error, "Unexpected number of profiles, should be 4")
            return
        end if

        ! Check development profile
        if (package%profiles(1)%name /= "development") then
            call test_failed(error, "Expected profile name 'development', got '" // package%profiles(1)%name // "'")
            return
        end if

        if (size(package%profiles(1)%features) /= 2) then
            call test_failed(error, "Unexpected number of features, should be 2")
            return
        end if

        if (package%profiles(1)%features(1)%s /= "debug" .or. &
            package%profiles(1)%features(2)%s /= "testing") then
            call test_failed(error, "Incorrect features in development profile")
            return
        end if

    end subroutine test_profiles

    !> Test invalid profile configuration should fail
    subroutine test_profiles_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(len=*), parameter :: manifest = 'fpm-profiles-error.toml'
        integer :: unit

        open(file=manifest, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[profiles]', &
            & 'development = "not_an_array"'
        close(unit)

        call get_package_data(package, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')
    end subroutine test_profiles_invalid

    !> Executables cannot be created from empty tables
    subroutine test_executable_empty(error)
        use fpm_manifest_executable

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

   !> Pass a TOML table with not allowed source dirs
    subroutine test_library_list(error)
        use fpm_manifest_library

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(string_t), allocatable :: source_dirs(:)
        type(toml_table) :: table
        type(library_config_t) :: library

        source_dirs = [string_t("src1"),string_t("src2")]
        call new_table  (table)
        call set_list   (table, "source-dir", source_dirs, error)
        call new_library(library, table, error)

    end subroutine test_library_list

    !> Pass a TOML table with a 1-sized source dir list
    subroutine test_library_listone(error)
        use fpm_manifest_library

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[library]', &
            & 'source-dir = ["my-src"]'
        close(unit)

        call get_package_data(package, temp_file, error)

    end subroutine test_library_listone

    !> Packages cannot be created from empty tables
    subroutine test_package_simple(error)
        use fpm_manifest_package

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

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        integer :: stat
        type(build_config_t) :: build

        table = toml_table()
        call set_value(table, "link", "z", stat=stat)

        call new_build_config(build, table, 'test_link_string', error)
        if (allocated(error)) return

        !> Test serialization roundtrip
        call build%test_serialization('test_link_string', error)
        if (allocated(error)) return

    end subroutine test_link_string


    !> Test link options
    subroutine test_link_array(error)
        use fpm_manifest_build

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
        if (allocated(error)) return

        !> Test serialization roundtrip
        call build%test_serialization('test_link_array', error)
        if (allocated(error)) return

    end subroutine test_link_array


    !> Test link options
    subroutine test_invalid_link(error)
        use fpm_manifest_build

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

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(install_config_t) :: install

        table = toml_table()
        call set_value(table, "prefix", "/some/install/path")

        call new_install_config(install, table, error)

    end subroutine test_install_wrongkey


    subroutine test_install_module_dir(error)
        use fpm_manifest_install

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(install_config_t) :: install

        table = toml_table()
        call set_value(table, "module-dir", "custom_modules")

        call new_install_config(install, table, error)
        if (allocated(error)) return

        if (.not.allocated(install%module_dir)) then
            call test_failed(error, "Module directory should be allocated")
            return
        end if

        if (install%module_dir /= "custom_modules") then
            call test_failed(error, "Module directory should match input")
            return
        end if

    end subroutine test_install_module_dir

    subroutine test_preprocess_empty(error)
        use fpm_manifest_preprocess

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(preprocess_config_t) :: preprocess

        call new_table(table)
        table%key = "example"

        call preprocess%new(table, error)

    end subroutine test_preprocess_empty

    !> Pass a TOML table with not allowed keys
    subroutine test_preprocess_wrongkey(error)
        use fpm_manifest_preprocess

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: child
        integer :: stat
        type(preprocess_config_t) :: preprocess

        call new_table(table)
        table%key = 'example'
        call add_table(table, 'wrong-field', child, stat)

        call preprocess%new(table, error)

    end subroutine test_preprocess_wrongkey

    !> Preprocess table cannot be empty.
    subroutine test_preprocessors_empty(error)
        use fpm_manifest_preprocess

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
        use fpm_compiler, only: get_macros, compiler_enum, id_gcc

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        integer(compiler_enum)  :: id
        
        id = id_gcc

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

        if (get_macros(id, package%preprocess(1)%macros, package%version) /= " -DFOO -DBAR=2 -DVERSION=0.1.0") then
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

        macros_package = get_macros(id, package%preprocess(1)%macros, package%version)
        macros_dependency = get_macros(id, dependency%preprocess(1)%macros, dependency%version)
        if (macros_package == macros_dependency) then
            call test_failed(error, "Macros of package and dependency should not be equal")
        end if

    end subroutine test_macro_parsing_dependency

    !> Ensure dependency "features" array is correctly parsed when present
    subroutine test_dependency_features_present(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit, i, idx_dep0, idx_dep1, idx_dep2

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & 'version = "0.1.0"', &
            & '[dependencies]', &
            & '"dep0" = { path = "local/dep0", features = ["featA", "featB"] }', &
            & '"dep1" = { git = "https://example.com/repo.git", tag = "v1.2.3", features = ["only"] }', &
            & '"dep2" = { path = "other/dep2" }'
        close(unit)

        call get_package_data(package, temp_file, error)
        if (allocated(error)) return

        if (.not.allocated(package%dependency)) then
            call test_failed(error, 'No dependencies parsed from manifest')
            return
        end if

        idx_dep0 = 0; idx_dep1 = 0; idx_dep2 = 0
        do i = 1, size(package%dependency)
            select case (package%dependency(i)%name)
            case ('dep0'); idx_dep0 = i
            case ('dep1'); idx_dep1 = i
            case ('dep2'); idx_dep2 = i
            end select
        end do

        if (idx_dep0 == 0 .or. idx_dep1 == 0 .or. idx_dep2 == 0) then
            call test_failed(error, 'Expected dependencies dep0/dep1/dep2 not found')
            return
        end if

        ! dep0: features = ["featA","featB"]
        if (.not.allocated(package%dependency(idx_dep0)%features)) then
            call test_failed(error, 'dep0 features not allocated')
            return
        end if
        if (size(package%dependency(idx_dep0)%features) /= 2) then
            call test_failed(error, 'dep0 features size /= 2')
            return
        end if
        if (package%dependency(idx_dep0)%features(1)%s /= 'featA' .or. &
            & package%dependency(idx_dep0)%features(2)%s /= 'featB') then
            call test_failed(error, 'dep0 features values mismatch')
            return
        end if

        ! dep1: features = ["only"]
        if (.not.allocated(package%dependency(idx_dep1)%features)) then
            call test_failed(error, 'dep1 features not allocated')
            return
        end if
        if (size(package%dependency(idx_dep1)%features) /= 1) then
            call test_failed(error, 'dep1 features size /= 1')
            return
        end if
        if (package%dependency(idx_dep1)%features(1)%s /= 'only') then
            call test_failed(error, 'dep1 features value mismatch')
            return
        end if

        ! dep2: no features key -> should be NOT allocated
        if (allocated(package%dependency(idx_dep2)%features)) then
            call test_failed(error, 'dep2 features should be unallocated when key is absent')
            return
        end if
    end subroutine test_dependency_features_present


    !> Ensure a dependency without "features" key is accepted (no allocation)
    subroutine test_dependency_features_absent(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit, i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[dependencies]', &
            & '"a" = { path = "a" }', &
            & '"b" = { git = "https://example.org/b.git", branch = "main" }'
        close(unit)

        call get_package_data(package, temp_file, error)
        if (allocated(error)) return

        if (.not.allocated(package%dependency)) then
            call test_failed(error, 'No dependencies parsed from manifest')
            return
        end if

        do i = 1, size(package%dependency)
            if (allocated(package%dependency(i)%features)) then
                call test_failed(error, 'features should be unallocated when not specified')
                return
            end if
        end do
    end subroutine test_dependency_features_absent


    !> Accept an explicit empty "features = []" list
    subroutine test_dependency_features_empty(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit, i, idx

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[dependencies]', &
            & '"empty" = { path = "local/empty", features = [] }'
        close(unit)

        call get_package_data(package, temp_file, error)
        if (allocated(error)) return

        idx = -1
        if (.not.allocated(package%dependency)) then
            call test_failed(error, 'No dependencies parsed from manifest')
            return
        end if

        do i = 1, size(package%dependency)
            if (package%dependency(i)%name == 'empty') then
                idx = i
                exit
            end if
        end do

        if (idx < 1) then
            call test_failed(error, 'Dependency "empty" not found')
            return
        end if

        if (.not.allocated(package%dependency(idx)%features)) then
            call test_failed(error, 'features should be allocated (size=0) for empty list')
            return
        end if
        if (size(package%dependency(idx)%features) /= 0) then
            call test_failed(error, 'features size should be zero for empty list')
            return
        end if
    end subroutine test_dependency_features_empty

    !> Test features demo manifest serialization (from example_packages/features_demo/fpm.toml)
    subroutine test_features_demo_serialization(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "features_demo"', &
            & 'version = "0.1.0"', &
            & 'license = "MIT"', &
            & 'description = "Demo package for FPM features functionality"', &
            & '', &
            & '[[executable]]', &
            & 'name = "features_demo"', &
            & 'source-dir = "app"', &
            & 'main = "main.f90"', &
            & '', &
            & '[features]', &
            & '# Base debug feature', &
            & 'debug.flags = "-g"', &
            & 'debug.preprocess.cpp.macros = "DEBUG"', &
            & '', &
            & '# Release feature', &
            & 'release.flags = "-O3"', &
            & 'release.preprocess.cpp.macros = "RELEASE"', &
            & '', &
            & '# Compiler-specific features', &
            & 'debug.gfortran.flags = "-Wall -fcheck=bounds"', &
            & 'release.gfortran.flags = "-mtune=generic -funroll-loops"', &
            & '', &
            & '# Platform-specific features', &
            & 'linux.preprocess.cpp.macros = "LINUX_BUILD"', &
            & '', &
            & '# Parallel features', &
            & 'mpi.preprocess.cpp.macros = "USE_MPI"', &
            & 'mpi.dependencies.mpi = "*"', &
            & 'openmp.preprocess.cpp.macros = "USE_OPENMP"', &
            & 'openmp.dependencies.openmp = "*"', &
            & '', &
            & '[profiles]', &
            & 'development = ["debug"]', &
            & 'production = ["release", "openmp"]'
        close(unit)

        call get_package_data(package, temp_file, error)
        if (allocated(error)) return

        ! Verify basic package structure
        if (package%name /= "features_demo") then
            call test_failed(error, "Package name should be 'features_demo'")
            return
        end if

        if (.not. allocated(package%features)) then
            call test_failed(error, "Features should be allocated")
            return
        end if

        if (.not. allocated(package%profiles)) then
            call test_failed(error, "Profiles should be allocated") 
            return
        end if

        if (.not. allocated(package%executable)) then
            call test_failed(error, "Executables should be allocated")
            return
        end if

        ! Test package serialization roundtrip
        call package%test_serialization('test_features_demo_serialization', error)
        if (allocated(error)) return

    end subroutine test_features_demo_serialization


    !> Test that a dependency with "profile" key is parsed correctly
    subroutine test_dependency_profile_present(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit, i, idx_dep0, idx_dep1

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & 'version = "0.1.0"', &
            & '[dependencies]', &
            & '"dep0" = { path = "local/dep0", profile = "release" }', &
            & '"dep1" = { path = "local/dep1" }'
        close(unit)

        call get_package_data(package, temp_file, error)
        if (allocated(error)) return

        if (.not.allocated(package%dependency)) then
            call test_failed(error, 'No dependencies parsed from manifest')
            return
        end if

        idx_dep0 = 0; idx_dep1 = 0
        do i = 1, size(package%dependency)
            select case (package%dependency(i)%name)
            case ('dep0'); idx_dep0 = i
            case ('dep1'); idx_dep1 = i
            end select
        end do

        if (idx_dep0 == 0 .or. idx_dep1 == 0) then
            call test_failed(error, 'Expected dependencies dep0/dep1 not found')
            return
        end if

        ! dep0: profile = "release"
        if (.not.allocated(package%dependency(idx_dep0)%profile)) then
            call test_failed(error, 'dep0 profile not allocated')
            return
        end if
        if (package%dependency(idx_dep0)%profile /= 'release') then
            call test_failed(error, 'dep0 profile value mismatch, expected "release"')
            return
        end if
        ! dep0 should NOT have features allocated
        if (allocated(package%dependency(idx_dep0)%features)) then
            if (size(package%dependency(idx_dep0)%features) > 0) then
                call test_failed(error, 'dep0 features should not be present when profile is used')
                return
            end if
        end if

        ! dep1: no profile key -> should be NOT allocated
        if (allocated(package%dependency(idx_dep1)%profile)) then
            call test_failed(error, 'dep1 profile should be unallocated when key is absent')
            return
        end if
    end subroutine test_dependency_profile_present


    !> Ensure a dependency without "profile" key leaves it unallocated
    subroutine test_dependency_profile_absent(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit, i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[dependencies]', &
            & '"a" = { path = "a" }', &
            & '"b" = { git = "https://example.org/b.git", branch = "main" }'
        close(unit)

        call get_package_data(package, temp_file, error)
        if (allocated(error)) return

        if (.not.allocated(package%dependency)) then
            call test_failed(error, 'No dependencies parsed from manifest')
            return
        end if

        do i = 1, size(package%dependency)
            if (allocated(package%dependency(i)%profile)) then
                call test_failed(error, 'profile should be unallocated when not specified')
                return
            end if
        end do
    end subroutine test_dependency_profile_absent


    !> Specifying both "features" and "profile" should be an error
    subroutine test_dependency_profile_features_conflict(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example"', &
            & '[dependencies]', &
            & '"bad" = { path = "bad", features = ["f1"], profile = "release" }'
        close(unit)

        call get_package_data(package, temp_file, error)

    end subroutine test_dependency_profile_features_conflict


end module test_manifest
