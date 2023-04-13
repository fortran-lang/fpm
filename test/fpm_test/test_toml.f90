!> Define tests for the `fpm_toml` modules
module test_toml
    use testsuite, only : new_unittest, unittest_t, error_t
    use fpm_toml
    use tomlf_constants, only: tf_i8
    use fpm_git
    use fpm_dependency, only: dependency_node_t, destroy_dependency_node, dependency_tree_t, &
         & new_dependency_node, new_dependency_tree, resize
    use fpm_manifest_dependency, only: dependency_config_t, dependency_destroy
    use fpm_versioning, only: new_version
    use fpm_strings, only: string_t, operator(==), split
    use fpm_model, only: fortran_features_t, package_t, FPM_SCOPE_LIB, FPM_UNIT_MODULE, fpm_model_t, &
         & srcfile_t
    use fpm_compiler, only: archiver_t, compiler_t, id_gcc

    implicit none
    private

    public :: collect_toml

    character, parameter :: NL = new_line('a')

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
           & new_unittest("serialize-git-invalid", git_target_invalid, should_fail=.true.), &
           & new_unittest("serialize-dependency-config", dependency_config_roundtrip), &
           & new_unittest("serialize-dependency-node", dependency_node_roundtrip), &
           & new_unittest("serialize-dependency-invalid", dependency_node_invalid, should_fail=.true.), &
           & new_unittest("serialize-dependency-invalid2", dependency_node_invalid_2, should_fail=.true.), &
           & new_unittest("serialize-dependency-tree", dependency_tree_roundtrip), &
           & new_unittest("serialize-dependency-tree-invalid", dependency_tree_invalid, should_fail=.true.), &
           & new_unittest("serialize-dependency-tree-invalid2", dependency_tree_invalid2, should_fail=.true.), &
           & new_unittest("serialize-string-array", string_array_roundtrip), &
           & new_unittest("serialize-fortran-features", fft_roundtrip), &
           & new_unittest("serialize-fortran-invalid", fft_invalid, should_fail=.true.), &
           & new_unittest("serialize-package", package_roundtrip), &
           & new_unittest("serialize-package-invalid", package_invalid, should_fail=.true.), &
           & new_unittest("serialize-srcfile-invalid", source_invalid, should_fail=.true.), &
           & new_unittest("serialize-archiver", ar_roundtrip), &
           & new_unittest("serialize-archiver-invalid", ar_invalid, should_fail=.true.), &
           & new_unittest("serialize-compiler", compiler_roundtrip), &
           & new_unittest("serialize-compiler-invalid", compiler_invalid, should_fail=.true.), &
           & new_unittest("serialize-model", fpm_model_roundtrip), &
           & new_unittest("serialize-model-invalid", fpm_model_invalid, should_fail=.true.)]

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


    !> Test invalid git_target_t serialization
    subroutine git_target_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(git_target_t) :: git
        type(toml_table), allocatable :: table

        character(*), parameter :: toml = 'descriptor = ""'//NL//& ! invalid descriptor ID
                                          'url = "https://github.com/toml-f/toml-f"'//NL//&
                                          'object = "54686e45993f3a9a1d05d5c7419f39e7d5a4eb3f"'


        call string_to_toml(toml, table)

        call git%load(table, error)

    end subroutine git_target_invalid

    !> Test git_target_t serialization
    subroutine dependency_config_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

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

    !> Test loading invalid dependency node
    subroutine dependency_node_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(dependency_node_t) :: dep
        type(toml_table), allocatable :: table

        character(*), parameter :: toml = 'name = "jonquil" '//NL//&
                                        & 'version = "h0.2.0"'//NL//& ! invalid version
                                        & 'proj-dir = "build/dependencies/jonquil"'//NL//&
                                        & 'revision = "05d30818bb12fb877226ce284b9a3a41b971a889"'//NL//&
                                        & 'done = true'//NL//&
                                        & 'update = false'//NL//&
                                        & 'cached = true'

        call string_to_toml(toml, table)

        call dep%load(table, error)

    end subroutine dependency_node_invalid

    !> Test loading invalid dependency node
    subroutine dependency_node_invalid_2(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(dependency_node_t) :: dep
        type(toml_table), allocatable :: table

        character(*), parameter :: toml = 'name = "jonquil" '//NL//&
                                        & 'version = "0.2.0"'//NL//&
                                        & 'proj-dir = "build/dependencies/jonquil"'//NL//&
                                        & 'revision = "05d30818bb12fb877226ce284b9a3a41b971a889"'//NL//&
                                        & 'done = 123'//NL//&  ! not a boolean
                                        & 'update = false'//NL//&
                                        & 'cached = true'

        call string_to_toml(toml, table)
        call dep%load(table, error)

    end subroutine dependency_node_invalid_2

    !> Test dependency_tree_t serialization
    subroutine dependency_tree_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(dependency_tree_t) :: deps
        type(dependency_config_t) :: dep

        integer, parameter :: ALLOCATED_DEPS = 5
        character(36) :: msg
        integer :: ii

        ! Generate dummy tree with ndep=3 but 5 allocated dependencies
        call new_dependency_tree(deps)
        call resize(deps%dep, ALLOCATED_DEPS)
        deps%ndep = 3
        dep%name = "dep1"
        dep%path = "fpm-tmp1-dir"
        call new_dependency_node(deps%dep(1), dep, proj_dir=dep%path)
        dep%name = "dep2"
        dep%path = "fpm-tmp2-dir"
        call new_dependency_node(deps%dep(2), dep, proj_dir=dep%path)
        deps%dep(3)%name = "M_CLI2"
        deps%dep(3)%path = "~/./some/dummy/path"
        deps%dep(3)%proj_dir = "~/./"
        deps%dep(3)%namespace = "urbanjost"
        deps%dep(3)%revision = "7264878cdb1baff7323cc48596d829ccfe7751b8"
        deps%dep(3)%cached = .true.
        deps%dep(3)%done = .false.
        deps%dep(3)%update = .true.
        allocate(deps%dep(3)%requested_version)
        call new_version(deps%dep(3)%requested_version, "3.2.0",error); if (allocated(error)) return
        allocate(deps%dep(3)%version)
        call new_version(deps%dep(3)%version, "4.53.2",error); if (allocated(error)) return
        allocate(deps%dep(3)%git)
        deps%dep(3)%git = git_target_revision(url="https://github.com/urbanjost/M_CLI2.git", &
                                              sha1="7264878cdb1baff7323cc48596d829ccfe7751b8")

        call deps%test_serialization("full dependency tree", error)
        if (allocated(error)) then
            print *, error%message
            stop 'catastrophic'
        end if

        ! Remove dependencies (including all them)
        do ii = 1, ALLOCATED_DEPS
            write(msg,1) ii
            call resize(deps%dep, size(deps%dep) - 1)
            call deps%test_serialization(trim(msg), error)
            if (allocated(error)) return
        end do

        ! deallocate dependencies
        deallocate(deps%dep)
        call deps%test_serialization("unallocated deps(:)", error)
        if (allocated(error)) return

        ! Remove deps dir
        deallocate(deps%dep_dir)
        call deps%test_serialization("no deps dir", error)
        if (allocated(error)) return

        1 format('removed ',i0,' dependencies')

    end subroutine dependency_tree_roundtrip

    !> Test invalid dependency tree loading
    subroutine dependency_tree_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table
        type(dependency_tree_t) :: dep

        character(len=*), parameter :: toml = &
            & 'unit = 6 '//NL//&
            & 'verbosity = true'//NL//& ! not a number
            & 'dep-dir = "build/dependencies"'//NL//&
            & 'ndep = 3'//NL//& ! consistency is not checked:
            & '[dependencies]'//NL//&
            & '[dependencies.dep1]'//NL//&
            & 'name = "dep1"'//NL//&
            & 'path = "fpm-tmp1-dir"'//NL//&
            & 'proj-dir = "fpm-tmp1-dir"'//NL//&
            & 'done = false'//NL//&
            & 'update = false'//NL//&
            & 'cached = false'

        call string_to_toml(toml, table)
        call dep%load(table, error)

    end subroutine dependency_tree_invalid

    !> Test invalid dependency tree loading
    subroutine dependency_tree_invalid2(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table
        type(dependency_tree_t) :: dep

        character(len=*), parameter :: toml = &
            & 'unit = "" '//NL//& ! not provided
            & 'verbosity = 1'//NL//&
            & 'dep-dir = "build/dependencies"'//NL//&
            & 'ndep = 3'//NL//& ! consistency is not checked:
            & '[dependencies.M_CLI2]'//NL//&
            & 'name = "M_CLI2"'//NL//&
            & 'path = "~/./some/dummy/path"'//NL//&
            & 'namespace = "urbanjost"'//NL//&
            & 'requested_version = "3.2.0"'//NL//&
            & 'version = "4.53.2"'//NL//&
            & 'proj-dir = "~/./"'//NL//&
            & 'revision = "7264878cdb1baff7323cc48596d829ccfe7751b8"'//NL//&
            & 'done = false'//NL//&
            & 'update = true'//NL//&
            & 'cached = true'//NL//&
            & '[dependencies.M_CLI2.git]'//NL//&
            & 'descriptor = "revision"'//NL//&
            & 'url = "https://github.com/urbanjost/M_CLI2.git"'//NL//&
            & 'object = "7264878cdb1baff7323cc48596d829ccfe7751b8"'

        call string_to_toml(toml, table)
        call dep%load(table, error)

    end subroutine dependency_tree_invalid2

    !> Test serialization/deserialization of a string array
    subroutine string_array_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=*), parameter :: lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing " &
            & //"elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad " &
            & //"minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo " &
            & //"consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum " &
            & //"dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt " &
            & //"in culpa qui officia deserunt mollit anim id est laborum"

        integer :: ii, nword
        character(len=:), allocatable :: tokens(:)
        type(string_t), allocatable :: list(:),copy(:)
        type(toml_table) :: table
        character(len=16) :: key

        call split(lorem, tokens)
        nword = size(tokens)

        !> Convert to string_t array
        allocate(list(nword))
        do ii = 1, nword
            list(ii) = string_t(trim(tokens(ii)))
        end do

        ! Test list with any length
        do ii = nword, 1, -1

            ! Shorten list
            list = list(1:ii)

            ! Set list to table
            table = toml_table()

            call set_list(table, key="lorem-ipsum", list=list, error=error)
            if (allocated(error)) return

            ! Load list from table
            call get_list(table, key="lorem-ipsum", list=copy, error=error)
            if (allocated(error)) return

            if (.not.(list==copy)) then
               call fatal_error(error,'string_array is not equal after TOML roundtrip')
               return
            end if

        end do

        ! Test empty list
        deallocate(list)
        allocate(list(0))
        ! Set list to table
        table = toml_table()

        call set_list(table, key="lorem-ipsum", list=list, error=error)
        if (allocated(error)) return

        ! Load list from table
        call get_list(table, key="lorem-ipsum", list=copy, error=error)
        if (allocated(error)) return

        if (.not.(list==copy)) then
           call fatal_error(error,'empty string_array is not equal after TOML roundtrip')
           return
        end if

        ! Test unallocated list
        deallocate(list)
        table = toml_table()

        call set_list(table, key="lorem-ipsum", list=list, error=error)
        if (allocated(error)) return

        ! Load list from table
        call get_list(table, key="lorem-ipsum", list=copy, error=error)
        if (allocated(error)) return

        if (.not.(list==copy)) then
           call fatal_error(error,'deallocated string_array is not equal after TOML roundtrip')
           return
        end if

        1 format('word_',i0)

    end subroutine string_array_roundtrip

    !> Test serialization/deserialization of a fortran-features structure
    subroutine fft_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(fortran_features_t) :: fortran

        !> Default object
        call fortran%test_serialization('fortran_features_t: default object',error)
        if (allocated(error)) return

        !> Set form
        fortran%source_form = "free"
        call fortran%test_serialization('fortran_features_t: with form',error)
        if (allocated(error)) return

    end subroutine fft_roundtrip

    !> Test deserialization of an invalid fortran-features structure
    subroutine fft_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(fortran_features_t) :: fortran
        type(toml_table), allocatable :: table

        character(len=*), parameter :: toml = 'implicit-typing = false '//NL//&
                                            & 'implicit-external = 0 '//NL//& ! not a boolean
                                            & 'source-form = "free" '

        call string_to_toml(toml, table)

        !> Default object
        call fortran%load(table,error)

    end subroutine fft_invalid

    !> Test serialization/deserialization of a package_t structure
    subroutine package_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_t) :: pkg
        integer :: ierr

        !> Default object
        call pkg%test_serialization('package_t: default object',error)
        if (allocated(error)) return

        !> Create a dummy package
        pkg%name = "orderpack"
        pkg%version = "0.1.0"
        pkg%enforce_module_names = .false.
        pkg%module_prefix = string_t("")
        pkg%features%source_form = "free"

        if (allocated(pkg%sources)) deallocate(pkg%sources)
        allocate(pkg%sources(4))

        pkg%sources(1)%file_name = "build/dependencies/orderpack/src/M_valnth.f90"
        pkg%sources(1)%digest = 2662523002405134329_tf_i8
        pkg%sources(1)%unit_scope = FPM_SCOPE_LIB
        pkg%sources(1)%unit_type = FPM_UNIT_MODULE
        pkg%sources(1)%modules_provided = [string_t("m_valnth")]
        deallocate(pkg%sources(1)%parent_modules, stat=ierr)
        deallocate(pkg%sources(1)%modules_used, stat=ierr)
        deallocate(pkg%sources(1)%include_dependencies, stat=ierr)
        deallocate(pkg%sources(1)%link_libraries, stat=ierr)

        pkg%sources(2)%file_name = "build/dependencies/orderpack/src/M_mrgrnk.f90"
        pkg%sources(2)%digest = 7985690966656622651_tf_i8
        pkg%sources(2)%unit_scope = FPM_SCOPE_LIB
        pkg%sources(2)%unit_type = FPM_UNIT_MODULE
        pkg%sources(2)%modules_provided = [string_t("m_mrgrnk")]
        pkg%sources(2)%link_libraries = [string_t("netcdf"),string_t("hdf-5")]
        deallocate(pkg%sources(2)%parent_modules, stat=ierr)
        deallocate(pkg%sources(2)%modules_used, stat=ierr)
        deallocate(pkg%sources(2)%include_dependencies, stat=ierr)
        deallocate(pkg%sources(2)%link_libraries, stat=ierr)

        pkg%sources(3)%file_name = "build/dependencies/orderpack/src/M_median.f90"
        pkg%sources(3)%digest = 7985690966656622651_tf_i8
        pkg%sources(3)%unit_scope = FPM_SCOPE_LIB
        pkg%sources(3)%unit_type = FPM_UNIT_MODULE
        pkg%sources(3)%modules_provided = [string_t("m_median")]
        deallocate(pkg%sources(3)%parent_modules, stat=ierr)
        deallocate(pkg%sources(3)%modules_used, stat=ierr)
        deallocate(pkg%sources(3)%include_dependencies, stat=ierr)
        deallocate(pkg%sources(3)%link_libraries, stat=ierr)

        pkg%sources(4)%file_name = "build/dependencies/orderpack/src/M_unista.f90"
        pkg%sources(4)%digest = -7512253540457404792_tf_i8
        pkg%sources(4)%unit_scope = FPM_SCOPE_LIB
        pkg%sources(4)%unit_type = FPM_UNIT_MODULE
        pkg%sources(4)%modules_provided = [string_t("m_unista")]
        pkg%sources(4)%modules_used = [string_t("m_uniinv")]
        deallocate(pkg%sources(4)%parent_modules, stat=ierr)
        deallocate(pkg%sources(4)%include_dependencies, stat=ierr)
        deallocate(pkg%sources(4)%link_libraries, stat=ierr)

        !> Package mock
        call pkg%test_serialization('package_t: orderpack',error)
        if (allocated(error)) return

        !> Remove some entries
        pkg%sources(1)%file_name = ""
        pkg%sources(3)%digest = 0
        pkg%sources = pkg%sources(1:3)
        call pkg%test_serialization('package_t: orderpack (reduced)',error)
        if (allocated(error)) return

        !> Remove all sources
        deallocate(pkg%sources,stat=ierr)
        call pkg%test_serialization('package_t: no sources',error)
        if (allocated(error)) return

    end subroutine package_roundtrip

    !> Test deserialization of an invalid package TOML file
    subroutine package_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=*), parameter :: toml = &
            & 'name = "toml-f"  '//NL//&
            & 'version = "0.8.0"  '//NL//&
            & 'module-naming = "prefix"  '//NL//& ! this should be boolean
            & 'module-prefix = ""  '

        type(package_t) :: pkg
        type(toml_table), allocatable :: table

        call string_to_toml(toml, table)

        !> Default object
        call pkg%load(table,error)

    end subroutine package_invalid

    !> Test deserialization of an invalid source file
    subroutine source_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=*), parameter :: toml = &
            & 'file-name = "build/dependencies/toml-f/src/tomlf.f90"  '//NL//&
            & 'digest = "abcde"  '//NL//& ! not a number
            & 'unit-scope = "FPM_SCOPE_MODULE"  '//NL//&
            & 'unit-type = "FPM_UNIT_MODULE"  '//NL//&
            & 'modules-provided = "tomlf"  '//NL//&
            & 'parent-modules = [ ]  '//NL//&
            & 'modules-used = [ "tomlf_build", "tomlf_datetime" ]  '//NL//&
            & 'include-dependencies = [ ]  '//NL//&
            & 'link-libraries = [ ]  '

        type(srcfile_t) :: src
        type(toml_table), allocatable :: table

        call string_to_toml(toml, table)

        !> Default object
        call src%load(table,error)

    end subroutine source_invalid

    !> Test serialization/deserialization of an archiver_t structure
    subroutine ar_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(archiver_t) :: ar
        integer :: ierr

        !> Default object
        call ar%test_serialization('archiver_t: default object',error)
        if (allocated(error)) return

        !> change a few items
        ar%ar = "ar"
        ar%echo = .true.
        ar%use_response_file = .false.

        call ar%test_serialization('archiver_t: ar',error)

    end subroutine ar_roundtrip

    !> Test deserialization of an invalid archiver_t structure
    subroutine ar_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=*), parameter :: toml = &
            & 'ar = "ar -rs "    '//NL//&
            & 'use-response-file = false    '//NL//&
            & 'echo = 123     '//NL//& ! not a boolean
            & 'verbose = false   '

        type(archiver_t) :: ar
        type(toml_table), allocatable :: table

        call string_to_toml(toml, table)

        !> Default object
        call ar%load(table,error)

    end subroutine ar_invalid

    !> Test serialization/deserialization of a compiler_t structure
    subroutine compiler_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(compiler_t) :: compiler

        !> Default object
        call compiler%test_serialization('compiler_t: default object',error)
        if (allocated(error)) return

        !> change a few items
        compiler%id = id_gcc
        compiler%fc = "gfortran -ffree-line-length-none -fdefault-real-8 -O3"
        compiler%cc = ""
        compiler%cxx = "g++ -O3 -std=c++11"
        compiler%echo = .false.

        call compiler%test_serialization('compiler_t: gcc',error)

    end subroutine compiler_roundtrip

    !> Test deserialization of an invalid compiler_t TOML structure
    subroutine compiler_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=*), parameter :: toml = &
            & 'id = "gfortran"    '//NL//& ! not an integer identifier
            & 'fc = "gfortran"    '//NL//&
            & 'cc = "gcc"    '//NL//&
            & 'cxx = "g++"     '//NL//&
            & 'echo = false    '//NL//&
            & 'verbose = false    '

        type(compiler_t) :: cc
        type(toml_table), allocatable :: table

        call string_to_toml(toml, table)

        !> Default object
        call cc%load(table,error)

    end subroutine compiler_invalid

    !> Get a simplified TOML representation of the fpm v0.8.1 model
    subroutine fpm_081_table(table)

        !> TOML representation of the fpm v0.8.1 model
        type(toml_table), allocatable, intent(out) :: table

        !> simplified TOML representation of the fpm v0.8.1 model
        character(len=:), allocatable :: fpm

        integer :: iunit

        allocate(character(len=0) :: fpm)
        fpm = fpm//NL//'package-name = "fpm"'
        fpm = fpm//NL//'fortran-flags = " -Wall -Wextra -fPIC -fmax-errors=1 -g "'
        fpm = fpm//NL//'c-flags = ""'
        fpm = fpm//NL//'cxx-flags = ""'
        fpm = fpm//NL//'link-flags = ""'
        fpm = fpm//NL//'build-prefix = "build/gfortran"'
        fpm = fpm//NL//'include-dirs = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'external-modules = [ ]'
        fpm = fpm//NL//'include-tests = false'
        fpm = fpm//NL//'module-naming = false'
        fpm = fpm//NL//'module-prefix = ""'
        fpm = fpm//NL//'[compiler]'
        fpm = fpm//NL//'id = 1'
        fpm = fpm//NL//'fc = "gfortran"'
        fpm = fpm//NL//'cc = "gcc"'
        fpm = fpm//NL//'cxx = "g++"'
        fpm = fpm//NL//'echo = false'
        fpm = fpm//NL//'verbose = false'
        fpm = fpm//NL//'[archiver]'
        fpm = fpm//NL//'ar = "ar -rs "'
        fpm = fpm//NL//'use-response-file = false'
        fpm = fpm//NL//'echo = false'
        fpm = fpm//NL//'verbose = false'
        fpm = fpm//NL//'[deps]'
        fpm = fpm//NL//'unit = 6'
        fpm = fpm//NL//'verbosity = 1'
        fpm = fpm//NL//'dep-dir = "build/dependencies"'
        fpm = fpm//NL//'cache = "build/cache.toml"'
        fpm = fpm//NL//'ndep = 4'
        fpm = fpm//NL//'[deps.dependencies]'
        fpm = fpm//NL//'[deps.dependencies.fpm]'
        fpm = fpm//NL//'name = "fpm"'
        fpm = fpm//NL//'path = "."'
        fpm = fpm//NL//'version = "0.8.0"'
        fpm = fpm//NL//'proj-dir = "./."'
        fpm = fpm//NL//'done = true'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.toml-f]'
        fpm = fpm//NL//'name = "toml-f"'
        fpm = fpm//NL//'version = "0.4.0"'
        fpm = fpm//NL//'proj-dir = "build/dependencies/toml-f"'
        fpm = fpm//NL//'revision = "54686e45993f3a9a1d05d5c7419f39e7d5a4eb3f"'
        fpm = fpm//NL//'done = true'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = true'
        fpm = fpm//NL//'[deps.dependencies.toml-f.git]'
        fpm = fpm//NL//'descriptor = "revision"'
        fpm = fpm//NL//'url = "https://github.com/toml-f/toml-f"'
        fpm = fpm//NL//'object = "54686e45993f3a9a1d05d5c7419f39e7d5a4eb3f"'
        fpm = fpm//NL//'[deps.dependencies.M_CLI2]'
        fpm = fpm//NL//'name = "M_CLI2"'
        fpm = fpm//NL//'version = "1.0.0"'
        fpm = fpm//NL//'proj-dir = "build/dependencies/M_CLI2"'
        fpm = fpm//NL//'revision = "7264878cdb1baff7323cc48596d829ccfe7751b8"'
        fpm = fpm//NL//'done = true'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = true'
        fpm = fpm//NL//'[deps.dependencies.M_CLI2.git]'
        fpm = fpm//NL//'descriptor = "revision"'
        fpm = fpm//NL//'url = "https://github.com/urbanjost/M_CLI2.git"'
        fpm = fpm//NL//'object = "7264878cdb1baff7323cc48596d829ccfe7751b8"'
        fpm = fpm//NL//'[deps.dependencies.jonquil]'
        fpm = fpm//NL//'name = "jonquil"'
        fpm = fpm//NL//'version = "0.2.0"'
        fpm = fpm//NL//'proj-dir = "build/dependencies/jonquil"'
        fpm = fpm//NL//'revision = "05d30818bb12fb877226ce284b9a3a41b971a889"'
        fpm = fpm//NL//'done = true'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = true'
        fpm = fpm//NL//'[deps.dependencies.jonquil.git]'
        fpm = fpm//NL//'descriptor = "revision"'
        fpm = fpm//NL//'url = "https://github.com/toml-f/jonquil"'
        fpm = fpm//NL//'object = "05d30818bb12fb877226ce284b9a3a41b971a889"'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_5]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_6]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_7]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_8]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_9]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_10]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_11]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_12]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_13]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_14]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_15]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_16]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_17]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_18]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_19]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_20]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_21]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_22]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_23]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_24]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[deps.dependencies.UNNAMED_DEPENDENCY_25]'
        fpm = fpm//NL//'done = false'
        fpm = fpm//NL//'update = false'
        fpm = fpm//NL//'cached = false'
        fpm = fpm//NL//'[packages]'
        fpm = fpm//NL//'[packages.fpm]'
        fpm = fpm//NL//'name = "fpm"'
        fpm = fpm//NL//'version = "0.8.0"'
        fpm = fpm//NL//'module-naming = false'
        fpm = fpm//NL//'module-prefix = ""'
        fpm = fpm//NL//'[packages.fpm.fortran]'
        fpm = fpm//NL//'implicit-typing = false'
        fpm = fpm//NL//'implicit-external = false'
        fpm = fpm//NL//'source-form = "free"'
        fpm = fpm//NL//'[packages.fpm.sources]'
        fpm = fpm//NL//'[packages.fpm.sources.src_1]'
        fpm = fpm//NL//'file-name = "././src/fpm.f90"'
        fpm = fpm//NL//'digest = 4322290725857190613'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_strings", "fpm_backend", "fpm_compiler", "fpm_error" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_2]'
        fpm = fpm//NL//'file-name = "././src/fpm_backend.F90"'
        fpm = fpm//NL//'digest = -3210121688944515946'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_backend"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_filesystem", "fpm_backend_output" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_3]'
        fpm = fpm//NL//'file-name = "././src/fpm_environment.f90"'
        fpm = fpm//NL//'digest = 2235607720245152632'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_environment"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "fpm_error"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_4]'
        fpm = fpm//NL//'file-name = "././src/fpm_model.f90"'
        fpm = fpm//NL//'digest = -6774177234665080583'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_model"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_compiler", "fpm_dependency" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_5]'
        fpm = fpm//NL//'file-name = "././src/filesystem_utilities.c"'
        fpm = fpm//NL//'digest = 4957633104775755438'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_CSOURCE"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_6]'
        fpm = fpm//NL//'file-name = "././src/fpm_filesystem.F90"'
        fpm = fpm//NL//'digest = 1871084827152368652'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_filesystem"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_environment", "fpm_environment" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_7]'
        fpm = fpm//NL//'file-name = "././src/fpm_strings.f90"'
        fpm = fpm//NL//'digest = 7038915013685504829'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_strings"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_8]'
        fpm = fpm//NL//'file-name = "././src/fpm_settings.f90"'
        fpm = fpm//NL//'digest = -885425387141891996'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_settings"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_filesystem", "fpm_environment", "fpm_error" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_9]'
        fpm = fpm//NL//'file-name = "././src/fpm_os.c"'
        fpm = fpm//NL//'digest = -4523865409175594663'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_CSOURCE"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_10]'
        fpm = fpm//NL//'file-name = "././src/fpm_backend_console.f90"'
        fpm = fpm//NL//'digest = 1732983699585955966'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_backend_console"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_11]'
        fpm = fpm//NL//'file-name = "././src/fpm_source_parsing.f90"'
        fpm = fpm//NL//'digest = 6098986130375861226'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_source_parsing"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_strings", "fpm_model", "fpm_filesystem" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_12]'
        fpm = fpm//NL//'file-name = "././src/fpm_os.F90"'
        fpm = fpm//NL//'digest = -4743856136050054640'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_os"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_filesystem", "fpm_environment", "fpm_error" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_13]'
        fpm = fpm//NL//'file-name = "././src/fpm_compiler.F90"'
        fpm = fpm//NL//'digest = -2442073797366752057'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_compiler"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_environment", "fpm_filesystem", "fpm_strings" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_14]'
        fpm = fpm//NL//'file-name = "././src/fpm_command_line.f90"'
        fpm = fpm//NL//'digest = 7180707928326338392'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_command_line"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_environment", "m_cli2", "m_cli2", "fpm_strings" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_15]'
        fpm = fpm//NL//'file-name = "././src/fpm_backend_output.f90"'
        fpm = fpm//NL//'digest = 7154367044486334558'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_backend_output"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_filesystem", "fpm_targets", "fpm_backend_console" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_16]'
        fpm = fpm//NL//'file-name = "././src/fpm_targets.f90"'
        fpm = fpm//NL//'digest = -8234965779941208361'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_targets"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_model", "fpm_compiler", "fpm_environment" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_17]'
        fpm = fpm//NL//'file-name = "././src/fpm_sources.f90"'
        fpm = fpm//NL//'digest = 3391120653956350167'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_sources"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_model", "fpm_filesystem", "fpm_strings" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_18]'
        fpm = fpm//NL//'file-name = "././src/ptycheck/iscygpty.c"'
        fpm = fpm//NL//'digest = -4887164695298162637'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_CSOURCE"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = "iscygpty.h"'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_19]'
        fpm = fpm//NL//'file-name = "././src/ptycheck/isatty.c"'
        fpm = fpm//NL//'digest = 6664536934601490990'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_CSOURCE"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = "iscygpty.h"'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_20]'
        fpm = fpm//NL//'file-name = "././src/ptycheck/iscygpty.h"'
        fpm = fpm//NL//'digest = -3550201113101300999'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_CHEADER"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_21]'
        fpm = fpm//NL//'file-name = "././src/fpm/downloader.f90"'
        fpm = fpm//NL//'digest = 620358568720613499'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_SUBPROGRAM"'
        fpm = fpm//NL//'modules-provided = "fpm_downloader"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_filesystem", "fpm_versioning", "jonquil" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_22]'
        fpm = fpm//NL//'file-name = "././src/fpm/error.f90"'
        fpm = fpm//NL//'digest = 7324399436715753500'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_error"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "fpm_strings"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_23]'
        fpm = fpm//NL//'file-name = "././src/fpm/toml.f90"'
        fpm = fpm//NL//'digest = 2411620725015864401'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_toml"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_strings", "tomlf" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_24]'
        fpm = fpm//NL//'file-name = "././src/fpm/installer.f90"'
        fpm = fpm//NL//'digest = 581769321360482292'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_installer"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_environment", "fpm_error", "fpm_filesystem" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_25]'
        fpm = fpm//NL//'file-name = "././src/fpm/versioning.f90"'
        fpm = fpm//NL//'digest = -1370610786727991294'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_versioning"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "fpm_error"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_26]'
        fpm = fpm//NL//'file-name = "././src/fpm/git.f90"'
        fpm = fpm//NL//'digest = -7368368636549243157'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_git"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_filesystem", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_27]'
        fpm = fpm//NL//'file-name = "././src/fpm/dependency.f90"'
        fpm = fpm//NL//'digest = -2836785909441977019'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_dependency"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_environment", "fpm_error", "fpm_filesystem"]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_28]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest.f90"'
        fpm = fpm//NL//'digest = -1346850924839827718'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_manifest_build", "fpm_manifest_example" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_29]'
        fpm = fpm//NL//'file-name = "././src/fpm/cmd/new.f90"'
        fpm = fpm//NL//'digest = 697853208011446608'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_cmd_new"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_command_line", "fpm_environment" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_30]'
        fpm = fpm//NL//'file-name = "././src/fpm/cmd/update.f90"'
        fpm = fpm//NL//'digest = -8232305547308400988'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_cmd_update"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_command_line", "fpm_manifest" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_31]'
        fpm = fpm//NL//'file-name = "././src/fpm/cmd/install.f90"'
        fpm = fpm//NL//'digest = -6707501025391219376'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_cmd_install"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm", "fpm_backend", "fpm_command_line" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_32]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/test.f90"'
        fpm = fpm//NL//'digest = 1399197227023080626'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_test"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_manifest_dependency", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_33]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/profiles.f90"'
        fpm = fpm//NL//'digest = -7975317648924650587'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_profile"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_toml", "fpm_strings", "fpm_filesystem" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_34]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/example.f90"'
        fpm = fpm//NL//'digest = 2220193652669081694'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_example"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_manifest_dependency", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_35]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/executable.f90"'
        fpm = fpm//NL//'digest = 2826537585451151940'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_executable"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_manifest_dependency", "fpm_error", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_36]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/install.f90"'
        fpm = fpm//NL//'digest = 6941308343630725905'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_install"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_37]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/package.f90"'
        fpm = fpm//NL//'digest = 4046915203104200691'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_package"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_manifest_build", "fpm_manifest_dependency",  ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_38]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/preprocess.f90"'
        fpm = fpm//NL//'digest = 4463864760686846214'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_mainfest_preprocess"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_strings", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_39]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/build.f90"'
        fpm = fpm//NL//'digest = 7486174362460284832'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_build"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_strings", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_40]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/dependency.f90"'
        fpm = fpm//NL//'digest = -6006235286439662663'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_dependency"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_git", "fpm_versioning" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_41]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/library.f90"'
        fpm = fpm//NL//'digest = -1698783511442136567'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_library"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_strings", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_42]'
        fpm = fpm//NL//'file-name = "././src/fpm/manifest/fortran.f90"'
        fpm = fpm//NL//'digest = -6768952943164424742'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "fpm_manifest_fortran"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_error", "fpm_toml" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_43]'
        fpm = fpm//NL//'file-name = "app/main.f90"'
        fpm = fpm//NL//'exe-name = "fpm"'
        fpm = fpm//NL//'digest = 7759460120440225004'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_APP"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_PROGRAM"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_command_line", "fpm_os" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_44]'
        fpm = fpm//NL//'file-name = "test/help_test/help_test.f90"'
        fpm = fpm//NL//'exe-name = "help-test"'
        fpm = fpm//NL//'digest = -7601948172740854190'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_PROGRAM"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_filesystem", "fpm_environment" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_45]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_os.f90"'
        fpm = fpm//NL//'digest = 718441623146001654'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_os"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "fpm_filesystem", "fpm_environment", "fpm_os" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_46]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_source_parsing.f90"'
        fpm = fpm//NL//'digest = 5852386252678959798'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_SUBPROGRAM"'
        fpm = fpm//NL//'modules-provided = "test_source_parsing"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "fpm_filesystem", "fpm_strings" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_47]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_settings.f90"'
        fpm = fpm//NL//'digest = -3541669032396077479'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_settings"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "fpm_settings", "fpm_filesystem", "fpm_os" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_48]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_backend.f90"'
        fpm = fpm//NL//'digest = 2723265999281936523'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_backend"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "test_module_dependencies", "fpm_backend" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_49]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_versioning.f90"'
        fpm = fpm//NL//'digest = 7879213895027593947'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_versioning"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "fpm_versioning" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_50]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_manifest.f90"'
        fpm = fpm//NL//'digest = -5417606542127631442'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_manifest"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_filesystem", "testsuite", "fpm_manifest" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_51]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_filesystem.f90"'
        fpm = fpm//NL//'digest = -3128825714354096496'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_filesystem"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "fpm_filesystem", "fpm_environment" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_52]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_installer.f90"'
        fpm = fpm//NL//'digest = 6893981694820313345'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_installer"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "fpm_environment", "fpm_filesystem", "fpm_installer" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_53]'
        fpm = fpm//NL//'file-name = "test/fpm_test/main.f90"'
        fpm = fpm//NL//'exe-name = "fpm-test"'
        fpm = fpm//NL//'digest = -6659997723519103741'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_PROGRAM"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "test_toml", "test_manifest", "test_os" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_54]'
        fpm = fpm//NL//'file-name = "test/fpm_test/testsuite.f90"'
        fpm = fpm//NL//'digest = 4708439108904007602'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "testsuite"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "fpm_error"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_55]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_toml.f90"'
        fpm = fpm//NL//'digest = -4238391920328466228'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_toml"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "fpm_toml", "tomlf_constants", "fpm_compiler" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_56]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_package_dependencies.f90"'
        fpm = fpm//NL//'digest = 1143008373292682612'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_package_dependencies"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_filesystem", "testsuite", "fpm_versioning", "jonquil" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_57]'
        fpm = fpm//NL//'file-name = "test/fpm_test/test_module_dependencies.f90"'
        fpm = fpm//NL//'digest = -8398823885747598218'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "test_module_dependencies"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "testsuite", "fpm_targets", "fpm" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_58]'
        fpm = fpm//NL//'file-name = "test/cli_test/cli_test.f90"'
        fpm = fpm//NL//'exe-name = "cli-test"'
        fpm = fpm//NL//'digest = 7502982943646619950'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_PROGRAM"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_command_line", "fpm", "fpm_cmd_new" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.fpm.sources.src_59]'
        fpm = fpm//NL//'file-name = "test/new_test/new_test.f90"'
        fpm = fpm//NL//'exe-name = "new-test"'
        fpm = fpm//NL//'digest = 4683353150944180202'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_TEST"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_PROGRAM"'
        fpm = fpm//NL//'modules-provided = [ ]'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "fpm_filesystem", "fpm_strings", "fpm_environment" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f]'
        fpm = fpm//NL//'name = "toml-f"'
        fpm = fpm//NL//'version = "0.8.0"'
        fpm = fpm//NL//'module-naming = false'
        fpm = fpm//NL//'module-prefix = ""'
        fpm = fpm//NL//'[packages.toml-f.fortran]'
        fpm = fpm//NL//'implicit-typing = false'
        fpm = fpm//NL//'implicit-external = false'
        fpm = fpm//NL//'source-form = "free"'
        fpm = fpm//NL//'[packages.toml-f.sources]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_1]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf.f90"'
        fpm = fpm//NL//'digest = -8299830903248890534'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_build", "tomlf_datetime" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_2]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/constants.f90"'
        fpm = fpm//NL//'digest = 7170350792708576173'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_constants"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_3]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/version.f90"'
        fpm = fpm//NL//'digest = 7297460108185920032'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_version"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_4]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/structure.f90"'
        fpm = fpm//NL//'digest = -5586939372904264461'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_structure"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_structure_ordered_map" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_5]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/error.f90"'
        fpm = fpm//NL//'digest = -6990387780017431402'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_error"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "tomlf_constants"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_6]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/ser.f90"'
        fpm = fpm//NL//'digest = 2173577414279434444'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_ser"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_utils" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_7]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/de.f90"'
        fpm = fpm//NL//'digest = 6984491308379570724'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_de"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_type" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_8]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/utils.f90"'
        fpm = fpm//NL//'digest = -1654455727593730955'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_utils"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_datetime", "tomlf_utils_io" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_9]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/datetime.f90"'
        fpm = fpm//NL//'digest = 360194003049506468'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_datetime"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "tomlf_constants"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_10]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/diagnostic.f90"'
        fpm = fpm//NL//'digest = -6145654881147673446'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_diagnostic"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "tomlf_terminal"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_11]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/type.f90"'
        fpm = fpm//NL//'digest = 7822704506185839449'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_type"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_type_value" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_12]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/build.f90"'
        fpm = fpm//NL//'digest = 6734874397655167084'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_build"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_build_array", "tomlf_build_table" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_13]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/all.f90"'
        fpm = fpm//NL//'digest = -3373616532185720889'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_all"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_build", "tomlf_type", "tomlf_utils", "tomlf_version" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_14]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/terminal.f90"'
        fpm = fpm//NL//'digest = 6124874315911091908'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_terminal"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "tomlf_utils"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_15]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/utils/sort.f90"'
        fpm = fpm//NL//'digest = -7275638313901306893'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_utils_sort"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "tomlf_type_value"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_16]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/utils/io.f90"'
        fpm = fpm//NL//'digest = -4559681945420894782'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_utils_io"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "tomlf_constants"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_17]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/type/keyval.f90"'
        fpm = fpm//NL//'digest = 7305553188003635285'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_type_keyval"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_datetime", "tomlf_type_value" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_18]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/type/table.f90"'
        fpm = fpm//NL//'digest = -1731470661964884986'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_type_table"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_structure" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_19]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/type/array.f90"'
        fpm = fpm//NL//'digest = 5202963073293705116'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_type_array"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_error", "tomlf_type_value", "tomlf_structure" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_20]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/type/value.f90"'
        fpm = fpm//NL//'digest = 988208496786453415'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_type_value"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_utils" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_21]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/de/context.f90"'
        fpm = fpm//NL//'digest = -6236998766484611847'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_de_context"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_terminal" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_22]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/de/lexer.f90"'
        fpm = fpm//NL//'digest = -5703883624156149303'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_de_lexer"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_error", "tomlf_utils" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_23]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/de/token.f90"'
        fpm = fpm//NL//'digest = -6068697997670165243'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_de_token"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_24]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/de/parser.f90"'
        fpm = fpm//NL//'digest = -3187016653233800622'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_de_parser"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_datetime", "tomlf_type" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_25]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/de/abc.f90"'
        fpm = fpm//NL//'digest = -1146733275418683599'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_de_abc"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_datetime", "tomlf_de_token" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_26]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/build/merge.f90"'
        fpm = fpm//NL//'digest = -8357953095488542628'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_build_merge"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_type" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_27]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/build/keyval.f90"'
        fpm = fpm//NL//'digest = -4107572447442746790'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_build_keyval"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_datetime", "tomlf_error", "tomlf_type", "tomlf_utils" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_28]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/build/table.f90"'
        fpm = fpm//NL//'digest = 3419266420890706227'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_build_table"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_build_keyval", "tomlf_constants", "tomlf_datetime", "tomlf_error", "tomlf_type" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_29]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/build/array.f90"'
        fpm = fpm//NL//'digest = 5731959908631518546'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_build_array"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_build_keyval", "tomlf_error", "tomlf_type" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_30]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/build/path.f90"'
        fpm = fpm//NL//'digest = 1001559863484583002'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_build_path"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_build_table", "tomlf_error", "tomlf_type" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_31]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/structure/node.f90"'
        fpm = fpm//NL//'digest = 4105605469572416054'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_structure_node"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = "tomlf_type_value"'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_32]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/structure/array_list.f90"'
        fpm = fpm//NL//'digest = 1707150725310470906'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_structure_array_list"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_type_value" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_33]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/structure/ordered_map.f90"'
        fpm = fpm//NL//'digest = 9194757273934069933'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_structure_ordered_map"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants",  "tomlf_type_value" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_34]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/structure/map.f90"'
        fpm = fpm//NL//'digest = 10697944851042277'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_structure_map"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_type_value" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.toml-f.sources.src_35]'
        fpm = fpm//NL//'file-name = "build/dependencies/toml-f/src/tomlf/structure/list.f90"'
        fpm = fpm//NL//'digest = 6018335058365199200'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "tomlf_structure_list"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_type_value" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.M_CLI2]'
        fpm = fpm//NL//'name = "M_CLI2"'
        fpm = fpm//NL//'version = "0.8.0"'
        fpm = fpm//NL//'module-naming = false'
        fpm = fpm//NL//'module-prefix = ""'
        fpm = fpm//NL//'[packages.M_CLI2.fortran]'
        fpm = fpm//NL//'implicit-typing = false'
        fpm = fpm//NL//'implicit-external = false'
        fpm = fpm//NL//'source-form = "free"'
        fpm = fpm//NL//'[packages.M_CLI2.sources]'
        fpm = fpm//NL//'[packages.M_CLI2.sources.src_1]'
        fpm = fpm//NL//'file-name = "build/dependencies/M_CLI2/src/M_CLI2.F90"'
        fpm = fpm//NL//'digest = -6169834068995303802'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "m_cli2"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.jonquil]'
        fpm = fpm//NL//'name = "jonquil"'
        fpm = fpm//NL//'version = "0.8.0"'
        fpm = fpm//NL//'module-naming = false'
        fpm = fpm//NL//'module-prefix = ""'
        fpm = fpm//NL//'[packages.jonquil.fortran]'
        fpm = fpm//NL//'implicit-typing = false'
        fpm = fpm//NL//'implicit-external = false'
        fpm = fpm//NL//'source-form = "free"'
        fpm = fpm//NL//'[packages.jonquil.sources]'
        fpm = fpm//NL//'[packages.jonquil.sources.src_1]'
        fpm = fpm//NL//'file-name = "build/dependencies/jonquil/src/jonquil.f90"'
        fpm = fpm//NL//'digest = 5552073973512025871'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "jonquil"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf", "tomlf_type", "jonquil_ser" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.jonquil.sources.src_2]'
        fpm = fpm//NL//'file-name = "build/dependencies/jonquil/src/jonquil/version.f90"'
        fpm = fpm//NL//'digest = -2934903401983932826'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "jonquil_version"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.jonquil.sources.src_3]'
        fpm = fpm//NL//'file-name = "build/dependencies/jonquil/src/jonquil/ser.f90"'
        fpm = fpm//NL//'digest = 2690773570566028936'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "jonquil_ser"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_error", "tomlf_utils" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.jonquil.sources.src_4]'
        fpm = fpm//NL//'file-name = "build/dependencies/jonquil/src/jonquil/lexer.f90"'
        fpm = fpm//NL//'digest = 4057038173684122483'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "jonquil_lexer"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_datetime", "tomlf_utils" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'[packages.jonquil.sources.src_5]'
        fpm = fpm//NL//'file-name = "build/dependencies/jonquil/src/jonquil/parser.f90"'
        fpm = fpm//NL//'digest = -2426842130572494815'
        fpm = fpm//NL//'unit-scope = "FPM_SCOPE_LIB"'
        fpm = fpm//NL//'unit-type = "FPM_UNIT_MODULE"'
        fpm = fpm//NL//'modules-provided = "jonquil_parser"'
        fpm = fpm//NL//'parent-modules = [ ]'
        fpm = fpm//NL//'modules-used = [ "tomlf_constants", "tomlf_datetime", "tomlf_de_context" ]'
        fpm = fpm//NL//'include-dependencies = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'

        call string_to_toml(fpm, table)

    end subroutine fpm_081_table

    !> Convert a character string to a TOML table
    subroutine string_to_toml(string, table)

        !> The input TOML as a string
        character(*), intent(in) :: string

        !> The TOML table
        type(toml_table), allocatable, intent(out) :: table

        integer :: iunit

        ! Write
        open(newunit=iunit,form='formatted',status='scratch',action='readwrite')

        !> Dump to scratch file
        write(iunit,*) string

        !> Load from scratch file
        rewind(iunit)
        call toml_load(table, iunit)

        close(iunit)

    end subroutine string_to_toml

    subroutine fpm_model_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        type(fpm_model_t) :: model
        type(toml_table), allocatable :: table

        call model%test_serialization('fpm_model_t: default object', error)
        if (allocated(error)) return

        !> Now init form fpm 0.8.1 table
        call fpm_081_table(table)

        call model%load(table, error)
        if (allocated(error)) then
            call fatal_error(error, 'fpm_model_t: cannot load model from fpm v0.8.1 TOML')
            return
        end if

        call model%test_serialization('fpm_model_t: fpm v0.8.1 model test', error)

    end subroutine fpm_model_roundtrip


    subroutine fpm_model_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(fpm_model_t) :: model
        type(toml_table), allocatable :: table
        character(len=:), allocatable :: fpm

        allocate(character(len=0) :: fpm)
        fpm = fpm//NL//'package-name = "fpm"'
        fpm = fpm//NL//'fortran-flags = " -Wall -Wextra -fPIC -fmax-errors=1 -g "'
        fpm = fpm//NL//'c-flags = ""'
        fpm = fpm//NL//'cxx-flags = ""'
        fpm = fpm//NL//'link-flags = ""'
        fpm = fpm//NL//'build-prefix = "build/gfortran"'
        fpm = fpm//NL//'include-dirs = [ ]'
        fpm = fpm//NL//'link-libraries = [ ]'
        fpm = fpm//NL//'external-modules = "" '
        fpm = fpm//NL//'include-tests = "my_test"' ! not a boolean
        fpm = fpm//NL//'module-naming = false'
        fpm = fpm//NL//'module-prefix = ""'

        call string_to_toml(fpm, table)
        call model%load(table,error)

    end subroutine fpm_model_invalid

end module test_toml
