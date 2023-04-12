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
    use fpm_model, only: fortran_features_t, package_t, FPM_SCOPE_LIB, FPM_UNIT_MODULE

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
            & new_unittest("serialize-dependency-node", dependency_node_roundtrip), &
            & new_unittest("serialize-dependency-tree", dependency_tree_roundtrip), &
            & new_unittest("serialize-string-array", string_array_roundtrip), &
            & new_unittest("serialize-fortran-features", fft_roundtrip), &
            & new_unittest("serialize-package", package_roundtrip)]

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

    !> Test serialization/deserialization of a package_t structure
    subroutine package_roundtrip(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_t) :: pkg
        integer :: ierr

        call pkg%dump('pkg.toml',error)

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



end module test_toml
