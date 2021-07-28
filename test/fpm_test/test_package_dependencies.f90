!> Define tests for the `fpm_dependency` module
module test_package_dependencies
    use fpm_filesystem, only: get_temp_filename
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_dependency
    use fpm_manifest
    use fpm_manifest_dependency
    use fpm_toml
    implicit none
    private

    public :: collect_package_dependencies

    type, extends(dependency_tree_t) :: mock_dependency_tree_t
    contains
        procedure :: resolve_dependency => resolve_dependency_once
    end type mock_dependency_tree_t


contains


    !> Collect all exported unit tests
    subroutine collect_package_dependencies(testsuite)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("cache-load-dump", test_cache_load_dump), &
            & new_unittest("cache-dump-load", test_cache_dump_load), &
            & new_unittest("status-after-load", test_status), &
            & new_unittest("add-dependencies", test_add_dependencies)]

    end subroutine collect_package_dependencies


    !> Round trip of the dependency cache from a dependency tree to a TOML document
    !> to a dependency tree
    subroutine test_cache_dump_load(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(dependency_tree_t) :: deps
        type(dependency_config_t) :: dep
        integer :: unit

        call new_dependency_tree(deps)
        call resize(deps%dep, 5)
        deps%ndep = 3
        dep%name = "dep1"
        dep%path = "fpm-tmp1-dir"
        call new_dependency_node(deps%dep(1), dep, proj_dir=dep%path)
        dep%name = "dep2"
        dep%path = "fpm-tmp2-dir"
        call new_dependency_node(deps%dep(2), dep, proj_dir=dep%path)
        dep%name = "dep3"
        dep%path = "fpm-tmp3-dir"
        call new_dependency_node(deps%dep(3), dep, proj_dir=dep%path)

        open(newunit=unit, status='scratch')
        call deps%dump(unit, error)
        if (.not.allocated(error)) then
            rewind(unit)

            call new_dependency_tree(deps)
            call resize(deps%dep, 2)
            call deps%load(unit, error)
            close(unit)
        end if
        if (allocated(error)) return

        if (deps%ndep /= 3) then
            call test_failed(error, "Expected three dependencies in loaded cache")
            return
        end if

    end subroutine test_cache_dump_load


    !> Round trip of the dependency cache from a TOML data structure to
    !> a dependency tree to a TOML data structure
    subroutine test_cache_load_dump(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(toml_key), allocatable :: list(:)
        type(dependency_tree_t) :: deps

        table = toml_table()
        call add_table(table, "dep1", ptr)
        call set_value(ptr, "version", "1.1.0")
        call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
        call add_table(table, "dep2", ptr)
        call set_value(ptr, "version", "0.55.3")
        call set_value(ptr, "proj-dir", "fpm-tmp2-dir")
        call set_value(ptr, "git", "https://github.com/fortran-lang/dep2")
        call add_table(table, "dep3", ptr)
        call set_value(ptr, "version", "20.1.15")
        call set_value(ptr, "proj-dir", "fpm-tmp3-dir")
        call set_value(ptr, "git", "https://gitlab.com/fortran-lang/dep3")
        call set_value(ptr, "rev", "c0ffee")
        call add_table(table, "dep4", ptr)
        call set_value(ptr, "proj-dir", "fpm-tmp4-dir")

        call new_dependency_tree(deps)
        call deps%load(table, error)
        if (allocated(error)) return

        if (deps%ndep /= 4) then
            call test_failed(error, "Expected four dependencies in loaded cache")
            return
        end if

        call table%destroy
        table = toml_table()

        call deps%dump(table, error)
        if (allocated(error)) return

        call table%get_keys(list)

        if (size(list) /= 4) then
            call test_failed(error, "Expected four dependencies in dumped cache")
            return
        end if

    end subroutine test_cache_load_dump


    subroutine test_status(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(dependency_tree_t) :: deps

        table = toml_table()
        call add_table(table, "dep1", ptr)
        call set_value(ptr, "version", "1.1.0")
        call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
        call add_table(table, "dep2", ptr)
        call set_value(ptr, "version", "0.55.3")
        call set_value(ptr, "proj-dir", "fpm-tmp2-dir")
        call set_value(ptr, "git", "https://github.com/fortran-lang/dep2")

        call new_dependency_tree(deps)
        call deps%load(table, error)
        if (allocated(error)) return

        if (deps%finished()) then
            call test_failed(error, "Newly initialized dependency tree cannot be reolved")
            return
        end if

    end subroutine test_status


    subroutine test_add_dependencies(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table
        type(toml_table), pointer :: ptr
        type(mock_dependency_tree_t) :: deps
        type(dependency_config_t), allocatable :: nodes(:)

        table = toml_table()
        call add_table(table, "sub1", ptr)
        call set_value(ptr, "path", "external")
        call add_table(table, "lin2", ptr)
        call set_value(ptr, "git", "https://github.com/fortran-lang/lin2")
        call add_table(table, "pkg3", ptr)
        call set_value(ptr, "git", "https://gitlab.com/fortran-lang/pkg3")
        call set_value(ptr, "rev", "c0ffee")
        call add_table(table, "proj4", ptr)
        call set_value(ptr, "path", "vendor")

        call new_dependencies(nodes, table, error)
        if (allocated(error)) return

        call new_dependency_tree(deps%dependency_tree_t)
        call deps%add(nodes, error)
        if (allocated(error)) return

        if (deps%finished()) then
            call test_failed(error, "Newly added nodes cannot be already resolved")
            return
        end if

        if (deps%ndep /= 4) then
            call test_failed(error, "Expected for dependencies in tree")
            return
        end if

        call deps%resolve(".", error)
        if (allocated(error)) return

        if (.not.deps%finished()) then
            call test_failed(error, "Mocked dependency tree must resolve in one step")
            return
        end if

    end subroutine test_add_dependencies


    !> Resolve a single dependency node
    subroutine resolve_dependency_once(self, dependency, root, error)
        !> Mock instance of the dependency tree
        class(mock_dependency_tree_t), intent(inout) :: self
        !> Dependency configuration to add
        type(dependency_node_t), intent(inout) :: dependency
        !> Current installation prefix
        character(len=*), intent(in) :: root
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        if (dependency%done) then
            call test_failed(error, "Should only visit this node once")
            return
        end if
        dependency%done = .true.

    end subroutine resolve_dependency_once


end module test_package_dependencies
