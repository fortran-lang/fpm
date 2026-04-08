!> Define tests for the `fpm_dependency` module
module test_package_dependencies
  use fpm_filesystem, only: get_temp_filename
  use testsuite, only: new_unittest, unittest_t, error_t, test_failed
  use fpm_filesystem, only: is_dir, join_path, filewrite, mkdir, os_delete_dir, exists
  use fpm_environment, only: os_is_unix
  use fpm_os, only: get_current_directory
  use fpm_dependency
  use fpm_manifest_dependency
  use fpm_manifest_metapackages, only: metapackage_config_t
  use fpm_manifest, only: package_config_t, get_package_data
  use tomlf, only: toml_table, new_table
  use fpm_toml, only: toml_key, add_table, set_value, get_value
  use fpm_settings, only: fpm_global_settings, get_registry_settings, get_global_settings
  use fpm_downloader, only: downloader_t
  use fpm_versioning, only: version_t
  use jonquil, only: json_object, json_value, json_loads, cast_to_object

  implicit none
  private

  public :: collect_package_dependencies

  character(*), parameter :: tmp_folder = 'tmp'
  character(*), parameter :: config_file_name = 'config.toml'

  type, extends(dependency_tree_t) :: mock_dependency_tree_t
  contains
    procedure, private :: resolve_dependency => resolve_dependency_once
  end type mock_dependency_tree_t

  type, extends(downloader_t) :: mock_downloader_t
  contains
    procedure, nopass :: get_pkg_data, get_file, unpack => unpack_mock_package
  end type mock_downloader_t

contains

  !> Collect all exported unit tests
  subroutine collect_package_dependencies(testsuite)

    !> Collection of tests
    type(unittest_t), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
        & new_unittest("cache-load-dump", test_cache_load_dump), &
        & new_unittest("cache-dump-load", test_cache_dump_load), &
        & new_unittest("status-after-load", test_status), &
        & new_unittest("add-dependencies", test_add_dependencies), &
        & new_unittest("update-dependencies", test_update_dependencies), &
        & new_unittest("metapackage-override", test_metapackage_override), &
        & new_unittest("do-not-update-dependencies", test_non_updated_dependencies), &
        & new_unittest("registry-dir-not-found", registry_dir_not_found, should_fail=.true.), &
        & new_unittest("no-versions-in-registry", no_versions_in_registry, should_fail=.true.), &
  & new_unittest("local-registry-specified-version-not-found", local_registry_specified_version_not_found, should_fail=.true.), &
        & new_unittest("local-registry-specified-no-manifest", local_registry_specified_no_manifest, should_fail=.true.), &
        & new_unittest("local-registry-specified-has-manifest", local_registry_specified_has_manifest), &
        & new_unittest("local-registry-specified-not-a-dir", local_registry_specified_not_a_dir, should_fail=.true.), &
        & new_unittest("local-registry-unspecified-no-versions", local_registry_unspecified_no_versions, should_fail=.true.), &
        & new_unittest("local-registry-unspecified-no-manifest", local_registry_unspecified_no_manifest, should_fail=.true.), &
        & new_unittest("local-registry-unspecified-has-manifest", local_registry_unspecified_has_manifest), &
        & new_unittest("cache-specified-version-found", cache_specified_version_found), &
        & new_unittest("specified-version-not-found-in-cache", registry_specified_version_not_found_in_cache), &
        & new_unittest("registry-specified-version-not-exists", registry_specified_version_not_exists, should_fail=.true.), &
        & new_unittest("registry-specified-version-other-versions-exist", registry_specified_version_other_versions_exist), &
        & new_unittest("registry-unspecified-version", registry_unspecified_version), &
        & new_unittest("registry-unspecified-version_exists_in_cache", registry_unspecified_version_exists_in_cache), &
        & new_unittest("pkg-data-no-code", pkg_data_no_code, should_fail=.true.), &
        & new_unittest("pkg-data-corrupt-code", pkg_data_corrupt_code, should_fail=.true.), &
        & new_unittest("pkg-data-missing-error-message", pkg_data_missing_error_msg, should_fail=.true.), &
        & new_unittest("pkg-data-error-reading-message", pkg_data_error_reading_msg, should_fail=.true.), &
        & new_unittest("pkg-data-error-has-message", pkg_data_error_has_msg, should_fail=.true.), &
        & new_unittest("pkg-data-error-no-data", pkg_data_no_data, should_fail=.true.), &
        & new_unittest("pkg-data-error-reading-data", pkg_data_error_reading_data, should_fail=.true.), &
        & new_unittest("pkg-data-requested-version-wrong-key", pkg_data_requested_version_wrong_key, should_fail=.true.), &
        & new_unittest("pkg-data-no-version-requested-wrong-key", pkg_data_no_version_requested_wrong_key, should_fail=.true.), &
        & new_unittest("pkg-data-error-reading-latest-version", pkg_data_error_reading_latest_version, should_fail=.true.), &
        & new_unittest("pkg-data-no-download-url", pkg_data_no_download_url, should_fail=.true.), &
        & new_unittest("pkg-data-error-reading-donwload-url", pkg_data_error_reading_download_url, should_fail=.true.), &
        & new_unittest("pkg-data-no-version", pkg_data_no_version, should_fail=.true.), &
        & new_unittest("pkg-data-error-reading-version", pkg_data_error_reading_version, should_fail=.true.), &
        & new_unittest("pkg-data-invalid-version", pkg_data_invalid_version, should_fail=.true.) &
        & ]

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

    open (newunit=unit, status='scratch')
    call deps%dump_cache(unit, error)
    if (.not. allocated(error)) then
      rewind (unit)

      call new_dependency_tree(deps)
      call resize(deps%dep, 2)
      call deps%load_cache(unit, error)
      close (unit)
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
    call deps%load_cache(table, error)
    if (allocated(error)) return

    if (deps%ndep /= 4) then
      call test_failed(error, "Expected four dependencies in loaded cache")
      return
    end if

    call table%destroy
    table = toml_table()

    call deps%dump_cache(table, error)
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
    call deps%load_cache(table, error)
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

    call new_dependencies(nodes, table, error=error)
    if (allocated(error)) return

    call new_dependencies(nodes, table, root='.', error=error)
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

    ! Do not use polymorphic version due to Ifort issue
    call resolve_dependencies(deps, ".", error)
    if (allocated(error)) return

    if (.not. deps%finished()) then
      call test_failed(error, "Mocked dependency tree must resolve in one step")
      return
    end if

  end subroutine test_add_dependencies

  subroutine test_non_updated_dependencies(error)

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: cache, manifest
    type(toml_table), pointer :: ptr
    type(toml_key), allocatable :: list(:)
    type(dependency_tree_t) :: cached, manifest_deps
    integer :: ii

    ! Create a dummy cache
    cache = toml_table()
    call add_table(cache, "dep1", ptr)
    call set_value(ptr, "version", "1.1.0")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(cache, "dep2", ptr)
    call set_value(ptr, "git", "https://gitlab.com/fortran-lang/lin2")
    call set_value(ptr, "rev", "c0ffee")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(cache, "dep3", ptr)
    call set_value(ptr, "git", "https://gitlab.com/fortran-lang/pkg3")
    call set_value(ptr, "rev", "t4a")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(cache, "dep4", ptr)
    call set_value(ptr, "version", "1.0.0")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")

    ! Load into a dependency tree
    call new_dependency_tree(cached)
    call cached%load_cache(cache, error)
    if (allocated(error)) return
    ! Mark all dependencies as "cached"
    do ii=1,cached%ndep
        cached%dep(ii)%cached = .true.
    end do
    call cache%destroy()

    ! Create a dummy manifest, with different version
    manifest = toml_table()
    call add_table(manifest, "dep1", ptr)
    call set_value(ptr, "version", "1.1.1")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(manifest, "dep2", ptr)
    call set_value(ptr, "git", "https://gitlab.com/fortran-lang/lin4")
    call set_value(ptr, "rev", "c0ffee")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(manifest, "dep3", ptr)
    call set_value(ptr, "git", "https://gitlab.com/fortran-lang/pkg3")
    call set_value(ptr, "rev", "t4a")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")

    ! Load dependencies from manifest
    call new_dependency_tree(manifest_deps)
    call manifest_deps%load_cache(manifest, error)
    call manifest%destroy()
    if (allocated(error)) return

    ! Add cached dependencies afterwards; will flag those that need udpate
    do ii=1,cached%ndep
        cached%dep(ii)%cached = .true.
        call manifest_deps%add(cached%dep(ii), error)
        if (allocated(error)) return
    end do

    ! Test that dependencies 1-2 are flagged as "update"
    if (.not. manifest_deps%dep(1)%update) then
      call test_failed(error, "Updated dependency (different version) not detected")
      return
    end if
    if (.not. manifest_deps%dep(2)%update) then
      call test_failed(error, "Updated dependency (git address) not detected")
      return
    end if

    ! Test that dependency 3 is flagged as "not update"
    if (manifest_deps%dep(3)%update) then
      call test_failed(error, "Updated dependency (git rev) detected, should not be")
      return
    end if

  end subroutine test_non_updated_dependencies

  subroutine test_update_dependencies(error)

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: cache, manifest
    type(toml_table), pointer :: ptr
    type(toml_key), allocatable :: list(:)
    type(dependency_tree_t) :: cached, manifest_deps
    integer :: ii

    ! Create a dummy cache
    cache = toml_table()
    call add_table(cache, "dep1", ptr)
    call set_value(ptr, "version", "1.1.0")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(cache, "dep2", ptr)
    call set_value(ptr, "git", "https://gitlab.com/fortran-lang/lin2")
    call set_value(ptr, "rev", "c0ffee")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(cache, "dep3", ptr)
    call set_value(ptr, "git", "https://gitlab.com/fortran-lang/pkg3")
    call set_value(ptr, "rev", "t4a")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(cache, "dep4", ptr)
    call set_value(ptr, "version", "1.0.0")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")

    ! Load into a dependency tree
    call new_dependency_tree(cached)
    call cached%load_cache(cache, error)
    if (allocated(error)) return
    ! Mark all dependencies as "cached"
    do ii=1,cached%ndep
        cached%dep(ii)%cached = .true.
    end do
    call cache%destroy()

    ! Create a dummy manifest, with different version
    manifest = toml_table()
    call add_table(manifest, "dep1", ptr)
    call set_value(ptr, "version", "1.1.1")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(manifest, "dep2", ptr)
    call set_value(ptr, "git", "https://gitlab.com/fortran-lang/lin4")
    call set_value(ptr, "rev", "c0ffee")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")
    call add_table(manifest, "dep3", ptr)
    call set_value(ptr, "git", "https://gitlab.com/fortran-lang/pkg3")
    call set_value(ptr, "rev", "l4tte")
    call set_value(ptr, "proj-dir", "fpm-tmp1-dir")

    ! Load dependencies from manifest
    call new_dependency_tree(manifest_deps)
    call manifest_deps%load_cache(manifest, error)
    call manifest%destroy()
    if (allocated(error)) return

    ! Add cached dependencies afterwards; will flag those that need udpate
    do ii=1,cached%ndep
        cached%dep(ii)%cached = .true.
        call manifest_deps%add(cached%dep(ii), error)
        if (allocated(error)) return
    end do

    ! Test that all dependencies are flagged as "update"
    if (.not. manifest_deps%dep(1)%update) then
      call test_failed(error, "Updated dependency (different version) not detected")
      return
    end if
    if (.not. manifest_deps%dep(2)%update) then
      call test_failed(error, "Updated dependency (git address) not detected")
      return
    end if
    if (.not. manifest_deps%dep(3)%update) then
      call test_failed(error, "Updated dependency (git rev) not detected")
      return
    end if

  end subroutine test_update_dependencies


  !> Test that a metapackage is overridden if a regular dependency is provided
  subroutine test_metapackage_override(error)

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: manifest
    type(toml_table), pointer :: ptr
    type(dependency_config_t), allocatable :: deps(:)
    type(metapackage_config_t) :: meta
    logical :: found
    integer :: i

    ! Create a dummy manifest, with a standard git dependency for stdlib
    manifest = toml_table()
    call add_table(manifest, "stdlib", ptr)
    call set_value(ptr, "git", "https://github.com/fortran-lang/stdlib")
    call set_value(ptr, "branch", "stdlib-fpm")

    ! Load dependencies from manifest
    call new_dependencies(deps, manifest, meta=meta, error=error)
    if (allocated(error)) return

    ! Check that stdlib is in the regular dependency list
    found = .false.
    do i=1,size(deps)
       if (deps(i)%name=="stdlib") found = .true.
    end do

    if (.not.found) then
        call test_failed(error,"standard git-based dependency for stdlib not recognized")
        return
    end if
    call manifest%destroy()


    ! Create a dummy manifest, with a version-based metapackage dependency for stdlib
    manifest = toml_table()
    call set_value(manifest, "stdlib", "*")

    ! Load dependencies from manifest
    call new_dependencies(deps, manifest, meta=meta, error=error)
    if (allocated(error)) return

    ! Check that stdlib is in the metapackage config and not the standard dependencies
    found = .false.
    do i=1,size(deps)
       if (deps(i)%name=="stdlib") found = .true.
    end do

    if (found) then
        call test_failed(error,"metapackage dependency for stdlib should not be in the tree")
        return
    end if
    call manifest%destroy()

    if (.not.meta%stdlib%on) then
        call test_failed(error,"metapackage dependency for stdlib should be in the metapackage config")
        return
    end if

  end subroutine test_metapackage_override

  !> Directories for namespace and package name not found in path registry.
  subroutine registry_dir_not_found(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache')) ! Missing directories for namesapce and package name

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine registry_dir_not_found

  !> No versions found in path registry.
  subroutine no_versions_in_registry(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep'))

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine no_versions_in_registry

  !> Specific version not found in the local registry.
  subroutine local_registry_specified_version_not_found(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')
    call set_value(table, 'v', '0.1.0')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.0.9'))
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.1'))

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine local_registry_specified_version_not_found

  !> Target package in path registry does not contain manifest.
  subroutine local_registry_specified_no_manifest(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')
    call set_value(table, 'v', '0.1.0')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.0.9'))
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.0'))
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.1'))

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine local_registry_specified_no_manifest

  !> Target package in path registry contains manifest.
  subroutine local_registry_specified_has_manifest(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir, cwd
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')
    call set_value(table, 'v', '0.1.0')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.0.0'))
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.0'))
    call filewrite(join_path(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.0'), 'fpm.toml'), [''])
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.2.0'))

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_current_directory(cwd, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    if (target_dir /= join_path(cwd, join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.0'))) then
      call test_failed(error, 'target_dir not set correctly')
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine local_registry_specified_has_manifest

  !> Target is a file, not a directory.
  subroutine local_registry_specified_not_a_dir(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep'))
    call filewrite(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.0'), ['']) ! File, not directory

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine local_registry_specified_not_a_dir

  !> Try fetching the latest version in the local registry, but none are found.
  !> Compared to no-versions-in-registry, we aren't requesting a specific version here.
  subroutine local_registry_unspecified_no_versions(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep'))

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine local_registry_unspecified_no_versions

  !> Latest version in the local registry does not have a manifest.
  subroutine local_registry_unspecified_no_manifest(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir, cwd
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.0.0'))
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '1.3.0'))
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '1.2.1'))

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_current_directory(cwd, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    if (target_dir /= join_path(cwd, join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '1.3.0'))) then
      call test_failed(error, 'target_dir not set correctly: '//target_dir//"'")
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine local_registry_unspecified_no_manifest

  !> Latest version in the local registry has a manifest.
  subroutine local_registry_unspecified_has_manifest(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir, cwd
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.0.0'))
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '1.3.0'))
    call filewrite(join_path(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '1.3.0'), 'fpm.toml'), [''])
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '1.2.1'))

    call new_table(table)
    call add_table(table, 'registry', child)
    call set_value(child, 'path', 'cache')

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_current_directory(cwd, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    if (target_dir /= join_path(cwd, join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '1.3.0'))) then
      call test_failed(error, 'target_dir not set correctly: '//target_dir//"'")
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine local_registry_unspecified_has_manifest

  !> Version specified in manifest, version found in cache.
  subroutine cache_specified_version_found(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir, cwd, path
    type(toml_table), pointer :: child

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')
    call set_value(table, 'v', '2.3.0')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    path = join_path(tmp_folder, 'dependencies', 'test-org', 'test-dep', '2.3.0')
    call mkdir(path)
    call filewrite(join_path(path, 'fpm.toml'), [''])

    call new_table(table)
    call add_table(table, 'registry', child) ! No cache_path specified, use default

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_current_directory(cwd, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    if (target_dir /= join_path(cwd, join_path(tmp_folder, 'dependencies', 'test-org', 'test-dep', '2.3.0'))) then
      call test_failed(error, "Target directory not set correctly: '"//target_dir//"'")
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine cache_specified_version_found

  !> Version specified in manifest, but not found in cache. Therefore download dependency.
  subroutine registry_specified_version_not_found_in_cache(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir, cwd
    type(toml_table), pointer :: child
    type(mock_downloader_t) :: mock_downloader

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')
    call set_value(table, 'v', '0.1.0')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(tmp_folder) ! Dependencies folder doesn't exist

    call new_table(table)
    call add_table(table, 'registry', child)

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error, mock_downloader)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_current_directory(cwd, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    if (target_dir /= join_path(cwd, join_path(tmp_folder, 'dependencies', 'test-org', 'test-dep', '0.1.0'))) then
      call test_failed(error, "Target directory not set correctly: '"//target_dir//"'")
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine registry_specified_version_not_found_in_cache

  !> Version specified in manifest, but not found in cache or registry.
  subroutine registry_specified_version_not_exists(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir
    type(toml_table), pointer :: child
    type(mock_downloader_t) :: mock_downloader

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')
    call set_value(table, 'v', '9.9.9')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(tmp_folder)

    call new_table(table)
    call add_table(table, 'registry', child)

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error, mock_downloader)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine registry_specified_version_not_exists

  subroutine registry_specified_version_other_versions_exist(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir
    type(toml_table), pointer :: child
    type(mock_downloader_t) :: mock_downloader

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')
    call set_value(table, 'v', '0.1.0')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'dependencies', 'test-org', 'test-dep', '2.1.0'))
    call mkdir(join_path(tmp_folder, 'dependencies', 'test-org', 'test-dep', '9.1.0'))

    call new_table(table)
    call add_table(table, 'registry', child)

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error, mock_downloader)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine registry_specified_version_other_versions_exist

  !> No version specified, get the newest version from the registry.
  subroutine registry_unspecified_version(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir, cwd
    type(toml_table), pointer :: child
    type(mock_downloader_t) :: mock_downloader

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(tmp_folder)

    call new_table(table)
    call add_table(table, 'registry', child)

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error, mock_downloader)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_current_directory(cwd, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    if (target_dir /= join_path(cwd, join_path(tmp_folder, 'dependencies', 'test-org', 'test-dep', '0.1.0'))) then
      call test_failed(error, "Target directory not set correctly: '"//target_dir//"'")
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine registry_unspecified_version

  !> No version specified, therefore load package data from the registry. Find out that there is a cached version of
  !> the latest package.
  subroutine registry_unspecified_version_exists_in_cache(error)
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(dependency_node_t) :: node
    type(fpm_global_settings) :: global_settings
    character(len=:), allocatable :: target_dir, cwd
    type(toml_table), pointer :: child
    type(mock_downloader_t) :: mock_downloader

    call new_table(table)
    table%key = 'test-dep'
    call set_value(table, 'namespace', 'test-org')

    call new_dependency(node%dependency_config_t, table, error=error)
    if (allocated(error)) return

    call delete_tmp_folder
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.0.0'))
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.0'))
    call filewrite(join_path(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '0.1.0'), 'fpm.toml'), [''])
    call mkdir(join_path(tmp_folder, 'cache', 'test-org', 'test-dep', '1.2.1'))

    call new_table(table)
    call add_table(table, 'registry', child)

    call setup_global_settings(global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_registry_settings(child, global_settings, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call node%get_from_registry(target_dir, global_settings, error, mock_downloader)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    call get_current_directory(cwd, error)
    if (allocated(error)) then
      call delete_tmp_folder; return
    end if

    if (target_dir /= join_path(cwd, join_path(tmp_folder, 'dependencies', 'test-org', 'test-dep', '0.1.0'))) then
      call test_failed(error, "Target directory not set correctly: '"//target_dir//"'")
      call delete_tmp_folder; return
    end if

    call delete_tmp_folder

  end subroutine registry_unspecified_version_exists_in_cache

  !> Package data returned from the registry does not contain a code field.
  subroutine pkg_data_no_code(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_no_code

  !> Error reading status code from package data.
  subroutine pkg_data_corrupt_code(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": "integer expected"}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_corrupt_code

  subroutine pkg_data_missing_error_msg(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 123}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_missing_error_msg

  subroutine pkg_data_error_reading_msg(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 123, "message": 123}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_error_reading_msg

  subroutine pkg_data_error_has_msg(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 123, "message": "Really bad error message"}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_error_has_msg

  subroutine pkg_data_no_data(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_no_data

  subroutine pkg_data_error_reading_data(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200, "data": 123}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_error_reading_data

  subroutine pkg_data_requested_version_wrong_key(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    allocate (node%requested_version)
    call json_loads(j_value, '{"code": 200, "data": {"latest_version_data": 123}}') ! Expected key: "version_data"
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_requested_version_wrong_key

  subroutine pkg_data_no_version_requested_wrong_key(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200, "data": {"version_data": 123}}') ! Expected key: "latest_version_data"
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_no_version_requested_wrong_key

  subroutine pkg_data_error_reading_latest_version(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200, "data": {"latest_version_data": 123}}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_error_reading_latest_version

  subroutine pkg_data_no_download_url(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200, "data": {"latest_version_data": {}}}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_no_download_url

  subroutine pkg_data_error_reading_download_url(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200, "data": {"latest_version_data": {"download_url": 123}}}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_error_reading_download_url

  subroutine pkg_data_no_version(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200, "data": {"latest_version_data": {"download_url": "abc"}}}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_no_version

  subroutine pkg_data_error_reading_version(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200, "data": {"latest_version_data": {"download_url": "abc", "version": 123}}}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_error_reading_version

  subroutine pkg_data_invalid_version(error)
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node
    character(:), allocatable :: url
    type(version_t) :: version
    type(json_object) :: json
    class(json_value), allocatable :: j_value

    call json_loads(j_value, '{"code": 200, "data": {"latest_version_data": {"download_url": "abc", "version": "abc"}}}')
    json = cast_to_object(j_value)

    call check_and_read_pkg_data(json, node, url, version, error)

  end subroutine pkg_data_invalid_version

  !> Resolve a single dependency node
  subroutine resolve_dependency_once(self, dependency, global_settings, root, error)
    !> Mock instance of the dependency tree
    class(mock_dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_node_t), intent(inout) :: dependency
    !> Global configuration settings.
    type(fpm_global_settings), intent(in) :: global_settings
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

  !> Resolve all dependencies in the tree
  subroutine resolve_dependencies(self, root, error)
    !> Instance of the dependency tree
    type(mock_dependency_tree_t), intent(inout) :: self
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(fpm_global_settings) :: global_settings
    integer :: ii

    call get_global_settings(global_settings, error)
    if (allocated(error)) return

    do ii = 1, self%ndep
      call resolve_dependency_once(self, self%dep(ii), global_settings, root, error)
      if (allocated(error)) exit
    end do

    if (allocated(error)) return

  end subroutine resolve_dependencies

  subroutine delete_tmp_folder
    if (is_dir(tmp_folder)) call os_delete_dir(os_is_unix(), tmp_folder)
  end

  subroutine setup_global_settings(global_settings, error)
    type(fpm_global_settings), intent(out) :: global_settings
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: cwd

    call get_current_directory(cwd, error)
    if (allocated(error)) return

    global_settings%path_to_config_folder = join_path(cwd, tmp_folder)
    global_settings%config_file_name = config_file_name
  end

  subroutine get_pkg_data(url, version, tmp_pkg_file, json, error)
    character(*), intent(in) :: url
    type(version_t), allocatable, intent(in) :: version
    character(*), intent(in) :: tmp_pkg_file
    type(json_object), intent(out) :: json
    type(error_t), allocatable, intent(out) :: error

    class(json_value), allocatable :: j_value

    if (allocated(version)) then
      if (version%s() == '9.9.9') then
        call json_loads(j_value, '{"code": 404, "message": "Package not found"}')
      else
        call json_loads(j_value, '{"code": 200, "data": {"version_data": {"version": "0.1.0", "download_url": "abc"}}}')
      end if
    else
      call json_loads(j_value, '{"code": 200, "data": {"latest_version_data": {"version": "0.1.0", "download_url": "abc"}}}')
    end if

    json = cast_to_object(j_value)
  end

  subroutine get_file(url, tmp_pkg_file, error)
    character(*), intent(in) :: url
    character(*), intent(in) :: tmp_pkg_file
    type(error_t), allocatable, intent(out) :: error
  end

  subroutine unpack_mock_package(tmp_pkg_file, destination, error)
    character(*), intent(in) :: tmp_pkg_file
    character(*), intent(in) :: destination
    type(error_t), allocatable, intent(out) :: error

    integer :: stat

    call execute_command_line('cp '//tmp_pkg_file//' '//destination, exitstat=stat)

    if (stat /= 0) then
      call test_failed(error, "Failed to create mock package"); return
    end if
  end

end module test_package_dependencies
