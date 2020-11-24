!> # Dependency management
!>
!> ## Fetching dependencies and creating a dependency tree
!>
!> Dependencies on the top-level can be specified from:
!>
!> - `package%dependencies`
!> - `package%dev_dependencies`
!> - `package%executable(:)%dependencies`
!> - `package%test(:)%dependencies`
!>
!> Each dependency is fetched in some way and provides a path to its package
!> manifest.
!> The `package%dependencies` of the dependencies are resolved recursively.
!>
!> To initialize the dependency tree all dependencies are recursively fetched
!> and stored in a flat data structure to avoid retrieving a package twice.
!> The data structure used to store this information should describe the current
!> status of the dependency tree. Important information are:
!>
!> - name of the package
!> - version of the package
!> - path to the package root
!>
!> Additionally, for version controlled dependencies the following should be
!> stored along with the package:
!>
!> - the upstream url
!> - the current checked out revision
!>
!> Fetching a remote (version controlled) dependency turns it for our purpose
!> into a local path dependency which is handled by the same means.
!>
!> ## Updating dependencies
!>
!> For a given dependency tree all top-level dependencies can be updated.
!> We have two cases to consider, a remote dependency and a local dependency,
!> again, remote dependencies turn into local dependencies by fetching.
!> Therefore we will update remote dependencies by simply refetching them.
!>
!> For remote dependencies we have to refetch if the information in the manifest
!> changes like:
!>
!> - different upstream URL
!> - changed revision
!>
!> or the upstream HEAD has changed (works similar for branches _and_ tags).
!>
!> @Note For our purpose a tag is just a fancy branch name. Tags can be delete and
!>       modified afterwards, therefore they do not differ too much from branches
!>       from our perspective.
!>
!> For the latter case we only know if we actually fetch from the upstream URL.
!>
!> In case of local (and fetched remote) dependencies we have to read the package
!> manifest and compare its dependencies against our dependency tree, any change
!> requires updating the respective dependencies as well.
!>
!> ## Handling dependency compatibilties
!>
!> Currenly ignored. First come, first serve.
module fpm_dependency
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm_constants, only : fpm_manifest_file, fpm_build_dir, fpm_lock_file
  use fpm_error, only : error_t, fatal_error
  use fpm_filesystem, only : join_path, exists, mkdir
  use fpm_git, only : git_target_t, git_revision
  use fpm_manifest, only : get_package_data, package_config_t, &
    executable_config_t, dependency_config_t
  use fpm_toml, only : toml_table, toml_parse, toml_serializer, toml_error, &
    toml_key, add_table, set_value, get_value
  use fpm_versioning, only : version_t, char
  implicit none
  private
  public :: update_dep_lock, dependency_walker_t, new_dependency_walker

  type :: enum_policy
    integer :: fetch = 1
    integer :: update = 2
    integer :: force_update = 3
  end type enum_policy
  type(enum_policy), parameter :: update_policy = enum_policy()

  type :: update_name
    character(len=:), allocatable :: dep
  end type update_name

  !> Common information for walking the dependency tree
  type :: dependency_walker_t
    !> Prefix for saving dependencies in
    character(len=:), allocatable :: prefix
    !> Update policy
    integer :: policy = update_policy%fetch
    !> Rerender all dependencies
    logical :: clean = .false.
    !> Output unit for diagnostics
    integer :: unit = output_unit
    !> Print level while walking the dependency tree
    integer :: verbosity = 1
    !> Dependencies to update
    type(update_name), allocatable :: update(:)
  end type dependency_walker_t

contains

  !> Constructor for dependency walker
  pure function new_dependency_walker(prefix, update, force_update, clean, &
      verbosity) result(self)
    !> Prefix for storing dependencies
    character(len=*), intent(in) :: prefix
    !> Dependencies to update
    character(len=*), intent(in), optional :: update(:)
    !> Update all existing packages
    logical, intent(in), optional :: force_update
    !> Rerender all dependencies
    logical, intent(in), optional :: clean
    !> Print level while walking the dependency tree
    integer, intent(in), optional :: verbosity
    !> Instance of the dependency handler
    type(dependency_walker_t) :: self

    integer :: ii

    self%prefix = prefix
    if (present(update)) then
      allocate(self%update(size(update)))
      do ii = 1, size(update)
        self%update(ii)%dep = trim(update(ii))
      end do
    else
      allocate(self%update(0))
    end if
    self%policy = merge(update_policy%update, update_policy%fetch, present(update))
    if (present(force_update)) then
      if (force_update) self%policy = update_policy%force_update
    end if
    if (present(clean)) then
      self%clean = clean
    end if
    if (present(verbosity)) then
      self%verbosity = verbosity
    end if

  end function new_dependency_walker

  !> Update all dependencies in the current project
  subroutine update_dep_lock(config, package, error)
    !> Instance of the dependency handler
    class(dependency_walker_t), intent(in) :: config
    !> Package configuration data
    type(package_config_t), intent(in) :: package
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_table), allocatable :: table
    type(toml_serializer) :: ser
    character(len=:), allocatable :: root, lock
    integer :: unit

    lock = join_path(fpm_build_dir, fpm_lock_file)
    root = "."

    if (.not.config%clean .and. exists(lock)) then
      open(file=lock, newunit=unit)
      call get_dependency_lock(table, unit)
      if (.not.allocated(table)) then
        close(unit, status="delete")
      else
        close(unit)
      end if
    end if

    if (.not.allocated(table)) then
      table = toml_table()
    end if

    call get_project_deps(config, table, package, root, error)
    if (allocated(error)) return

    open(file=lock, newunit=unit)
    write(unit, '(a)') "# Dependency lock file generated by fpm"
    ser = toml_serializer(unit)
    call table%accept(ser)
    close(unit)

  end subroutine update_dep_lock

  !> Update dependencies for root project
  subroutine get_project_deps(config, table, package, root, error)
    !> Instance of the dependency handler
    class(dependency_walker_t), intent(in) :: config
    !> Table to collect all dependencies
    type(toml_table), intent(inout) :: table
    !> Root package configuration data
    type(package_config_t), intent(in) :: package
    !> Current project root directory
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    if (allocated(package%dependency)) then
      call get_deps(config, table, package%dependency, root, error)
      if (allocated(error)) return
    end if

    if (allocated(package%dev_dependency)) then
      call get_deps(config, table, package%dev_dependency, root, error)
      if (allocated(error)) return
    end if

    if (allocated(package%executable)) then
      call get_executable_deps(config, table, package%executable, root, error)
      if (allocated(error)) return
    end if

    if (allocated(package%test)) then
      call get_executable_deps(config, table, package%test, root, error)
      if (allocated(error)) return
    end if
  end subroutine get_project_deps

  !> Handle dependencies within a list of executables
  subroutine get_executable_deps(config, table, executable, root, error)
    !> Table to collect all dependencies
    type(toml_table), intent(inout) :: table
    !> Array of executables with local dependencies
    class(executable_config_t), intent(in) :: executable(:)
    !> Current project root directory
    character(len=*), intent(in) :: root
    !> Instance of the dependency handler
    class(dependency_walker_t), intent(in) :: config
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    integer :: ii

    do ii = 1, size(executable)
      if (allocated(executable(ii)%dependency)) then
        call get_deps(config, table, executable(ii)%dependency, root, error)
        if (allocated(error)) exit
      end if
    end do
    if (allocated(error)) return

  end subroutine get_executable_deps

  !> Handle a list of dependencies
  recursive subroutine get_deps(config, table, dependency, root, error)
    !> Table to collect all dependencies
    type(toml_table), intent(inout) :: table
    !> Array of all dependencies
    type(dependency_config_t), intent(in) :: dependency(:)
    !> Current project root directory
    character(len=*), intent(in) :: root
    !> Instance of the dependency handler
    class(dependency_walker_t), intent(in) :: config
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    integer :: ii

    do ii = 1, size(dependency)
      call get_dep(config, table, dependency(ii), root, error)
      if (allocated(error)) exit
    end do
    if (allocated(error)) return

  end subroutine get_deps

  !> Fetch a dependency and create its package manifest
  recursive subroutine get_dep(config, table, dependency, root, error)
    !> Instance of the dependency handler
    class(dependency_walker_t), intent(in) :: config
    !> Error handling
    !> Table to collect all dependencies
    type(toml_table), intent(inout) :: table
    !> Instance of the dependency data
    type(dependency_config_t), intent(in) :: dependency
    !> Current project root directory
    character(len=*), intent(in) :: root
    type(error_t), allocatable, intent(out) :: error

    type(package_config_t) :: package
    character(len=:), allocatable :: manifest, project_dir
    logical :: done, fetch

    if (allocated(dependency%git)) then
      project_dir = join_path(config%prefix, dependency%name)
      manifest = join_path(project_dir, fpm_manifest_file)
      fetch = .not.exists(manifest)
      if (check_require_refetch(config%policy)) then
        fetch = fetch .or. require_refetch(config, table, dependency)
      end if
      if (fetch) then
        call dependency%git%checkout(project_dir, error)
        if (allocated(error)) return
      end if
    else if (allocated(dependency%path)) then
      project_dir = join_path(root, dependency%path)
      manifest = join_path(project_dir, fpm_manifest_file)
    end if

    if (allocated(manifest)) then
      call get_package_data(package, manifest, error)
      if (allocated(error)) return
      deallocate(manifest)
    else
      call fatal_error(error, "Dependency resolution failed for "//dependency%name)
      return
    end if

    call record_dep(config, table, package, project_dir, dependency%git, &
      done, error)
    if (allocated(error)) return
    if (done) return

    if (allocated(package%dependency)) then
      call get_deps(config, table, package%dependency, project_dir, error)
    end if

  end subroutine get_dep

  !> Policy check for refetching remote dependencies
  elemental function check_require_refetch(policy) result(check)
    !> Current update policy
    integer, intent(in) :: policy
    !> Perform check for refetching remote
    logical :: check

    check = policy == update_policy%update &
      .or.  policy == update_policy%force_update

  end function check_require_refetch

  !> Register a new package in the dependency lock
  subroutine record_dep(config, table, package, root, git, done, error)
    !> Instance of the dependency handler
    class(dependency_walker_t), intent(in) :: config
    !> Table to collect all dependencies
    type(toml_table), intent(inout) :: table
    !> Package configuration data
    type(package_config_t), intent(in) :: package
    !> Current project root directory
    character(len=*), intent(in) :: root
    !> Git repository information
    type(git_target_t), intent(in), optional :: git
    !> Dependency already recorded
    logical, intent(out) :: done
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_table), pointer :: dep
    character(len=:), allocatable :: version, rev

    done = table%has_key(package%name)
    if (done) then
      call get_value(table, package%name, dep)
      call get_value(dep, "required", done, .false.)
      call set_value(dep, "required", .true.)
    else
      call add_table(table, package%name, dep)
    end if
    call set_value(dep, "version", package%version, error)
    if (allocated(error)) return
    call set_value(dep, "path", root)
    if (present(git)) then
      call set_value(dep, "git-url", git%url)
      call git_revision(root, rev, error)
      if (allocated(error)) return
      call set_value(dep, "git-obj", rev)
    end if

  end subroutine record_dep

  !> Check if we have to fetch a version controlled dependency.
  function require_refetch(config, table, dependency) result(fetch)
    !> Instance of the dependency handler
    class(dependency_walker_t), intent(in) :: config
    !> Table to collect all dependencies
    type(toml_table), intent(inout) :: table
    !> Instance of the dependency data
    type(dependency_config_t), intent(in) :: dependency
    !> Refetch a remote dependency
    logical :: fetch

    type(toml_table), pointer :: ptr
    character(len=:), allocatable :: url, obj

    if (config%policy /= update_policy%force_update) then
      if (.not.is_selected_dep(config%update, dependency)) then
        fetch = .false.
        return
      end if
    end if

    ! If the dependency is not registered yet, we have to fetch it
    fetch = .not.table%has_key(dependency%name)
    if (fetch) return

    call get_value(table, dependency%name, ptr)

    ! Actual dependency is a git dependency
    if (allocated(dependency%git)) then
      ! In case no specific version is pinned, we always refetch
      fetch = .not.allocated(dependency%git%object)
      if (fetch .and. config%verbosity > 1) then
        write(config%unit, '("#", *(1x, a))') &
          "Fetch:", dependency%name, "revision", "HEAD"
      end if
      if (fetch) return
      call get_value(ptr, "git-obj", obj)
      if (allocated(obj)) then
        ! In case the object specs don't match, we will refetch,
        ! this will always refetch branches *and* tags (see explanation above),
        ! also partial commit hashs will not pass this test for now.
        fetch = obj /= dependency%git%object
        if (fetch .and. config%verbosity > 1) then
          write(config%unit, '("#", *(1x, a))') &
            "Fetch:", dependency%name, "revision", obj, &
            "->", dependency%git%object
        end if
        if (fetch) return
      end if
    end if

    call get_value(ptr, "git-url", url)
    ! Cached dependency is a git dependency
    if (allocated(url)) then
      ! Actual dependency is a git dependency as well
      if (allocated(dependency%git)) then
        ! Always update if remote URL changed
        fetch = url /= dependency%git%url
        if (fetch .and. config%verbosity > 1) then
          write(config%unit, '("#", *(1x, a))') &
            "Fetch:", dependency%name, "url", url, &
            "->", dependency%git%url
        end if
        if (fetch) return
      end if
    else
      ! Cached dependency is not a git dependency, but the actual dependency is
      fetch = allocated(dependency%git)
      if (fetch .and. config%verbosity > 1) then
        write(config%unit, '("#", *(1x, a))') &
          "Fetch:", dependency%name, "from", dependency%git%url
      end if
      if (fetch) return
    end if

  end function require_refetch

  !> Check if a dependency is selected for updating
  pure function is_selected_dep(update, dependency) result(selected)
    !> Names of all selected dependencies
    type(update_name), intent(in) :: update(:)
    !> Current dependency
    type(dependency_config_t), intent(in) :: dependency
    !> Check dependency for updates
    logical :: selected

    integer :: ii

    selected = .false.
    do ii = 1, size(update)
      selected = selected .or. dependency%name == update(ii)%dep
      if (selected) exit
    end do

  end function is_selected_dep

  !> Try to retrieve the dependency lock, invalid lock files are dropped
  subroutine get_dependency_lock(table, unit)
    !> TOML data structure
    type(toml_table), allocatable, intent(out) :: table
    !> Formatted unit connected to dependency lock file
    integer, intent(in) :: unit

    type(toml_table), pointer :: ptr
    type(toml_error), allocatable :: parse_error
    type(toml_key), allocatable :: list(:)
    integer :: ii
    logical :: done

    call toml_parse(table, unit, parse_error)

    if (allocated(parse_error)) then
      if (allocated(table)) then
        call table%destroy
        deallocate(table)
      end if
    end if

    if (allocated(table)) then
      call table%get_keys(list)

      do ii = 1, size(list)
        call get_value(table, list(ii)%key, ptr)
        call set_value(ptr, "required", .false.)
      end do
    end if

  end subroutine get_dependency_lock

  !> Update all locked dependencies
  subroutine update_lock(table, error)
    !> Table containing all dependencies
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii
    type(toml_key), allocatable :: list(:)

    call table%get_keys(list)

    do ii = 1, size(list)
      print'(a)', list(ii)%key
    end do

  end subroutine update_lock

end module fpm_dependency
