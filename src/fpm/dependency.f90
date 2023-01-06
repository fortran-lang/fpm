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
!> For remote dependencies we have to refetch if the revision in the manifest
!> changes or the upstream HEAD has changed (for branches _and_ tags).
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
  use, intrinsic :: iso_fortran_env, only: output_unit
  use fpm_environment, only: get_os_type, OS_WINDOWS
  use fpm_error, only: error_t, fatal_error
  use fpm_filesystem, only: exists, join_path, mkdir, canon_path, windows_path
  use fpm_git, only: git_target_revision, git_target_default, git_revision
  use fpm_manifest, only: package_config_t, dependency_config_t, &
                          get_package_data
  use fpm_strings, only: string_t, operator(.in.)
  use fpm_toml, only: toml_table, toml_key, toml_error, toml_serializer, &
                      toml_parse, get_value, set_value, add_table
  use fpm_versioning, only: version_t, new_version, char
  implicit none
  private

  public :: dependency_tree_t, new_dependency_tree
  public :: dependency_node_t, new_dependency_node
  public :: resize

  !> Overloaded reallocation interface
  interface resize
    module procedure :: resize_dependency_node
  end interface resize

  !> Dependency node in the projects dependency tree
  type, extends(dependency_config_t) :: dependency_node_t
    !> Actual version of this dependency
    type(version_t), allocatable :: version
    !> Installation prefix of this dependencies
    character(len=:), allocatable :: proj_dir
    !> Checked out revision of the version control system
    character(len=:), allocatable :: revision
    !> Dependency is handled
    logical :: done = .false.
    !> Dependency should be updated
    logical :: update = .false.
  contains
    !> Update dependency from project manifest
    procedure :: register
  end type dependency_node_t

  !> Respresentation of a projects dependencies
  !>
  !> The dependencies are stored in a simple array for now, this can be replaced
  !> with a binary-search tree or a hash table in the future.
  type :: dependency_tree_t
    !> Unit for IO
    integer :: unit = output_unit
    !> Verbosity of printout
    integer :: verbosity = 1
    !> Installation prefix for dependencies
    character(len=:), allocatable :: dep_dir
    !> Number of currently registered dependencies
    integer :: ndep = 0
    !> Flattend list of all dependencies
    type(dependency_node_t), allocatable :: dep(:)
    !> Cache file
    character(len=:), allocatable :: cache
  contains
    !> Overload procedure to add new dependencies to the tree
    generic :: add => add_project, add_project_dependencies, add_dependencies, &
      add_dependency
    !> Main entry point to add a project
    procedure, private :: add_project
    !> Add a project and its dependencies to the dependency tree
    procedure, private :: add_project_dependencies
    !> Add a list of dependencies to the dependency tree
    procedure, private :: add_dependencies
    !> Add a single dependency to the dependency tree
    procedure, private :: add_dependency
    !> Resolve dependencies
    generic :: resolve => resolve_dependencies, resolve_dependency
    !> Resolve dependencies
    procedure, private :: resolve_dependencies
    !> Resolve dependencies
    procedure, private :: resolve_dependency
    !> Find a dependency in the tree
    generic :: find => find_dependency, find_name
    !> Find a dependency from an dependency configuration
    procedure, private :: find_dependency
    !> Find a dependency by its name
    procedure, private :: find_name
    !> Depedendncy resolution finished
    procedure :: finished
    !> Reading of dependency tree
    generic :: load => load_from_file, load_from_unit, load_from_toml
    !> Read dependency tree from file
    procedure, private :: load_from_file
    !> Read dependency tree from formatted unit
    procedure, private :: load_from_unit
    !> Read dependency tree from TOML data structure
    procedure, private :: load_from_toml
    !> Writing of dependency tree
    generic :: dump => dump_to_file, dump_to_unit, dump_to_toml
    !> Write dependency tree to file
    procedure, private :: dump_to_file
    !> Write dependency tree to formatted unit
    procedure, private :: dump_to_unit
    !> Write dependency tree to TOML data structure
    procedure, private :: dump_to_toml
    !> Update dependency tree
    generic :: update => update_dependency
    !> Update a list of dependencies
    procedure, private :: update_dependency
  end type dependency_tree_t

  !> Common output format for writing to the command line
  character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

contains

  !> Create a new dependency tree
  subroutine new_dependency_tree(self, verbosity, cache)
    !> Instance of the dependency tree
    type(dependency_tree_t), intent(out) :: self
    !> Verbosity of printout
    integer, intent(in), optional :: verbosity
    !> Name of the cache file
    character(len=*), intent(in), optional :: cache

    call resize(self%dep)
    self%dep_dir = join_path("build", "dependencies")

    if (present(verbosity)) then
      self%verbosity = verbosity
    end if

    if (present(cache)) then
      self%cache = cache
    end if

  end subroutine new_dependency_tree

  !> Create a new dependency node from a configuration
  pure subroutine new_dependency_node(self, dependency, version, proj_dir, update)
    !> Instance of the dependency node
    type(dependency_node_t), intent(out) :: self
    !> Dependency configuration data
    type(dependency_config_t), intent(in) :: dependency
    !> Version of the dependency
    type(version_t), intent(in), optional :: version
    !> Installation prefix of the dependency
    character(len=*), intent(in), optional :: proj_dir
    !> Dependency should be updated
    logical, intent(in), optional :: update

    self%dependency_config_t = dependency

    if (present(version)) then
      self%version = version
    end if

    if (present(proj_dir)) then
      self%proj_dir = proj_dir
    end if

    if (present(update)) then
      self%update = update
    end if

  end subroutine new_dependency_node

  !> Add project dependencies, each depth level after each other.
  !>
  !> We implement this algorithm in an interative rather than a recursive fashion
  !> as a choice of design.
  subroutine add_project(self, package, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Project configuration to add
    type(package_config_t), intent(in) :: package
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(dependency_config_t) :: dependency
    character(len=:), allocatable :: root

    if (allocated(self%cache)) then
      call self%load(self%cache, error)
      if (allocated(error)) return
    end if

    if (.not. exists(self%dep_dir)) then
      call mkdir(self%dep_dir)
    end if

    root = "."

    ! Create this project as the first dependency node (depth 0)
    dependency%name = package%name
    dependency%path = root
    call self%add(dependency, error)
    if (allocated(error)) return

    ! Resolve the root project
    call self%resolve(root, error)
    if (allocated(error)) return

    ! Add the root project dependencies (depth 1)
    call self%add(package, root, .true., error)
    if (allocated(error)) return

    ! Now decent into the dependency tree, level for level
    do while (.not. self%finished())
      call self%resolve(root, error)
      if (allocated(error)) exit
    end do
    if (allocated(error)) return

    if (allocated(self%cache)) then
      call self%dump(self%cache, error)
      if (allocated(error)) return
    end if

  end subroutine add_project

  !> Add a project and its dependencies to the dependency tree
  recursive subroutine add_project_dependencies(self, package, root, main, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Project configuration to add
    type(package_config_t), intent(in) :: package
    !> Current project root directory
    character(len=*), intent(in) :: root
    !> Is the main project
    logical, intent(in) :: main
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii

    if (allocated(package%dependency)) then
      call self%add(package%dependency, error)
      if (allocated(error)) return
    end if

    if (main) then
      if (allocated(package%dev_dependency)) then
        call self%add(package%dev_dependency, error)
        if (allocated(error)) return
      end if

      if (allocated(package%executable)) then
        do ii = 1, size(package%executable)
          if (allocated(package%executable(ii)%dependency)) then
            call self%add(package%executable(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if

      if (allocated(package%example)) then
        do ii = 1, size(package%example)
          if (allocated(package%example(ii)%dependency)) then
            call self%add(package%example(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if

      if (allocated(package%test)) then
        do ii = 1, size(package%test)
          if (allocated(package%test(ii)%dependency)) then
            call self%add(package%test(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if
    end if

  end subroutine add_project_dependencies

  !> Add a list of dependencies to the dependency tree
  subroutine add_dependencies(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_config_t), intent(in) :: dependency(:)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii, ndep

    ndep = size(self%dep)
    if (ndep < size(dependency) + self%ndep) then
      call resize(self%dep, ndep + ndep/2 + size(dependency))
    end if

    do ii = 1, size(dependency)
      call self%add(dependency(ii), error)
      if (allocated(error)) exit
    end do
    if (allocated(error)) return

  end subroutine add_dependencies

  !> Add a single dependency to the dependency tree
  pure subroutine add_dependency(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_config_t), intent(in) :: dependency
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: id

    id = self%find(dependency)
    if (id == 0) then
      self%ndep = self%ndep + 1
      call new_dependency_node(self%dep(self%ndep), dependency)
    end if

  end subroutine add_dependency

  !> Update dependency tree
  subroutine update_dependency(self, name, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Name of the dependency to update
    character(len=*), intent(in) :: name
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: id
    character(len=:), allocatable :: proj_dir, root

    id = self%find(name)
    root = "."

    if (id <= 0) then
      call fatal_error(error, "Cannot update dependency '"//name//"'")
      return
    end if

    associate (dep => self%dep(id))
      if (allocated(dep%git) .and. dep%update) then
        if (self%verbosity > 1) then
          write (self%unit, out_fmt) "Update:", dep%name
        end if
        proj_dir = join_path(self%dep_dir, dep%name)
        call dep%git%checkout(proj_dir, error)
        if (allocated(error)) return

        ! Unset dependency and remove updatable attribute
        dep%done = .false.
        dep%update = .false.

        ! Now decent into the dependency tree, level for level
        do while (.not. self%finished())
          call self%resolve(root, error)
          if (allocated(error)) exit
        end do
        if (allocated(error)) return
      end if
    end associate

  end subroutine update_dependency

  !> Resolve all dependencies in the tree
  subroutine resolve_dependencies(self, root, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii

    do ii = 1, self%ndep
      call self%resolve(self%dep(ii), root, error)
      if (allocated(error)) exit
    end do

    if (allocated(error)) return

  end subroutine resolve_dependencies

  !> Resolve a single dependency node
  subroutine resolve_dependency(self, dependency, root, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_node_t), intent(inout) :: dependency
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(package_config_t) :: package
    character(len=:), allocatable :: manifest, proj_dir, revision
    logical :: fetch

    if (dependency%done) return

    fetch = .false.
    if (allocated(dependency%proj_dir)) then
      proj_dir = dependency%proj_dir
    else
      if (allocated(dependency%path)) then
        proj_dir = join_path(root, dependency%path)
      else if (allocated(dependency%git)) then
        proj_dir = join_path(self%dep_dir, dependency%name)
        fetch = .not. exists(proj_dir)
        if (fetch) then
          call dependency%git%checkout(proj_dir, error)
          if (allocated(error)) return
        end if

      end if
    end if

    if (allocated(dependency%git)) then
      call git_revision(proj_dir, revision, error)
      if (allocated(error)) return
    end if

    manifest = join_path(proj_dir, "fpm.toml")
    call get_package_data(package, manifest, error)
    if (allocated(error)) return

    call dependency%register(package, proj_dir, fetch, revision, error)
    if (allocated(error)) return

    if (self%verbosity > 1) then
      write (self%unit, out_fmt) &
        "Dep:", dependency%name, "version", char(dependency%version), &
        "at", dependency%proj_dir
    end if

    call self%add(package, proj_dir, .false., error)
    if (allocated(error)) return

  end subroutine resolve_dependency

  !> Find a dependency in the dependency tree
  pure function find_dependency(self, dependency) result(pos)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> Dependency configuration to add
    class(dependency_config_t), intent(in) :: dependency
    !> Index of the dependency
    integer :: pos

    pos = self%find(dependency%name)

  end function find_dependency

  !> Find a dependency in the dependency tree
  pure function find_name(self, name) result(pos)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> Dependency configuration to add
    character(len=*), intent(in) :: name
    !> Index of the dependency
    integer :: pos

    integer :: ii

    pos = 0
    do ii = 1, self%ndep
      if (name == self%dep(ii)%name) then
        pos = ii
        exit
      end if
    end do

  end function find_name

  !> Check if we are done with the dependency resolution
  pure function finished(self)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> All dependencies are updated
    logical :: finished

    finished = all(self%dep(:self%ndep)%done)

  end function finished

  !> Update dependency from project manifest
  subroutine register(self, package, root, fetch, revision, error)
    !> Instance of the dependency node
    class(dependency_node_t), intent(inout) :: self
    !> Package configuration data
    type(package_config_t), intent(in) :: package
    !> Project has been fetched
    logical, intent(in) :: fetch
    !> Root directory of the project
    character(len=*), intent(in) :: root
    !> Git revision of the project
    character(len=*), intent(in), optional :: revision
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    logical :: update

    update = .false.
    if (self%name /= package%name) then
      call fatal_error(error, "Dependency name '"//package%name// &
        & "' found, but expected '"//self%name//"' instead")
    end if

    self%version = package%version
    self%proj_dir = root

    if (allocated(self%git) .and. present(revision)) then
      self%revision = revision
      if (.not. fetch) then
        ! git object is HEAD always allows an update
        update = .not. allocated(self%git%object)
        if (.not. update) then
          ! allow update in case the revision does not match the requested object
          update = revision /= self%git%object
        end if
      end if
    end if

    self%update = update
    self%done = .true.

  end subroutine register

  !> Read dependency tree from file
  subroutine load_from_file(self, file, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    character(len=*), intent(in) :: file
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: unit
    logical :: exist

    inquire (file=file, exist=exist)
    if (.not. exist) return

    open (file=file, newunit=unit)
    call self%load(unit, error)
    close (unit)
  end subroutine load_from_file

  !> Read dependency tree from file
  subroutine load_from_unit(self, unit, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    integer, intent(in) :: unit
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_error), allocatable :: parse_error
    type(toml_table), allocatable :: table

    call toml_parse(table, unit, parse_error)

    if (allocated(parse_error)) then
      allocate (error)
      call move_alloc(parse_error%message, error%message)
      return
    end if

    call self%load(table, error)
    if (allocated(error)) return

  end subroutine load_from_unit

  !> Read dependency tree from TOML data structure
  subroutine load_from_toml(self, table, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ndep, ii
    logical :: unix
    character(len=:), allocatable :: version, url, obj, rev, proj_dir
    type(toml_key), allocatable :: list(:)
    type(toml_table), pointer :: ptr

    call table%get_keys(list)

    ndep = size(self%dep)
    if (ndep < size(list) + self%ndep) then
      call resize(self%dep, ndep + ndep/2 + size(list))
    end if

    unix = get_os_type() /= OS_WINDOWS

    do ii = 1, size(list)
      call get_value(table, list(ii)%key, ptr)
      call get_value(ptr, "version", version)
      call get_value(ptr, "proj-dir", proj_dir)
      call get_value(ptr, "git", url)
      call get_value(ptr, "obj", obj)
      call get_value(ptr, "rev", rev)
      if (.not. allocated(proj_dir)) cycle
      self%ndep = self%ndep + 1
      associate (dep => self%dep(self%ndep))
        dep%name = list(ii)%key
        if (unix) then
          dep%proj_dir = proj_dir
        else
          dep%proj_dir = windows_path(proj_dir)
        end if
        dep%done = .false.
        if (allocated(version)) then
          if (.not. allocated(dep%version)) allocate (dep%version)
          call new_version(dep%version, version, error)
          if (allocated(error)) exit
        end if
        if (allocated(version)) then
          call new_version(dep%version, version, error)
          if (allocated(error)) exit
        end if
        if (allocated(url)) then
          if (allocated(obj)) then
            dep%git = git_target_revision(url, obj)
          else
            dep%git = git_target_default(url)
          end if
          if (allocated(rev)) then
            dep%revision = rev
          end if
        else
          dep%path = proj_dir
        end if
      end associate
    end do
    if (allocated(error)) return

    self%ndep = size(list)
  end subroutine load_from_toml

  !> Write dependency tree to file
  subroutine dump_to_file(self, file, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    character(len=*), intent(in) :: file
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: unit

    open (file=file, newunit=unit)
    call self%dump(unit, error)
    close (unit)
    if (allocated(error)) return

  end subroutine dump_to_file

  !> Write dependency tree to file
  subroutine dump_to_unit(self, unit, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Formatted unit
    integer, intent(in) :: unit
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(toml_serializer) :: ser

    table = toml_table()
    ser = toml_serializer(unit)

    call self%dump(table, error)

    call table%accept(ser)

  end subroutine dump_to_unit

  !> Write dependency tree to TOML datastructure
  subroutine dump_to_toml(self, table, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii
    type(toml_table), pointer :: ptr
    character(len=:), allocatable :: proj_dir

    do ii = 1, self%ndep
      associate (dep => self%dep(ii))
        call add_table(table, dep%name, ptr)
        if (.not. associated(ptr)) then
          call fatal_error(error, "Cannot create entry for "//dep%name)
          exit
        end if
        if (allocated(dep%version)) then
          call set_value(ptr, "version", char(dep%version))
        end if
        proj_dir = canon_path(dep%proj_dir)
        call set_value(ptr, "proj-dir", proj_dir)
        if (allocated(dep%git)) then
          call set_value(ptr, "git", dep%git%url)
          if (allocated(dep%git%object)) then
            call set_value(ptr, "obj", dep%git%object)
          end if
          if (allocated(dep%revision)) then
            call set_value(ptr, "rev", dep%revision)
          end if
        end if
      end associate
    end do
    if (allocated(error)) return

  end subroutine dump_to_toml

  !> Reallocate a list of dependencies
  pure subroutine resize_dependency_node(var, n)
    !> Instance of the array to be resized
    type(dependency_node_t), allocatable, intent(inout) :: var(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(dependency_node_t), allocatable :: tmp(:)
    integer :: this_size, new_size
    integer, parameter :: initial_size = 16

    if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
    else
      this_size = initial_size
    end if

    if (present(n)) then
      new_size = n
    else
      new_size = this_size + this_size/2 + 1
    end if

    allocate (var(new_size))

    if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate (tmp)
    end if

  end subroutine resize_dependency_node

end module fpm_dependency
