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
  use fpm_environment, only: get_os_type, OS_WINDOWS, os_is_unix
  use fpm_error, only: error_t, fatal_error
  use fpm_filesystem, only: exists, join_path, mkdir, canon_path, windows_path, list_files, is_dir, basename, os_delete_dir
  use fpm_git, only: git_target_revision, git_target_default, git_revision, operator(==)
  use fpm_manifest, only: package_config_t, dependency_config_t, get_package_data
  use fpm_manifest_dependency, only: manifest_has_changed
  use fpm_strings, only: string_t, operator(.in.)
  use fpm_toml, only: toml_table, toml_key, toml_error, toml_serialize, &
                      get_value, set_value, add_table, toml_load, toml_stat
  use fpm_versioning, only: version_t, new_version
  use fpm_settings, only: fpm_global_settings, get_global_settings, official_registry_base_url
  use fpm_downloader, only: downloader_t
  use jonquil, only: json_object
  use fpm_strings, only: str
  implicit none
  private

  public :: dependency_tree_t, new_dependency_tree, dependency_node_t, new_dependency_node, resize, &
            & check_and_read_pkg_data

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
    !> Dependency was loaded from a cache
    logical :: cached = .false.
  contains
    !> Update dependency from project manifest.
    procedure :: register
    !> Get dependency from the registry.
    procedure :: get_from_registry
    procedure, private :: get_from_local_registry
    !> Print information on this instance
    procedure :: info
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
      add_dependency, add_dependency_node
    !> Main entry point to add a project
    procedure, private :: add_project
    !> Add a project and its dependencies to the dependency tree
    procedure, private :: add_project_dependencies
    !> Add a list of dependencies to the dependency tree
    procedure, private :: add_dependencies
    !> Add a single dependency to the dependency tree
    procedure, private :: add_dependency
    !> Add a single dependency node to the dependency tree
    procedure, private :: add_dependency_node
    !> Resolve dependencies
    generic :: resolve => resolve_dependencies, resolve_dependency
    !> Resolve dependencies
    procedure, private :: resolve_dependencies
    !> Resolve dependency
    procedure, private :: resolve_dependency
    !> True if entity can be found
    generic :: has => has_dependency
    !> True if dependency is part of the tree
    procedure, private :: has_dependency
    !> Find a dependency in the tree
    generic :: find => find_name
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
    generic :: update => update_dependency, update_tree
    !> Update a list of dependencies
    procedure, private :: update_dependency
    !> Update all dependencies in the tree
    procedure, private :: update_tree
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
  subroutine new_dependency_node(self, dependency, version, proj_dir, update)
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

  !> Write information on instance
  subroutine info(self, unit, verbosity)

    !> Instance of the dependency configuration
    class(dependency_node_t), intent(in) :: self

    !> Unit for IO
    integer, intent(in) :: unit

    !> Verbosity of the printout
    integer, intent(in), optional :: verbosity

    integer :: pr
    character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

    if (present(verbosity)) then
      pr = verbosity
    else
      pr = 1
    end if

    !> Call base object info
    call self%dependency_config_t%info(unit, pr)

    if (allocated(self%version)) then
      write (unit, fmt) "- version", self%version%s()
    end if

    if (allocated(self%proj_dir)) then
      write (unit, fmt) "- dir", self%proj_dir
    end if

    if (allocated(self%revision)) then
      write (unit, fmt) "- revision", self%revision
    end if

    write (unit, fmt) "- done", merge('YES', 'NO ', self%done)
    write (unit, fmt) "- update", merge('YES', 'NO ', self%update)

  end subroutine info

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
    type(dependency_tree_t) :: cached
    character(len=*), parameter :: root = '.'
    integer :: id

    if (.not. exists(self%dep_dir)) then
      call mkdir(self%dep_dir)
    end if

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

    ! After resolving all dependencies, check if we have cached ones to avoid updates
    if (allocated(self%cache)) then
      call new_dependency_tree(cached, verbosity=2,cache=self%cache)
      call cached%load(self%cache, error)
      if (allocated(error)) return

      ! Skip root node
      do id=2,cached%ndep
          cached%dep(id)%cached = .true.
          call self%add(cached%dep(id), error)
          if (allocated(error)) return
      end do
    end if

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

  !> Add a single dependency node to the dependency tree
  !> Dependency nodes contain additional information (version, git, revision)
  subroutine add_dependency_node(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_node_t), intent(in) :: dependency
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: id

    if (self%has_dependency(dependency)) then
      ! A dependency with this same name is already in the dependency tree.
      ! Check if it needs to be updated
      id = self%find(dependency%name)

      ! If this dependency was in the cache, and we're now requesting a different version
      ! in the manifest, ensure it is marked for update. Otherwise, if we're just querying
      ! the same dependency from a lower branch of the dependency tree, the existing one from
      ! the manifest has priority
      if (dependency%cached) then
        if (dependency_has_changed(dependency, self%dep(id), self%verbosity, self%unit)) then
           if (self%verbosity>0) write (self%unit, out_fmt) "Dependency change detected:", dependency%name
           self%dep(id)%update = .true.
        else
           ! Store the cached one
           self%dep(id) = dependency
           self%dep(id)%update = .false.
        endif
      end if
    else
      ! New dependency: add from scratch
      self%ndep = self%ndep + 1
      self%dep(self%ndep) = dependency
    end if

  end subroutine add_dependency_node

  !> Add a single dependency to the dependency tree
  subroutine add_dependency(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_config_t), intent(in) :: dependency
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node

    call new_dependency_node(node, dependency)
    call add_dependency_node(self, node, error)

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
        if (self%verbosity>0) write (self%unit, out_fmt) "Update:", dep%name
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

  !> Update whole dependency tree
  subroutine update_tree(self, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: i

    ! Update dependencies where needed
    do i = 1, self%ndep
      call self%update(self%dep(i)%name, error)
      if (allocated(error)) return
    end do

  end subroutine update_tree

  !> Resolve all dependencies in the tree
  subroutine resolve_dependencies(self, root, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(fpm_global_settings) :: global_settings
    integer :: ii

    call get_global_settings(global_settings, error)
    if (allocated(error)) return

    do ii = 1, self%ndep
      call self%resolve(self%dep(ii), global_settings, root, error)
      if (allocated(error)) exit
    end do

    if (allocated(error)) return

  end subroutine resolve_dependencies

  !> Resolve a single dependency node
  subroutine resolve_dependency(self, dependency, global_settings, root, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_node_t), intent(inout) :: dependency
    !> Global configuration settings.
    type(fpm_global_settings), intent(in) :: global_settings
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
    else if (allocated(dependency%path)) then
      proj_dir = join_path(root, dependency%path)
    else if (allocated(dependency%git)) then
      proj_dir = join_path(self%dep_dir, dependency%name)
      fetch = .not. exists(proj_dir)
      if (fetch) then
        call dependency%git%checkout(proj_dir, error)
        if (allocated(error)) return
      end if
    else
      call dependency%get_from_registry(proj_dir, global_settings, error)
      if (allocated(error)) return
    end if

    if (allocated(dependency%git)) then
      call git_revision(proj_dir, revision, error)
      if (allocated(error)) return
    end if

    manifest = join_path(proj_dir, "fpm.toml")
    call get_package_data(package, manifest, error)
    if (allocated(error)) return

    print *, 'dependency',dependency%name,': fetch=',fetch,' allocated(git)=',allocated(dependency%git)
    print *, ' proj_dir=',proj_dir,' fetch=',fetch


    call dependency%register(package, proj_dir, fetch, revision, error)
    if (allocated(error)) return

    if (self%verbosity > 1) then
      write (self%unit, out_fmt) &
        "Dep:", dependency%name, "version", dependency%version%s(), &
        "at", dependency%proj_dir
    end if

    call self%add(package, proj_dir, .false., error)
    if (allocated(error)) return

  end subroutine resolve_dependency

  !> Get a dependency from the registry. Whether the dependency is fetched
  !> from a local, a custom remote or the official registry is determined
  !> by the global configuration settings.
  subroutine get_from_registry(self, target_dir, global_settings, error, downloader_)

    !> Instance of the dependency configuration.
    class(dependency_node_t), intent(in) :: self

    !> The target directory of the dependency.
    character(:), allocatable, intent(out) :: target_dir

    !> Global configuration settings.
    type(fpm_global_settings), intent(in) :: global_settings

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    !> Downloader instance.
    class(downloader_t), optional, intent(in) :: downloader_

    character(:), allocatable :: cache_path, target_url, tmp_pkg_path, tmp_pkg_file
    type(version_t) :: version
    integer :: stat, unit
    type(json_object) :: json
    class(downloader_t), allocatable :: downloader

    if (present(downloader_)) then
      downloader = downloader_
    else
      allocate (downloader)
    end if

    ! Use local registry if it was specified in the global config file.
    if (allocated(global_settings%registry_settings%path)) then
      call self%get_from_local_registry(target_dir, global_settings%registry_settings%path, error); return
    end if

    ! Include namespace and package name in the cache path.
    cache_path = join_path(global_settings%registry_settings%cache_path, self%namespace, self%name)

    ! Check cache before downloading from the remote registry if a specific version was requested. When no specific
    ! version was requested, do network request first to check which is the newest version.
    if (allocated(self%requested_version)) then
      if (exists(join_path(cache_path, self%requested_version%s(), 'fpm.toml'))) then
        print *, "Using cached version of '", join_path(self%namespace, self%name, self%requested_version%s()), "'."
        target_dir = join_path(cache_path, self%requested_version%s()); return
      end if
    end if

    ! Define location of the temporary folder and file.
    tmp_pkg_path = join_path(global_settings%path_to_config_folder, 'tmp')
    tmp_pkg_file = join_path(tmp_pkg_path, 'package_data.tmp')
    if (.not. exists(tmp_pkg_path)) call mkdir(tmp_pkg_path)
    open (newunit=unit, file=tmp_pkg_file, action='readwrite', iostat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Error creating temporary file for downloading package '"//self%name//"'."); return
    end if

    ! Include namespace and package name in the target url and download package data.
    target_url = global_settings%registry_settings%url//'/packages/'//self%namespace//'/'//self%name
    call downloader%get_pkg_data(target_url, self%requested_version, tmp_pkg_file, json, error)
    close (unit, status='delete')
    if (allocated(error)) return

    ! Verify package data and read relevant information.
    call check_and_read_pkg_data(json, self, target_url, version, error)
    if (allocated(error)) return

    ! Open new tmp file for downloading the actual package.
    open (newunit=unit, file=tmp_pkg_file, action='readwrite', iostat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Error creating temporary file for downloading package '"//self%name//"'."); return
    end if

    ! Include version number in the cache path. If no cached version exists, download it.
    cache_path = join_path(cache_path, version%s())
    if (.not. exists(join_path(cache_path, 'fpm.toml'))) then
      if (is_dir(cache_path)) call os_delete_dir(os_is_unix(), cache_path)
      call mkdir(cache_path)

      print *, "Downloading '"//join_path(self%namespace, self%name, version%s())//"' ..."
      call downloader%get_file(target_url, tmp_pkg_file, error)
      if (allocated(error)) then
        close (unit, status='delete'); return
      end if

      ! Unpack the downloaded package to the final location.
      call downloader%unpack(tmp_pkg_file, cache_path, error)
      close (unit, status='delete')
      if (allocated(error)) return
    end if

    target_dir = cache_path

  end subroutine get_from_registry

  subroutine check_and_read_pkg_data(json, node, download_url, version, error)
    type(json_object), intent(inout) :: json
    class(dependency_node_t), intent(in) :: node
    character(:), allocatable, intent(out) :: download_url
    type(version_t), intent(out) :: version
    type(error_t), allocatable, intent(out) :: error

    integer :: code, stat
    type(json_object), pointer :: p, q
    character(:), allocatable :: version_key, version_str, error_message

    if (.not. json%has_key('code')) then
      call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"': No status code."); return
    end if

    call get_value(json, 'code', code, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"': "// &
      & "Failed to read status code."); return
    end if

    if (code /= 200) then
      if (.not. json%has_key('message')) then
        call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"': No error message."); return
      end if

      call get_value(json, 'message', error_message, stat=stat)
      if (stat /= 0) then
        call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"': "// &
        & "Failed to read error message."); return
      end if

      call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"'. Status code: '"// &
      & str(code)//"'. Error message: '"//error_message//"'."); return
    end if

    if (.not. json%has_key('data')) then
      call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"': No data."); return
    end if

    call get_value(json, 'data', p, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to read package data for '"//join_path(node%namespace, node%name)//"'."); return
    end if

    if (allocated(node%requested_version)) then
      version_key = 'version_data'
    else
      version_key = 'latest_version_data'
    end if

    if (.not. p%has_key(version_key)) then
      call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"': No version data."); return
    end if

    call get_value(p, version_key, q, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to retrieve version data for '"//join_path(node%namespace, node%name)//"'."); return
    end if

    if (.not. q%has_key('download_url')) then
      call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"': No download url."); return
    end if

    call get_value(q, 'download_url', download_url, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to read download url for '"//join_path(node%namespace, node%name)//"'."); return
    end if

    download_url = official_registry_base_url//'/'//download_url

    if (.not. q%has_key('version')) then
      call fatal_error(error, "Failed to download '"//join_path(node%namespace, node%name)//"': No version found."); return
    end if

    call get_value(q, 'version', version_str, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to read version data for '"//join_path(node%namespace, node%name)//"'."); return
    end if

    call new_version(version, version_str, error)
    if (allocated(error)) then
      call fatal_error(error, "'"//version_str//"' is not a valid version for '"// &
      & join_path(node%namespace, node%name)//"'."); return
    end if
  end subroutine

  !> Get the dependency from a local registry.
  subroutine get_from_local_registry(self, target_dir, registry_path, error)

    !> Instance of the dependency configuration.
    class(dependency_node_t), intent(in) :: self

    !> The target directory to download the dependency to.
    character(:), allocatable, intent(out) :: target_dir

    !> The path to the local registry.
    character(*), intent(in) :: registry_path

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: path_to_name
    type(string_t), allocatable :: files(:)
    type(version_t), allocatable :: versions(:)
    type(version_t) :: version
    integer :: i

    path_to_name = join_path(registry_path, self%namespace, self%name)

    if (.not. exists(path_to_name)) then
      call fatal_error(error, "Dependency resolution of '"//self%name// &
      & "': Directory '"//path_to_name//"' doesn't exist."); return
    end if

    call list_files(path_to_name, files)
    if (size(files) == 0) then
      call fatal_error(error, "No versions of '"//self%name//"' found in '"//path_to_name//"'."); return
    end if

    ! Version requested, find it in the cache.
    if (allocated(self%requested_version)) then
      do i = 1, size(files)
        ! Identify directory that matches the version number.
        if (files(i)%s == join_path(path_to_name, self%requested_version%s()) .and. is_dir(files(i)%s)) then
          if (.not. exists(join_path(files(i)%s, 'fpm.toml'))) then
            call fatal_error(error, "'"//files(i)%s//"' is missing an 'fpm.toml' file."); return
          end if
          target_dir = files(i)%s; return
        end if
      end do
      call fatal_error(error, "Version '"//self%requested_version%s()//"' not found in '"//path_to_name//"'")
      return
    end if

    ! No specific version requested, therefore collect available versions.
    allocate (versions(0))
    do i = 1, size(files)
      if (is_dir(files(i)%s)) then
        call new_version(version, basename(files(i)%s), error)
        if (allocated(error)) return
        versions = [versions, version]
      end if
    end do

    if (size(versions) == 0) then
      call fatal_error(error, "No versions found in '"//path_to_name//"'"); return
    end if

    ! Find the latest version.
    version = versions(1)
    do i = 1, size(versions)
      if (versions(i) > version) version = versions(i)
    end do

    path_to_name = join_path(path_to_name, version%s())

    if (.not. exists(join_path(path_to_name, 'fpm.toml'))) then
      call fatal_error(error, "'"//path_to_name//"' is missing an 'fpm.toml' file."); return
    end if

    target_dir = path_to_name
  end subroutine get_from_local_registry

  !> True if dependency is part of the tree
  pure logical function has_dependency(self, dependency)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> Dependency configuration to check
    class(dependency_node_t), intent(in) :: dependency

    has_dependency = self%find(dependency%name) /= 0

  end function has_dependency

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
        ! Change in revision ID was checked already. Only update if git information is missing
        ! git object is HEAD always allows an update
        update = .not. allocated(self%git%object)
      end if
    end if

    if (update) self%update = update
    self%done = .true.

    print *, 'register: set '//self%name//' for update, has revision? ',present(revision),' fetch? ',fetch,' set update? ',self%update
    if (present(revision)) print *, ' git object=',self%git%object,' revision=',revision

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

    call toml_load(table, unit, error=parse_error)

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

    table = toml_table()
    call self%dump(table, error)

    write (unit, '(a)') toml_serialize(table)

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
          call set_value(ptr, "version", dep%version%s())
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

  !> Check if a dependency node has changed
  logical function dependency_has_changed(cached, manifest, verbosity, iunit) result(has_changed)
    !> Two instances of the same dependency to be compared
    type(dependency_node_t), intent(in) :: cached, manifest

    !> Log verbosity
    integer, intent(in) :: verbosity, iunit

    has_changed = .true.

    !> All the following entities must be equal for the dependency to not have changed
    if (manifest_has_changed(cached=cached, manifest=manifest, verbosity=verbosity, iunit=iunit)) return

    !> For now, only perform the following checks if both are available. A dependency in cache.toml
    !> will always have this metadata; a dependency from fpm.toml which has not been fetched yet
    !> may not have it
    if (allocated(cached%version) .and. allocated(manifest%version)) then
      if (cached%version /= manifest%version) then
         if (verbosity>1) write(iunit,out_fmt) "VERSION has changed: "//cached%version%s()//" vs. "//manifest%version%s()
         return
      endif
    else
       if (verbosity>1) write(iunit,out_fmt) "VERSION has changed presence "
    end if
    if (allocated(cached%revision) .and. allocated(manifest%revision)) then
      if (cached%revision /= manifest%revision) then
        if (verbosity>1) write(iunit,out_fmt) "REVISION has changed: "//cached%revision//" vs. "//manifest%revision
        return
      endif
    else
      if (verbosity>1) write(iunit,out_fmt) "REVISION has changed presence "
    end if
    if (allocated(cached%proj_dir) .and. allocated(manifest%proj_dir)) then
      if (cached%proj_dir /= manifest%proj_dir) then
        if (verbosity>1) write(iunit,out_fmt) "PROJECT DIR has changed: "//cached%proj_dir//" vs. "//manifest%proj_dir
        return
      endif
    else
      if (verbosity>1) write(iunit,out_fmt) "PROJECT DIR has changed presence "
    end if

    !> All checks passed: the two dependencies have no differences
    has_changed = .false.

  end function dependency_has_changed

end module fpm_dependency
