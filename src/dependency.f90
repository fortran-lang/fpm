!> Implementation of the meta data for dependencies.
!>
!> A dependency table can currently have the following fields
!>
!>```toml
!>[dependencies]
!>"dep1" = { git = "url" }
!>"dep2" = { git = "url", branch = "name" }
!>"dep3" = { git = "url", tag = "name" }
!>"dep4" = { git = "url", rev = "sha1" }
!>"dep0" = { path = "path" }
!>```
!>
!> To reduce the amount of boilerplate code this module provides two constructors
!> for dependency types, one basic for an actual dependency (inline) table
!> and another to collect all dependency objects from a dependencies table,
!> which is handling the allocation of the objects and is forwarding the
!> individual dependency tables to their respective constructors.
!> The usual entry point should be the constructor for the super table.
!>
!> This objects contains a target to retrieve required `fpm` projects to
!> build the target declaring the dependency.
!> Resolving a dependency will result in obtaining a new package configuration
!> data for the respective project.
module fpm_manifest_dependency
    use fpm_error, only: error_t, syntax_error
    use fpm_git, only: git_target_t, git_target_tag, git_target_branch, &
        & git_target_revision, git_target_default, operator(==), git_matches_manifest
    use fpm_toml, only: toml_table, toml_key, toml_stat, get_value, check_keys
    use fpm_filesystem, only: windows_path, join_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use fpm_manifest_metapackages, only: metapackage_config_t, is_meta_package, new_meta_config, &
            metapackage_request_t, new_meta_request
    use fpm_versioning, only: version_t, new_version
    use fpm_strings, only: string_t
    use fpm_manifest_preprocess
    implicit none
    private

    public :: dependency_config_t, new_dependency, new_dependencies, manifest_has_changed

    !> Configuration meta data for a dependency
    type :: dependency_config_t

        !> Name of the dependency
        character(len=:), allocatable :: name

        !> Local target
        character(len=:), allocatable :: path

        !> Namespace which the dependency belongs to.
        !> Enables multiple dependencies with the same name.
        !> Required for dependencies that are obtained via the official registry.
        character(len=:), allocatable :: namespace

        !> The requested version of the dependency.
        !> The latest version is used if not specified.
        type(version_t), allocatable :: requested_version

        !> Requested macros for the dependency
        type(preprocess_config_t), allocatable :: preprocess(:)

        !> Git descriptor
        type(git_target_t), allocatable :: git

    contains

        !> Print information on this instance
        procedure :: info

    end type dependency_config_t

    !> Common output format for writing to the command line
    character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

contains

    !> Construct a new dependency configuration from a TOML data structure
    subroutine new_dependency(self, table, root, error)

        !> Instance of the dependency configuration
        type(dependency_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: uri, value, requested_version

        type(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call table%get_key(self%name)
        call get_value(table, "namespace", self%namespace)

        call get_value(table, "v", requested_version)
        if (allocated(requested_version)) then
            if (.not. allocated(self%requested_version)) allocate (self%requested_version)
            call new_version(self%requested_version, requested_version, error)
            if (allocated(error)) return
        end if

        !> Get optional preprocessor directives
        call get_value(table, "preprocess", child, requested=.false.)
        if (associated(child)) then
            call new_preprocessors(self%preprocess, child, error)
            if (allocated(error)) return
        endif

        call get_value(table, "path", uri)
        if (allocated(uri)) then
            if (get_os_type() == OS_WINDOWS) uri = windows_path(uri)
            if (present(root)) uri = join_path(root,uri)  ! Relative to the fpm.toml it’s written in
            call move_alloc(uri, self%path)
            return
        end if

        call get_value(table, "git", uri)
        if (allocated(uri)) then
            call get_value(table, "tag", value)
            if (allocated(value)) then
                self%git = git_target_tag(uri, value)
            end if

            if (.not. allocated(self%git)) then
                call get_value(table, "branch", value)
                if (allocated(value)) then
                    self%git = git_target_branch(uri, value)
                end if
            end if

            if (.not. allocated(self%git)) then
                call get_value(table, "rev", value)
                if (allocated(value)) then
                    self%git = git_target_revision(uri, value)
                end if
            end if

            if (.not. allocated(self%git)) then
                self%git = git_target_default(uri)
            end if
            return
        end if

    end subroutine new_dependency

    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: name
        type(toml_key), allocatable :: list(:)
        type(toml_table), pointer :: child

        !> List of valid keys for the dependency table.
        character(*), dimension(*), parameter :: valid_keys = [character(24) :: &
            & "namespace", &
              "v", &
              "path", &
              "git", &
              "tag", &
              "branch", &
              "rev", &
              "preprocess" &
            & ]

        call table%get_key(name)
        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Dependency '"//name//"' does not provide sufficient entries")
            return
        end if

        call check_keys(table, valid_keys, error)
        if (allocated(error)) return

        if (table%has_key("path") .and. table%has_key("git")) then
            call syntax_error(error, "Dependency '"//name//"' cannot have both git and path entries")
            return
        end if

        if ((table%has_key("branch") .and. table%has_key("rev")) .or. &
            (table%has_key("branch") .and. table%has_key("tag")) .or. &
            (table%has_key("rev") .and. table%has_key("tag"))) then
            call syntax_error(error, "Dependency '"//name//"' can only have one of branch, rev or tag present")
            return
        end if

        if ((table%has_key("branch") .or. table%has_key("tag") .or. table%has_key("rev")) &
            .and. .not. table%has_key("git")) then
            call syntax_error(error, "Dependency '"//name//"' has git identifier but no git url")
            return
        end if

        if (.not. table%has_key("path") .and. .not. table%has_key("git") &
            .and. .not. table%has_key("namespace")) then
            call syntax_error(error, "Please provide a 'namespace' for dependency '"//name// &
            & "' if it is not a local path or git repository")
            return
        end if

        if (table%has_key('v') .and. (table%has_key('path') .or. table%has_key('git'))) then
            call syntax_error(error, "Dependency '"//name//"' cannot have both v and git/path entries")
            return
        end if

        ! Check preprocess key
        if (table%has_key('preprocess')) then

            call get_value(table, 'preprocess', child)

            if (.not.associated(child)) then
                call syntax_error(error, "Dependency '"//name//"' has invalid 'preprocess' entry")
                return
            end if

        end if

    end subroutine check

    !> Construct new dependency array from a TOML data structure
    subroutine new_dependencies(deps, table, root, meta, error)

        !> Instance of the dependency configuration
        type(dependency_config_t), allocatable, intent(out) :: deps(:)

        !> (optional) metapackages
        type(metapackage_config_t), optional, intent(out) :: meta

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: node
        type(toml_key), allocatable :: list(:)
        type(dependency_config_t), allocatable :: all_deps(:)
        type(metapackage_request_t) :: meta_request
        logical, allocatable :: is_meta(:)
        logical :: metapackages_allowed
        integer :: idep, stat, ndep

        call table%get_keys(list)
        ! An empty table is okay
        if (size(list) < 1) return

        !> Flag dependencies that should be treated as metapackages
        metapackages_allowed = present(meta)
        allocate(is_meta(size(list)),source=.false.)
        allocate(all_deps(size(list)))

        !> Parse all meta- and non-metapackage dependencies
        do idep = 1, size(list)

            ! Check if this is a standard dependency node
            call get_value(table, list(idep)%key, node, stat=stat)
            is_standard_dependency: if (stat /= toml_stat%success) then

                ! See if it can be a valid metapackage name
                call new_meta_request(meta_request, list(idep)%key, table, error=error)

                !> Neither a standard dep nor a metapackage
                if (allocated(error)) then
                   call syntax_error(error, "Dependency "//list(idep)%key//" is not a valid metapackage or a table entry")
                   return
                endif

                !> Valid meta dependency
                is_meta(idep) = .true.

            else

                ! Parse as a standard dependency
                is_meta(idep) = .false.

                call new_dependency(all_deps(idep), node, root, error)
                if (allocated(error)) return

            end if is_standard_dependency

        end do

        ! Non-meta dependencies
        ndep = count(.not.is_meta)

        ! Finalize standard dependencies
        allocate(deps(ndep))
        ndep = 0
        do idep = 1, size(list)
            if (is_meta(idep)) cycle
            ndep = ndep+1
            deps(ndep) = all_deps(idep)
        end do

        ! Finalize meta dependencies
        if (metapackages_allowed) call new_meta_config(meta,table,is_meta,error)

    end subroutine new_dependencies

    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the dependency configuration
        class(dependency_config_t), intent(in) :: self

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

        write (unit, fmt) "Dependency"
        if (allocated(self%name)) then
            write (unit, fmt) "- name", self%name
        end if

        if (allocated(self%git)) then
            write (unit, fmt) "- kind", "git"
            call self%git%info(unit, pr - 1)
        end if

        if (allocated(self%path)) then
            write (unit, fmt) "- kind", "local"
            write (unit, fmt) "- path", self%path
        end if

    end subroutine info

    !> Check if two dependency configurations are different
    logical function manifest_has_changed(cached, manifest, verbosity, iunit) result(has_changed)

        !> Two instances of the dependency configuration
        class(dependency_config_t), intent(in) :: cached, manifest

        !> Log verbosity
        integer, intent(in) :: verbosity, iunit

        has_changed = .true.

        !> Perform all checks
        if (allocated(cached%git).neqv.allocated(manifest%git)) then
            if (verbosity>1) write(iunit,out_fmt) "GIT presence has changed. "
            return
        endif
        if (allocated(cached%git)) then
            if (.not.git_matches_manifest(cached%git,manifest%git,verbosity,iunit)) return
        end if

        !> All checks passed! The two instances are equal
        has_changed = .false.

    end function manifest_has_changed


end module fpm_manifest_dependency
