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
    use fpm_error, only : error_t, syntax_error
    use fpm_git, only : git_target_t, git_target_tag, git_target_branch, &
        & git_target_revision, git_target_default
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    use fpm_filesystem, only: windows_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none
    private

    public :: dependency_config_t, new_dependency, new_dependencies


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

        !> The specified version of the dependency.
        !> The latest version is used if not specified.
        character(len=:), allocatable :: vers

        !> Git descriptor
        type(git_target_t), allocatable :: git

    contains

        !> Print information on this instance
        procedure :: info

    end type dependency_config_t


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

        character(len=:), allocatable :: uri, value

        call check(table, error)
        if (allocated(error)) return

        call table%get_key(self%name)
        call get_value(table, "namespace", self%namespace)

        call get_value(table, "path", uri)
        if (allocated(uri)) then
            if (get_os_type() == OS_WINDOWS) uri = windows_path(uri)
            if (present(root)) uri = root//uri  ! Relative to the fpm.toml it’s written in
            call move_alloc(uri, self%path)
            return
        end if

        call get_value(table, "git", uri)
        if (allocated(uri)) then
            call get_value(table, "tag", value)
            if (allocated(value)) then
                self%git = git_target_tag(uri, value)
            end if

            if (.not.allocated(self%git)) then
                call get_value(table, "branch", value)
                if (allocated(value)) then
                    self%git = git_target_branch(uri, value)
                end if
            end if

            if (.not.allocated(self%git)) then
                call get_value(table, "rev", value)
                if (allocated(value)) then
                    self%git = git_target_revision(uri, value)
                end if
            end if

            if (.not.allocated(self%git)) then
                self%git = git_target_default(uri)
            end if
            return
        end if

        call get_value(table, "vers", self%vers)

    end subroutine new_dependency


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: name, value, valid_keys_string
        type(toml_key), allocatable :: list(:)
        integer :: ikey, ivalid

        !> List of allowed keys for the dependency table
        character(*), dimension(*), parameter :: valid_keys = [character(24) ::&
            & "namespace",&
              "vers",&
              "path",&
              "git",&
              "tag",&
              "branch",&
              "rev" &
            & ]

        call table%get_key(name)
        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Dependency '"//name//"' does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            if (.not. any(list(ikey)%key == valid_keys)) then
                ! Improve error message
                valid_keys_string = new_line('a')//new_line('a')
                do ivalid = 1, size(valid_keys)
                    valid_keys_string = valid_keys_string//trim(valid_keys(ivalid))//new_line('a')
                end do
                call syntax_error(error, "Key '"//list(ikey)%key//"' not allowed in dependency '"//&
                name//"'."//new_line('a')//new_line('a')//'Valid keys: '//valid_keys_string)
                return
            end if

            ! Check if value can be mapped or else (wrong type) show error message with the error location
            call get_value(table, list(ikey)%key, value)
            if (.not. allocated(value)) then
                call syntax_error(error, "Dependency '"//name//"' has invalid '"//list(ikey)%key//"' entry")
                return
            end if
        end do

        if (table%has_key("path") .and. table%has_key("git")) then
            call syntax_error(error, "Dependency '"//name//"' cannot have both git and path entries")
            return
        end if
        
        if ((table%has_key("branch") .and. table%has_key("rev")) .or.&
        (table%has_key("branch") .and. table%has_key("tag")) .or.&
        (table%has_key("rev") .and. table%has_key("tag"))) then
            call syntax_error(error, "Dependency '"//name//"' can only have one of branch, rev or tag present")
            return
        end if
        
        if ((table%has_key("branch") .or. table%has_key("tag") .or. table%has_key("rev"))&
            .and. .not. table%has_key("git")) then
            call syntax_error(error, "Dependency '"//name//"' has git identifier but no git url")
            return
        end if
        
        if (.not. table%has_key("path") .and. .not. table%has_key("git")&
            .and. .not. table%has_key("namespace")) then
            call syntax_error(error, "Please provide a 'namespace' for dependency '"//name//&
            "' if it is not a local path or git repository")
            return
        end if

        if (table%has_key('vers') .and. (table%has_key('path') .or. table%has_key('git'))) then
            call syntax_error(error, "Dependency '"//name//"' cannot have both vers and git/path entries")
            return
        end if

    end subroutine check

    !> Construct new dependency array from a TOML data structure
    subroutine new_dependencies(deps, table, root, error)

        !> Instance of the dependency configuration
        type(dependency_config_t), allocatable, intent(out) :: deps(:)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: node
        type(toml_key), allocatable :: list(:)
        integer :: idep, stat

        call table%get_keys(list)
        ! An empty table is okay
        if (size(list) < 1) return

        allocate(deps(size(list)))
        do idep = 1, size(list)
            call get_value(table, list(idep)%key, node, stat=stat)
            if (stat /= toml_stat%success) then
                call syntax_error(error, "Dependency "//list(idep)%key//" must be a table entry")
                exit
            end if
            call new_dependency(deps(idep), node, root, error)
            if (allocated(error)) exit
        end do

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

        write(unit, fmt) "Dependency"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if

        if (allocated(self%git)) then
            write(unit, fmt) "- kind", "git"
            call self%git%info(unit, pr - 1)
        end if

        if (allocated(self%path)) then
            write(unit, fmt) "- kind", "local"
            write(unit, fmt) "- path", self%path
        end if

    end subroutine info


end module fpm_manifest_dependency
