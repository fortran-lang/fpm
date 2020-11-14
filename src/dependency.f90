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
    implicit none
    private

    public :: dependency_t, new_dependency, new_dependencies


    !> Configuration meta data for a dependency
    type :: dependency_t

        !> Name of the dependency
        character(len=:), allocatable :: name

        !> Local target
        character(len=:), allocatable :: path

        !> Git descriptor
        type(git_target_t), allocatable :: git

    contains

        !> Print information on this instance
        procedure :: info

    end type dependency_t


contains


    !> Construct a new dependency configuration from a TOML data structure
    subroutine new_dependency(self, table, error)

        !> Instance of the dependency configuration
        type(dependency_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: url, obj

        call check(table, error)
        if (allocated(error)) return

        call table%get_key(self%name)

        call get_value(table, "path", url)
        if (allocated(url)) then
            call move_alloc(url, self%path)
        else
            call get_value(table, "git", url)

            call get_value(table, "tag", obj)
            if (allocated(obj)) then
                self%git = git_target_tag(url, obj)
            end if

            if (.not.allocated(self%git)) then
                call get_value(table, "branch", obj)
                if (allocated(obj)) then
                    self%git = git_target_branch(url, obj)
                end if
            end if

            if (.not.allocated(self%git)) then
                call get_value(table, "rev", obj)
                if (allocated(obj)) then
                    self%git = git_target_revision(url, obj)
                end if
            end if

            if (.not.allocated(self%git)) then
                self%git = git_target_default(url)
            end if

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
        logical :: url_present, git_target_present, has_path
        integer :: ikey

        has_path = .false.
        url_present = .false.
        git_target_present = .false.

        call table%get_key(name)
        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Dependency "//name//" does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in dependency "//name)
                exit

            case("git", "path")
                if (url_present) then
                    call syntax_error(error, "Dependency "//name//" cannot have both git and path entries")
                    exit
                end if
                url_present = .true.
                has_path = list(ikey)%key == 'path'

            case("branch", "rev", "tag")
                if (git_target_present) then
                    call syntax_error(error, "Dependency "//name//" can only have one of branch, rev or tag present")
                    exit
                end if
                git_target_present = .true.

            end select
        end do
        if (allocated(error)) return

        if (.not.url_present) then
            call syntax_error(error, "Dependency "//name//" does not provide a method to actually retrieve itself")
            return
        end if

        if (has_path .and. git_target_present) then
            call syntax_error(error, "Dependency "//name//" uses a local path, therefore no git identifiers are allowed")
        end if

    end subroutine check


    !> Construct new dependency array from a TOML data structure
    subroutine new_dependencies(deps, table, error)

        !> Instance of the dependency configuration
        type(dependency_t), allocatable, intent(out) :: deps(:)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

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
            call new_dependency(deps(idep), node, error)
            if (allocated(error)) exit
        end do

    end subroutine new_dependencies


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the dependency configuration
        class(dependency_t), intent(in) :: self

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
