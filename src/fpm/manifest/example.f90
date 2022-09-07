!> Implementation of the meta data for an example.
!>
!> The example data structure is effectively a decorated version of an executable
!> and shares most of its properties, except for the defaults and can be
!> handled under most circumstances just like any other executable.
!>
!> A example table can currently have the following fields
!>
!>```toml
!>[[ example ]]
!>name = "string"
!>source-dir = "path"
!>main = "file"
!>link = ["lib"]
!>[example.dependencies]
!>```
module fpm_manifest_example
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies
    use fpm_manifest_executable, only : executable_config_t
    use fpm_error, only : error_t, syntax_error, bad_name_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
    implicit none
    private

    public :: example_config_t, new_example


    !> Configuation meta data for an example
    type, extends(executable_config_t) :: example_config_t

    contains

        !> Print information on this instance
        procedure :: info

    end type example_config_t


contains


    !> Construct a new example configuration from a TOML data structure
    subroutine new_example(self, table, error)

        !> Instance of the example configuration
        type(example_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve example name")
           return
        end if
        if (bad_name_error(error,'example',self%name))then
           return
        endif
        call get_value(table, "source-dir", self%source_dir, "example")
        call get_value(table, "main", self%main, "main.f90")

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, error=error)
            if (allocated(error)) return
        end if

        call get_list(table, "link", self%link, error)
        if (allocated(error)) return

    end subroutine new_example


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        logical :: name_present
        integer :: ikey

        name_present = .false.

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Example section does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in example entry")
                exit

            case("name")
                name_present = .true.

            case("source-dir", "main", "dependencies", "link")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Example name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the example configuration
        class(example_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ii
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)', &
            & fmti = '("#", 1x, a, t30, i0)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Example target"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if
        if (allocated(self%source_dir)) then
            if (self%source_dir /= "example" .or. pr > 2) then
                write(unit, fmt) "- source directory", self%source_dir
            end if
        end if
        if (allocated(self%main)) then
            if (self%main /= "main.f90" .or. pr > 2) then
                write(unit, fmt) "- example source", self%main
            end if
        end if

        if (allocated(self%dependency)) then
            if (size(self%dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- dependencies", size(self%dependency)
            end if
            do ii = 1, size(self%dependency)
                call self%dependency(ii)%info(unit, pr - 1)
            end do
        end if

    end subroutine info


end module fpm_manifest_example
