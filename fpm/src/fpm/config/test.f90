!> Implementation of the meta data for a test.
!
!  The test data structure is effectively a decorated version of an executable
!  and shares most of its properties, except for the defaults and can be
!  handled under most circumstances just like any other executable.
!
!  A test table can currently have the following fields
!
!  ```toml
!  [[test]]
!  name = "string"
!  source-dir = "path"
!  main = "file"
!  [test.dependencies]
!  ```
module fpm_config_test
    use fpm_config_dependency, only : dependency_t, new_dependencies
    use fpm_config_executable, only : executable_t
    use fpm_error, only : error_t, syntax_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    implicit none
    private

    public :: test_t, new_test


    !> Configuation meta data for an test
    type, extends(executable_t) :: test_t

    contains

        !> Print information on this instance
        procedure :: info

    end type test_t


contains


    !> Construct a new test configuration from a TOML data structure
    subroutine new_test(self, table, error)

        !> Instance of the test configuration
        type(test_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        class(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        call get_value(table, "source-dir", self%source_dir, "test")
        call get_value(table, "main", self%main, "main.f90")

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, error)
            if (allocated(error)) return
        end if

    end subroutine new_test


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

        if (.not.allocated(list)) then
            call syntax_error(error, "Executable section does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in test entry")
                exit

            case("name")
                name_present = .true.

            case("source-dir", "main", "dependencies")
                continue

            end select
        end do

        if (.not.name_present) then
            call syntax_error(error, "Executable name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the test configuration
        class(test_t), intent(in) :: self

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

        write(unit, fmt) "Test target"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if
        if (allocated(self%source_dir)) then
            if (self%source_dir /= "test" .or. pr > 2) then
                write(unit, fmt) "- source directory", self%source_dir
            end if
        end if
        if (allocated(self%main)) then
            if (self%main /= "main.f90" .or. pr > 2) then
                write(unit, fmt) "- test source", self%main
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


end module fpm_config_test
