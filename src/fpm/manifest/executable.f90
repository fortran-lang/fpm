!> Implementation of the meta data for an executables.
!>
!> An executable table can currently have the following fields
!>
!>```toml
!>[[ executable ]]
!>name = "string"
!>source-dir = "path"
!>main = "file"
!>link = ["lib"]
!>[executable.dependencies]
!>```
module fpm_manifest_executable
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies
    use fpm_error, only : error_t, syntax_error, bad_name_error
    use fpm_strings, only : string_t,string_cat
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
    implicit none
    private

    public :: executable_config_t, new_executable


    !> Configuation meta data for an executable
    type :: executable_config_t

        !> Name of the resulting executable
        character(len=:), allocatable :: name

        !> Source directory for collecting the executable
        type(string_t), allocatable :: source_dir(:)

        !> Name of the source file declaring the main program
        character(len=:), allocatable :: main

        !> Dependency meta data for this executable
        type(dependency_config_t), allocatable :: dependency(:)

        !> Libraries to link against
        type(string_t), allocatable :: link(:)

    contains

        !> Print information on this instance
        procedure :: info

    end type executable_config_t


contains


    !> Construct a new executable configuration from a TOML data structure
    subroutine new_executable(self, table, error)

        !> Instance of the executable configuration
        type(executable_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve executable name")
           return
        end if
        if (bad_name_error(error,'executable',self%name))then
           return
        endif

        call get_list(table, "source-dir", self%source_dir, error)
        if (allocated(error)) return

        ! Set default value of source-dir if not found in manifest
        if (.not.allocated(self%source_dir)) then
            self%source_dir = [string_t("app")]
        end if

        call get_value(table, "main", self%main, "main.f90")

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, error=error)
            if (allocated(error)) return
        end if

        call get_list(table, "link", self%link, error)
        if (allocated(error)) return


    end subroutine new_executable


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
            call syntax_error(error, "Executable section does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed as executable entry")
                exit

            case("name")
                name_present = .true.

            case("source-dir", "main", "dependencies", "link")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Executable name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the executable configuration
        class(executable_config_t), intent(in) :: self

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

        write(unit, fmt) "Executable target"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if
        if (allocated(self%source_dir)) then
            if (self%source_dir(1)%s /= "app" .or. size(self%source_dir)/=1 .or. pr > 2) then
                write(unit, fmt) "- source directory", string_cat(self%source_dir,",")
            end if
        end if
        if (allocated(self%main)) then
            if (self%main /= "main.f90" .or. pr > 2) then
                write(unit, fmt) "- program source", self%main
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


end module fpm_manifest_executable
