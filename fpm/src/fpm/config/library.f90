!> Implementation of the meta data for libraries.
!
!  A library table can currently have the following fields
!
!  ```toml
!  [library]
!  source-dir = "path"
!  build-script = "file"
!  ```
module fpm_config_library
    use fpm_error, only : error_t, syntax_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    implicit none
    private

    public :: library_t, new_library


    !> Configuration meta data for a library
    type :: library_t

        !> Source path prefix
        character(len=:), allocatable :: source_dir

        !> Alternative build script to be invoked
        character(len=:), allocatable :: build_script

    contains

        !> Print information on this instance
        procedure :: info

    end type library_t


contains


    !> Construct a new library configuration from a TOML data structure
    subroutine new_library(self, table, error)

        !> Instance of the library configuration
        type(library_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "source-dir", self%source_dir, "src")
        call get_value(table, "build-script", self%build_script)

    end subroutine new_library


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        integer :: ikey

        call table%get_keys(list)

        ! table can be empty
        if (.not.allocated(list)) return

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in package file")
                exit

            case("source-dir", "build-script")
                continue

            end select
        end do

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the library configuration
        class(library_t), intent(in) :: self

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

        if (pr < 1) return

        write(unit, fmt) "Library target"
        if (allocated(self%source_dir)) then
            write(unit, fmt) "- source directory", self%source_dir
        end if
        if (allocated(self%build_script)) then
            write(unit, fmt) "- custom build", self%build_script
        end if

    end subroutine info


end module fpm_config_library
