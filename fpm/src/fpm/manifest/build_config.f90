!> Implementation of the build configuration data.
!>
!> A build table can currently have the following fields
!>
!>```toml
!>[build]
!>auto-executables = bool
!>auto-tests = bool
!>```
module fpm_manifest_build_config
    use fpm_error, only : error_t, syntax_error, fatal_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    implicit none
    private

    public :: build_config_t, new_build_config


    !> Configuration data for build
    type :: build_config_t

        !> Automatic discovery of executables
        logical :: auto_executables

        !> Automatic discovery of tests
        logical :: auto_tests

    contains

        !> Print information on this instance
        procedure :: info

    end type build_config_t


contains


    !> Construct a new build configuration from a TOML data structure
    subroutine new_build_config(self, table, error)

        !> Instance of the build configuration
        type(build_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Status 
        integer :: stat

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "auto-executables", self%auto_executables, .true., stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'auto-executables' in fpm.toml, expecting logical")
            return
        end if

        call get_value(table, "auto-tests", self%auto_tests, .true., stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'auto-tests' in fpm.toml, expecting logical")
            return
        end if

    end subroutine new_build_config


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
        if (size(list) < 1) return

        do ikey = 1, size(list)
            select case(list(ikey)%key)

            case("auto-executables", "auto-tests")
                continue

            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in [build]")
                exit

            end select
        end do

    end subroutine check


    !> Write information on build configuration instance
    subroutine info(self, unit, verbosity)

        !> Instance of the build configuration
        class(build_config_t), intent(in) :: self

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

        write(unit, fmt) "Build configuration"
        ! if (allocated(self%auto_executables)) then
            write(unit, fmt) " - auto-discovery (apps) ", merge("enabled ", "disabled", self%auto_executables)
        ! end if
        ! if (allocated(self%auto_tests)) then
            write(unit, fmt) " - auto-discovery (tests) ", merge("enabled ", "disabled", self%auto_tests)
        ! end if

    end subroutine info

end module fpm_manifest_build_config
