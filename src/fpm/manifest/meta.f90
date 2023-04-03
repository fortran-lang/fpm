!> Implementation of the metapackage configuration data.
!>
!> A metapackage table can currently have the following fields
!>
!>```toml
!>[metapackages]
!>fpm = "0.1.0"
!>openmp = bool
!>stdlib = bool
!>```
module fpm_manifest_metapackages
    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    implicit none
    private

    public :: metapackage_config_t, new_meta_config

    !> Configuration data for metapackages
    type :: metapackage_config_t

        !> Request OpenMP support
        logical :: openmp = .false.

        !> Request stdlib support
        logical :: stdlib = .false.


    end type metapackage_config_t


contains


    !> Construct a new build configuration from a TOML data structure
    subroutine new_meta_config(self, table, error)

        !> Instance of the build configuration
        type(metapackage_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: stat

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "openmp", self%openmp, .false., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'openmp' in fpm.toml, expecting logical")
            return
        end if

        call get_value(table, "stdlib", self%stdlib, .false., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'stdlib' in fpm.toml, expecting logical")
            return
        end if

    end subroutine new_meta_config

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

            !> Supported metapackages
            case ("openmp","stdlib")
                continue

            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in [metapackages]")
                exit

            end select
        end do

    end subroutine check

end module fpm_manifest_metapackages
