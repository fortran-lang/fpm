module fpm_manifest_fortran
    use fpm_error, only : error_t, syntax_error, fatal_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    implicit none
    private

    public :: fortran_config_t, new_fortran_config

    !> Configuration data for Fortran
    type :: fortran_config_t

        !> Enable default implicit typing
        logical :: implicit_typing

        !> Enable implicit external interfaces
        logical :: implicit_external

        !> Form to use for all Fortran sources
        character(:), allocatable :: source_form

    end type fortran_config_t

contains

    !> Construct a new build configuration from a TOML data structure
    subroutine new_fortran_config(self, table, error)

        !> Instance of the fortran configuration
        type(fortran_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: stat
        character(:), allocatable :: source_form

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "implicit-typing", self%implicit_typing, .false., stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'implicit-typing' in fpm.toml, expecting logical")
            return
        end if

        call get_value(table, "implicit-external", self%implicit_external, .false., stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'implicit-external' in fpm.toml, expecting logical")
            return
        end if

        call get_value(table, "source-form", source_form, "free", stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'source-form' in fpm.toml, expecting logical")
            return
        end if
        select case(source_form)
        case default
            call fatal_error(error,"Value of source-form cannot be '"//source_form//"'")
            return
        case("free", "fixed", "default")
            self%source_form = source_form
        end select

    end subroutine new_fortran_config

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

            case("implicit-typing", "implicit-external", "source-form")
                continue

            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in fortran")
                exit

            end select
        end do

    end subroutine check

end module fpm_manifest_fortran
