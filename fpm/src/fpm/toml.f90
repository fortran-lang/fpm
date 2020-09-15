!> Interface to TOML processing library.
!
!  This module acts as a proxy to the `toml-f` public Fortran API and allows
!  to selectively expose components from the library to `fpm`.
!  The interaction with `toml-f` data types outside of this module should be
!  limited to tables, arrays and key-lists, most of the necessary interactions
!  are implemented in the building interface with the `get_value` and `set_value`
!  procedures.
!
!  This module allows to implement features necessary for `fpm`, which are
!  not yet available in upstream `toml-f`.
!
!  For more details on the library used see: https://github.com/toml-f/toml-f
module fpm_toml
    use fpm_error, only : error_t, fatal_error, file_not_found_error
    use tomlf, only : toml_table, toml_array, toml_key, toml_stat, get_value, &
        & set_value, toml_parse, toml_error, new_table, add_table, add_array, len
    implicit none
    private

    public :: read_package_file
    public :: toml_table, toml_array, toml_key, toml_stat, get_value, set_value
    public :: new_table, add_table, add_array, len


contains


    !> Process the configuration file to a TOML data structure
    subroutine read_package_file(table, manifest, error)

        !> TOML data structure
        type(toml_table), allocatable, intent(out) :: table

        !> Name of the package configuration file
        character(len=*), intent(in) :: manifest

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        type(toml_error), allocatable :: parse_error
        integer :: unit
        logical :: exist

        inquire(file=manifest, exist=exist)

        if (.not.exist) then
            call file_not_found_error(error, manifest)
            return
        end if

        open(file=manifest, newunit=unit)
        call toml_parse(table, unit, parse_error)
        close(unit)

        if (allocated(parse_error)) then
            allocate(error)
            call move_alloc(parse_error%message, error%message)
            return
        end if

    end subroutine read_package_file


end module fpm_toml
