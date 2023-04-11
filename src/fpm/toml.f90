!># Interface to TOML processing library
!>
!> This module acts as a proxy to the `toml-f` public Fortran API and allows
!> to selectively expose components from the library to `fpm`.
!> The interaction with `toml-f` data types outside of this module should be
!> limited to tables, arrays and key-lists, most of the necessary interactions
!> are implemented in the building interface with the `get_value` and `set_value`
!> procedures.
!>
!> This module allows to implement features necessary for `fpm`, which are
!> not yet available in upstream `toml-f`.
!>
!> For more details on the library used see the
!> [TOML-Fortran](https://toml-f.github.io/toml-f) developer pages.
module fpm_toml
    use fpm_error, only: error_t, fatal_error, file_not_found_error
    use fpm_strings, only: string_t
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat, get_value, &
        & set_value, toml_parse, toml_error, new_table, add_table, add_array, &
        & toml_serialize, len, toml_load
    implicit none
    private

    public :: read_package_file, toml_table, toml_array, toml_key, toml_stat, &
              get_value, set_value, get_list, new_table, add_table, add_array, len, &
              toml_error, toml_serialize, toml_load, check_keys

    !> An abstract interface for any fpm class that should be fully serializable to/from TOML/JSON
    type, abstract, public :: fpm_serializable

        contains

        !> Dump to TOML table
        procedure(fpm_to_toml), deferred :: dump_to_toml

        !> Dump TOML to unit/file
        procedure, non_overridable :: dump_to_file
        procedure, non_overridable :: dump_to_unit

        generic :: dump => dump_to_toml, dump_to_file, dump_to_unit

    end type fpm_serializable


    abstract interface

      !> Write object to TOML datastructure
      subroutine fpm_to_toml(self, table, error)
        import fpm_serializable,toml_table,error_t
        implicit none

        !> Instance of the dependency tree
        class(fpm_serializable), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

      end subroutine fpm_to_toml

    end interface

contains


    !> Write serializable object to a formatted Fortran unit
    subroutine dump_to_unit(self, unit, error)
        !> Instance of the dependency tree
        class(fpm_serializable), intent(inout) :: self
        !> Formatted unit
        integer, intent(in) :: unit
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table) :: table

        table = toml_table()
        call self%dump(table, error)

        write (unit, '(a)') toml_serialize(table)

        call table%destroy()

    end subroutine dump_to_unit

    !> Write serializable object to file
    subroutine dump_to_file(self, file, error)
        !> Instance of the dependency tree
        class(fpm_serializable), intent(inout) :: self
        !> File name
        character(len=*), intent(in) :: file
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: unit

        open (file=file, newunit=unit)
        call self%dump(unit, error)
        close (unit)
        if (allocated(error)) return

    end subroutine dump_to_file

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

        inquire (file=manifest, exist=exist)

        if (.not. exist) then
            call file_not_found_error(error, manifest)
            return
        end if

        open(file=manifest, newunit=unit)
        call toml_load(table, unit, error=parse_error)
        close(unit)

        if (allocated(parse_error)) then
            allocate (error)
            call move_alloc(parse_error%message, error%message)
            return
        end if

    end subroutine read_package_file

    subroutine get_list(table, key, list, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Key to read from
        character(len=*), intent(in) :: key

        !> List of strings to read
        type(string_t), allocatable, intent(out) :: list(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: stat, ilist, nlist
        type(toml_array), pointer :: children
        character(len=:), allocatable :: str

        if (.not.table%has_key(key)) return

        call get_value(table, key, children, requested=.false.)
        if (associated(children)) then
            nlist = len(children)
            allocate (list(nlist))
            do ilist = 1, nlist
                call get_value(children, ilist, str, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Entry in "//key//" field cannot be read")
                    exit
                end if
                call move_alloc(str, list(ilist)%s)
            end do
            if (allocated(error)) return
        else
            call get_value(table, key, str, stat=stat)
            if (stat /= toml_stat%success) then
                call fatal_error(error, "Entry in "//key//" field cannot be read")
                return
            end if
            if (allocated(str)) then
                allocate (list(1))
                call move_alloc(str, list(1)%s)
            end if
        end if

    end subroutine get_list

    !> Check if table contains only keys that are part of the list. If a key is
    !> found that is not part of the list, an error is allocated.
    subroutine check_keys(table, valid_keys, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> List of keys to check.
        character(len=*), intent(in) :: valid_keys(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: keys(:)
        character(:), allocatable :: name, value, valid_keys_string
        integer :: ikey, ivalid

        call table%get_key(name)
        call table%get_keys(keys)

        do ikey = 1, size(keys)
            if (.not. any(keys(ikey)%key == valid_keys)) then
                ! Generate error message
                valid_keys_string = new_line('a')//new_line('a')
                do ivalid = 1, size(valid_keys)
                    valid_keys_string = valid_keys_string//trim(valid_keys(ivalid))//new_line('a')
                end do
                allocate (error)
                error%message = "Key '"//keys(ikey)%key//"' not allowed in the '"// &
                & name//"' table."//new_line('a')//new_line('a')//'Valid keys: '//valid_keys_string
                return
            end if

            ! Check if value can be mapped or else (wrong type) show error message with the error location.
            ! Right now, it can only be mapped to a string, but this can be extended in the future.
            call get_value(table, keys(ikey)%key, value)
            if (.not. allocated(value)) then
                allocate (error)
                error%message = "'"//name//"' has an invalid '"//keys(ikey)%key//"' entry."
                return
            end if
        end do

    end subroutine check_keys

end module fpm_toml
