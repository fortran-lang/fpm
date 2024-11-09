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
    use fpm_strings, only: string_t, str_ends_with, lower
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat, get_value, &
        & set_value, toml_parse, toml_error, new_table, add_table, add_array, &
        & toml_serialize, len, toml_load, toml_value
    use tomlf_de_parser, only: parse
    use jonquil, only: json_serialize, json_error, json_value, json_object, json_load, &
                       cast_to_object
    use iso_fortran_env, only: int64
    implicit none
    private

    public :: read_package_file, toml_table, toml_array, toml_key, toml_stat, &
              get_value, set_value, get_list, new_table, add_table, add_array, len, &
              toml_error, toml_serialize, toml_load, check_keys, set_list, set_string, &
              name_is_json, has_list

    !> An abstract interface for any fpm class that should be fully serializable to/from TOML/JSON
    type, abstract, public :: serializable_t

        contains

        !> Dump to TOML table, unit, file
        procedure(to_toml), deferred :: dump_to_toml
        procedure, non_overridable, private :: dump_to_file
        procedure, non_overridable, private :: dump_to_unit
        generic :: dump => dump_to_toml, dump_to_file, dump_to_unit

        !> Load from TOML table, unit, file
        procedure(from_toml), deferred :: load_from_toml
        procedure, non_overridable, private :: load_from_file
        procedure, non_overridable, private :: load_from_unit
        generic :: load => load_from_toml, load_from_file, load_from_unit

        !> Serializable entities need a way to check that they're equal
        procedure(is_equal), deferred :: serializable_is_same
        generic :: operator(==) => serializable_is_same

        !> Test load/write roundtrip
        procedure, non_overridable :: test_serialization

    end type serializable_t

    !> add_table: fpm interface
    interface add_table
        module procedure add_table_fpm
    end interface add_table

    !> set_value: fpm interface
    interface set_value
        module procedure set_logical
        module procedure set_integer
        module procedure set_integer_64
    end interface set_value

    interface set_string
        module procedure set_character
        module procedure set_string_type
    end interface set_string

    !> get_value: fpm interface
    interface get_value
        module procedure get_logical
        module procedure get_integer
        module procedure get_integer_64
    end interface get_value


    abstract interface

      !> Write object to TOML datastructure
      subroutine to_toml(self, table, error)
        import serializable_t,toml_table,error_t
        implicit none

        !> Instance of the serializable object
        class(serializable_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

      end subroutine to_toml

      !> Read dependency tree from TOML data structure
      subroutine from_toml(self, table, error)
        import serializable_t,toml_table,error_t
        implicit none

        !> Instance of the serializable object
        class(serializable_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

      end subroutine from_toml

      !> Compare two serializable objects
      logical function is_equal(this,that)
         import serializable_t
         class(serializable_t), intent(in) :: this,that
      end function is_equal

    end interface

contains

    !> Test serialization of a serializable object
    subroutine test_serialization(self, message, error)
        class(serializable_t), intent(inout) :: self
        character(len=*), intent(in) :: message
        type(error_t), allocatable, intent(out) :: error

        integer :: iunit, ii
        class(serializable_t), allocatable :: copy
        character(len=4), parameter :: formats(2) = ['TOML','JSON']

        all_formats: do ii = 1, 2

            open(newunit=iunit,form='formatted',action='readwrite',status='scratch')

            !> Dump to scratch file
            call self%dump(iunit, error, json=ii==2)
            if (allocated(error)) then
                error%message = formats(ii)//': '//error%message
                return
            endif

            !> Load from scratch file
            rewind(iunit)
            allocate(copy,mold=self)
            call copy%load(iunit,error, json=ii==2)
            if (allocated(error)) then
                error%message = formats(ii)//': '//error%message
                return
            endif
            close(iunit)

            !> Check same
            if (.not.(self==copy)) then
                call fatal_error(error,'serializable object failed '//formats(ii)//&
                                       ' write/reread test: '//trim(message))
                return
            end if
            deallocate(copy)

        end do all_formats

    end subroutine test_serialization


    !> Write serializable object to a formatted Fortran unit
    subroutine dump_to_unit(self, unit, error, json)
        !> Instance of the dependency tree
        class(serializable_t), intent(inout) :: self
        !> Formatted unit
        integer, intent(in) :: unit
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        !> Optional JSON format requested?
        logical, optional, intent(in) :: json

        type(toml_table) :: table
        logical :: is_json

        is_json = .false.; if (present(json)) is_json = json

        table = toml_table()
        call self%dump(table, error)

        if (is_json) then

!            !> Deactivate JSON serialization for now
!            call fatal_error(error, 'JSON serialization option is not yet available')
!            return

            write (unit, '(a)') json_serialize(table)
        else
            write (unit, '(a)') toml_serialize(table)
        end if

        call table%destroy()

    end subroutine dump_to_unit

    !> Write serializable object to file
    subroutine dump_to_file(self, file, error, json)
        !> Instance of the dependency tree
        class(serializable_t), intent(inout) :: self
        !> File name
        character(len=*), intent(in) :: file
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        !> Optional JSON format
        logical, optional, intent(in) :: json

        integer :: unit

        open (file=file, newunit=unit)
        call self%dump(unit, error, json)
        close (unit)
        if (allocated(error)) return

    end subroutine dump_to_file

    !> Read dependency tree from file
    subroutine load_from_file(self, file, error, json)
        !> Instance of the dependency tree
        class(serializable_t), intent(inout) :: self
        !> File name
        character(len=*), intent(in) :: file
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        !> Optional JSON format
        logical, optional, intent(in) :: json

        integer :: unit
        logical :: exist

        inquire (file=file, exist=exist)
        if (.not. exist) return

        open (file=file, newunit=unit)
        call self%load(unit, error, json)
        close (unit)
    end subroutine load_from_file

    !> Read dependency tree from file
    subroutine load_from_unit(self, unit, error, json)
        !> Instance of the dependency tree
        class(serializable_t), intent(inout) :: self
        !> File name
        integer, intent(in) :: unit
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        !> Optional JSON format
        logical, optional, intent(in) :: json

        type(toml_error), allocatable :: local_error
        type(toml_table), allocatable :: table
        type(toml_table), pointer     :: jtable
        class(toml_value), allocatable :: object
        logical :: is_json

        is_json = .false.; if (present(json)) is_json = json

        if (is_json) then

           !> init JSON interpreter
           call json_load(object, unit, error=local_error)
           if (allocated(local_error)) then
              allocate (error)
              call move_alloc(local_error%message, error%message)
              return
           end if

           jtable => cast_to_object(object)
           if (.not.associated(jtable)) then
              call fatal_error(error,'cannot initialize JSON table ')
              return
           end if

           !> Read object from TOML table
           call self%load(jtable, error)

        else

           !> use default TOML parser
           call toml_load(table, unit, error=local_error)

           if (allocated(local_error)) then
              allocate (error)
              call move_alloc(local_error%message, error%message)
              return
           end if

           !> Read object from TOML table
           call self%load(table, error)

        endif

        if (allocated(error)) return

    end subroutine load_from_unit

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
    
    !> Check if an instance of the TOML data structure contains a list
    logical function has_list(table, key)
    
        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Key to read from
        character(len=*), intent(in) :: key

        type(toml_array), pointer :: children
        
        has_list = .false.

        if (.not.table%has_key(key)) return

        call get_value(table, key, children, requested=.false.)
        
        ! There is an allocated list
        has_list = associated(children)
        
    end function has_list
    

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

    ! Set string array
    subroutine set_list(table, key, list, error)

        !> Instance of the string array
        type(string_t), allocatable, intent(in) :: list(:)

        !> Key to save to
        character(len=*), intent(in) :: key

        !> Instance of the toml table
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Local variables
        integer :: stat, ilist
        type(toml_array), pointer :: children
        character(len=:), allocatable :: str

        !> Set no key if array is not present
        if (.not.allocated(list)) return

        !> Check the key is not empty
        if (len_trim(key)<=0) then
            call fatal_error(error, 'key is empty dumping string array to TOML table')
            return
        end if

        if (size(list)/=1) then ! includes empty list case

            !> String array
            call add_array(table, key, children, stat)
            if (stat /= toml_stat%success) then
                call fatal_error(error, "Cannot set array table in "//key//" field")
                return
            end if

            do ilist = 1, size(list)
                  call set_value(children, ilist, list(ilist)%s, stat=stat)
                  if (stat /= toml_stat%success) then
                      call fatal_error(error, "Cannot store array entry in "//key//" field")
                      return
                  end if
            end do

        else

            ! Single value: set string
            call set_value(table, key, list(1)%s, stat=stat)

            if (stat /= toml_stat%success) &
            call fatal_error(error, "Cannot store entry in "//key//" field")

            return
        end if

    end subroutine set_list

    !> Function wrapper to set a character(len=:), allocatable variable to a toml table
    subroutine set_character(table, key, var, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> List of keys to check.
        character(len=*), intent(in) :: key

        !> The character variable
        character(len=*), optional, intent(in) :: var

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        integer :: ierr

        !> Check the key is not empty
        if (len_trim(key)<=0) then
            call fatal_error(error, 'key is empty setting character string to TOML table')
            if (present(whereAt)) error%message = whereAt//': '//error%message
            return
        end if

        if (present(var)) then
            call set_value(table, key, var, ierr)
            if (ierr/=toml_stat%success) then
                call fatal_error(error,'cannot set character key <'//key//'> in TOML table')
                if (present(whereAt)) error%message = whereAt//': '//error%message
                return
            end if
        endif

    end subroutine set_character

    !> Function wrapper to set a logical variable to a toml table, returning an fpm error
    subroutine set_logical(table, key, var, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> The key
        character(len=*), intent(in) :: key

        !> The variable
        logical, intent(in) :: var

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        integer :: ierr

        call set_value(table, key, var, stat=ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'cannot set logical key <'//key//'> in TOML table')
            if (present(whereAt)) error%message = whereAt//': '//error%message
            return
        end if

    end subroutine set_logical

    !> Function wrapper to set a default integer variable to a toml table, returning an fpm error
    subroutine set_integer(table, key, var, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> The key
        character(len=*), intent(in) :: key

        !> The variable
        integer, intent(in) :: var

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        integer :: ierr

        call set_value(table, key, var, stat=ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'cannot set integer key <'//key//'> in TOML table')
            if (present(whereAt)) error%message = whereAt//': '//error%message
            return
        end if

    end subroutine set_integer

    !> Function wrapper to set a default integer variable to a toml table, returning an fpm error
    subroutine set_integer_64(table, key, var, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> The key
        character(len=*), intent(in) :: key

        !> The variable
        integer(int64), intent(in) :: var

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        integer :: ierr

        call set_value(table, key, var, stat=ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'cannot set integer(int64) key <'//key//'> in TOML table')
            if (present(whereAt)) error%message = whereAt//': '//error%message
            return
        end if

    end subroutine set_integer_64

    !> Function wrapper to set a character(len=:), allocatable variable to a toml table
    subroutine set_string_type(table, key, var, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> List of keys to check.
        character(len=*), intent(in) :: key

        !> The character variable
        type(string_t), intent(in) :: var

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        call set_character(table, key, var%s, error, whereAt)

    end subroutine set_string_type

    !> Function wrapper to add a toml table and return an fpm error
    subroutine add_table_fpm(table, key, ptr, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Table key
        character(len=*), intent(in) :: key

        !> The character variable
        type(toml_table), pointer, intent(out) :: ptr

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        integer :: ierr

        !> Nullify pointer
        nullify(ptr)

        call add_table(table, key, ptr, ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'cannot add <'//key//'> table in TOML table')
            if (present(whereAt)) error%message = whereAt//': '//error%message
            return
        end if

    end subroutine add_table_fpm

    !> Function wrapper to get a logical variable from a toml table, returning an fpm error
    subroutine get_logical(table, key, var, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> The key
        character(len=*), intent(in) :: key

        !> The variable
        logical, intent(inout) :: var

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        integer :: ierr

        call get_value(table, key, var, stat=ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'cannot get logical key <'//key//'> from TOML table')
            if (present(whereAt)) error%message = whereAt//': '//error%message
            return
        end if

    end subroutine get_logical

    !> Function wrapper to get a default integer variable from a toml table, returning an fpm error
    subroutine get_integer(table, key, var, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> The key
        character(len=*), intent(in) :: key

        !> The variable
        integer, intent(inout) :: var

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        integer :: ierr

        call get_value(table, key, var, stat=ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'cannot get integer key <'//key//'> from TOML table')
            if (present(whereAt)) error%message = whereAt//': '//error%message
            return
        end if

    end subroutine get_integer

    !> Function wrapper to get a integer(int64) variable from a toml table, returning an fpm error
    subroutine get_integer_64(table, key, var, error, whereAt)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> The key
        character(len=*), intent(in) :: key

        !> The variable
        integer(int64), intent(inout) :: var

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional description
        character(len=*), intent(in), optional :: whereAt

        integer :: ierr

        call get_value(table, key, var, stat=ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'cannot get integer(int64) key <'//key//'> from TOML table')
            if (present(whereAt)) error%message = whereAt//': '//error%message
            return
        end if

    end subroutine get_integer_64

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
        type(toml_table), pointer :: child
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
            ! Right now, it can only be mapped to a string or to a child node, but this can be extended in the future.
            call get_value(table, keys(ikey)%key, value)
            if (.not. allocated(value)) then

                ! If value is not a string, check if it is a child node
                call get_value(table, keys(ikey)%key, child)

                if (.not.associated(child)) then
                    allocate (error)
                    error%message = "'"//name//"' has an invalid '"//keys(ikey)%key//"' entry."
                    return
                endif
            end if
        end do

    end subroutine check_keys

    !> Choose between JSON or TOML based on a file name
    logical function name_is_json(filename)
        character(*), intent(in) :: filename

        character(*), parameter :: json_identifier = ".json"

        name_is_json = .false.

        if (len_trim(filename)<len(json_identifier)) return

        name_is_json = str_ends_with(lower(filename),json_identifier)

    end function name_is_json

end module fpm_toml
