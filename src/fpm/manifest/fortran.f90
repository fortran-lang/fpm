module fpm_manifest_fortran
    use fpm_error, only : error_t, syntax_error, fatal_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, serializable_t, set_value, set_string
    implicit none
    private

    public :: fortran_config_t, new_fortran_config

    !> Configuration data for Fortran
    type, extends(serializable_t) :: fortran_config_t

        !> Enable default implicit typing
        logical :: implicit_typing = .false.

        !> Enable implicit external interfaces
        logical :: implicit_external = .false.

        !> Form to use for all Fortran sources
        character(:), allocatable :: source_form

        contains

            !> Serialization interface
            procedure :: serializable_is_same => fortran_is_same
            procedure :: dump_to_toml
            procedure :: load_from_toml


    end type fortran_config_t

    character(len=*), parameter, private :: class_name = 'fortran_config_t'

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

  logical function fortran_is_same(this,that)
    class(fortran_config_t), intent(in) :: this
    class(serializable_t), intent(in) :: that

    fortran_is_same = .false.

    select type (other=>that)
       type is (fortran_config_t)
          if (this%implicit_typing.neqv.other%implicit_typing) return
          if (this%implicit_external.neqv.other%implicit_external) return
          if (.not.allocated(this%source_form).eqv.allocated(other%source_form)) return
          if (.not.this%source_form==other%source_form) return
       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    fortran_is_same = .true.

  end function fortran_is_same

  !> Dump install config to toml table
  subroutine dump_to_toml(self, table, error)

    !> Instance of the serializable object
    class(fortran_config_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call set_value(table, "implicit-typing", self%implicit_typing, error, class_name)
    if (allocated(error)) return
    call set_value(table, "implicit-external", self%implicit_external, error, class_name)
    if (allocated(error)) return
    call set_string(table, "source-form", self%source_form, error, class_name)
    if (allocated(error)) return

  end subroutine dump_to_toml

  !> Read install config from toml table (no checks made at this stage)
  subroutine load_from_toml(self, table, error)

    !> Instance of the serializable object
    class(fortran_config_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call get_value(table, "implicit-typing", self%implicit_typing, error, class_name)
    if (allocated(error)) return
    call get_value(table, "implicit-external", self%implicit_external, error, class_name)
    if (allocated(error)) return
    call get_value(table, "source-form", self%source_form)

  end subroutine load_from_toml


end module fpm_manifest_fortran
