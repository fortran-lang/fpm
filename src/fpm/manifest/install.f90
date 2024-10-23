!> Implementation of the installation configuration.
!>
!> An install table can currently have the following fields
!>
!>```toml
!>library = bool
!>```
module fpm_manifest_install
  use fpm_error, only : error_t, fatal_error, syntax_error
  use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, set_value, serializable_t
  implicit none
  private

  public :: install_config_t, new_install_config

  !> Configuration data for installation
  type, extends(serializable_t) :: install_config_t

    !> Install library with this project
    logical :: library = .false.
    
    !> Install tests with this project
    logical :: test = .false.

  contains

    !> Print information on this instance
    procedure :: info

    !> Serialization interface
    procedure :: serializable_is_same => install_conf_same
    procedure :: dump_to_toml
    procedure :: load_from_toml

  end type install_config_t

  character(*), parameter, private :: class_name = 'install_config_t'

contains

  !> Create a new installation configuration from a TOML data structure
  subroutine new_install_config(self, table, error)

    !> Instance of the install configuration
    type(install_config_t), intent(out) :: self

    !> Instance of the TOML data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call check(table, error)
    if (allocated(error)) return

    call get_value(table, "library", self%library, .false.)
    call get_value(table, "test", self%test, .false.)

  end subroutine new_install_config


  !> Check local schema for allowed entries
  subroutine check(table, error)

    !> Instance of the TOML data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_key), allocatable :: list(:)
    integer :: ikey

    call table%get_keys(list)
    if (size(list) < 1) return

    do ikey = 1, size(list)
      select case(list(ikey)%key)
      case default
        call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in install table")
        exit
      case("library","test")
        continue    
      end select
    end do
    if (allocated(error)) return

  end subroutine check

  !> Write information on install configuration instance
  subroutine info(self, unit, verbosity)

    !> Instance of the build configuration
    class(install_config_t), intent(in) :: self

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

    write(unit, fmt) "Install configuration"
    write(unit, fmt) " - library install", trim(merge("enabled ", "disabled", self%library))
    write(unit, fmt) " - test    install", trim(merge("enabled ", "disabled", self%test))

  end subroutine info

  logical function install_conf_same(this,that)
    class(install_config_t), intent(in) :: this
    class(serializable_t), intent(in) :: that

    install_conf_same = .false.

    select type (other=>that)
       type is (install_config_t)
          if (this%library.neqv.other%library) return
          if (this%test.neqv.other%test) return
       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    install_conf_same = .true.

  end function install_conf_same

  !> Dump install config to toml table
  subroutine dump_to_toml(self, table, error)

    !> Instance of the serializable object
    class(install_config_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call set_value(table, "library", self%library, error, class_name)
    if (allocated(error)) return
    
    call set_value(table, "test", self%test, error, class_name)
    if (allocated(error)) return

  end subroutine dump_to_toml

  !> Read install config from toml table (no checks made at this stage)
  subroutine load_from_toml(self, table, error)

    !> Instance of the serializable object
    class(install_config_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: stat

    call get_value(table, "library", self%library, error, class_name)
    if (allocated(error)) return
    call get_value(table, "test", self%test, error, class_name)
    if (allocated(error)) return

  end subroutine load_from_toml

end module fpm_manifest_install
