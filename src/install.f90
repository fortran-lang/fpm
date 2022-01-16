!> Implementation of the installation configuration.
!>
!> An install table can currently have the following fields
!>
!>```toml
!>library = bool
!>```
module fpm_manifest_install
  use fpm_error, only : error_t, fatal_error, syntax_error
  use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
  implicit none
  private

  public :: install_config_t, new_install_config

  !> Configuration data for installation
  type :: install_config_t

    !> Install library with this project
    logical :: library

  contains

    !> Print information on this instance
    procedure :: info

  end type install_config_t

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
      case("library")
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
    write(unit, fmt) " - library install", &
      & trim(merge("enabled ", "disabled", self%library))

  end subroutine info

end module fpm_manifest_install
