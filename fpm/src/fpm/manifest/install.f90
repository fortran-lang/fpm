module fpm_manifest_install
  use fpm_error, only : error_t, fatal_error, syntax_error
  use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
  implicit none
  private

  public :: install_config_t, new_install_config

  type :: install_config_t
    logical :: library
  end type install_config_t

contains

  subroutine new_install_config(self, table, error)
    type(install_config_t), intent(out) :: self
    type(toml_table), intent(inout) :: table
    type(error_t), allocatable, intent(out) :: error
    call check(table, error)
    if (allocated(error)) return

    call get_value(table, "library", self%library, .false.)

  end subroutine new_install_config

  subroutine check(table, error)
    type(toml_table), intent(inout) :: table
    type(error_t), allocatable, intent(out) :: error

    type(toml_key), allocatable :: list(:)
    integer :: ikey
    call table%get_keys(list)
    if (size(list) < 1) return

    do ikey = 1, size(list)
      select case(list(ikey)%key)
      case default
        call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in package file")
        exit
      case("library")
        continue
      end select
    end do
    if (allocated(error)) return
  end subroutine check

end module fpm_manifest_install
