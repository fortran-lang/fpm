!> Implementation of the build configuration data.
!>
!> A build table can currently have the following fields
!>
!>```toml
!>[build]
!>auto-executables = bool
!>auto-examples = bool
!>auto-tests = bool
!>link = ["lib"]
!>```
module fpm_manifest_build
  use fpm_error, only: error_t, syntax_error, fatal_error
  use fpm_strings, only: string_t
  use fpm_toml, only: toml_table, toml_key, toml_stat, get_value, get_list
  implicit none
  private

  public :: build_config_t, new_build_config

  !> Configuration data for build
  type :: build_config_t

    !> Automatic discovery of executables
    logical :: auto_executables

    !> Automatic discovery of examples
    logical :: auto_examples

    !> Automatic discovery of tests
    logical :: auto_tests

    !> Libraries to link against
    type(string_t), allocatable :: link(:)

    !> External modules to use
    type(string_t), allocatable :: external_modules(:)

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

    integer :: stat

    call check(table, error)
    if (allocated(error)) return

    call get_value(table, "auto-executables", self%auto_executables, .true., stat=stat)

    if (stat /= toml_stat%success) then
      call fatal_error(error, "Error while reading value for 'auto-executables' in fpm.toml, expecting logical")
      return
    end if

    call get_value(table, "auto-tests", self%auto_tests, .true., stat=stat)

    if (stat /= toml_stat%success) then
      call fatal_error(error, "Error while reading value for 'auto-tests' in fpm.toml, expecting logical")
      return
    end if

    call get_value(table, "auto-examples", self%auto_examples, .true., stat=stat)

    if (stat /= toml_stat%success) then
      call fatal_error(error, "Error while reading value for 'auto-examples' in fpm.toml, expecting logical")
      return
    end if

    call get_list(table, "link", self%link, error)
    if (allocated(error)) return

    call get_list(table, "external-modules", self%external_modules, error)
    if (allocated(error)) return

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
      select case (list(ikey)%key)

      case ("auto-executables", "auto-examples", "auto-tests", "link", "external-modules")
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

    integer :: pr, ilink, imod
    character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

    if (present(verbosity)) then
      pr = verbosity
    else
      pr = 1
    end if

    if (pr < 1) return

    write (unit, fmt) "Build configuration"
    write (unit, fmt) " - auto-discovery (apps) ", merge("enabled ", "disabled", self%auto_executables)
    write (unit, fmt) " - auto-discovery (examples) ", merge("enabled ", "disabled", self%auto_examples)
    write (unit, fmt) " - auto-discovery (tests) ", merge("enabled ", "disabled", self%auto_tests)
    if (allocated(self%link)) then
      write (unit, fmt) " - link against"
      do ilink = 1, size(self%link)
        write (unit, fmt) "   - "//self%link(ilink)%s
      end do
    end if
    if (allocated(self%external_modules)) then
      write (unit, fmt) " - external modules"
      do imod = 1, size(self%external_modules)
        write (unit, fmt) "   - "//self%external_modules(imod)%s
      end do
    end if

  end subroutine info

end module fpm_manifest_build
