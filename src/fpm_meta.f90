!># The fpm meta-package model
!>
!> This is a wrapper data type that encapsulate all pre-processing information
!> (compiler flags, linker libraries, etc.) required to correctly enable a package
!> to use a core library.
!>
!>
!>### Available core libraries
!>
!> - OpenMP
!>
!> @note Core libraries are enabled in the [build] section of the fpm.toml manifest
!>
!>
module fpm_meta
use fpm_strings, only: string_t
use fpm_error, only: error_t, fatal_error, syntax_error
implicit none

private

!> Type for describing a source file
type, public :: metapackage_t

    logical :: has_link_libraries = .false.
    logical :: has_compiler_flags = .false.

    !> List of compiler flags and options to be added
    type(string_t), allocatable :: fflags(:)
    type(string_t), allocatable :: link_flags(:)
    type(string_t), allocatable :: link_dirs(:)

    contains

       !> Clean metapackage structure
       procedure :: destroy

       !> Initialize the metapackage structure from its given name
       procedure :: new => init_from_name



end type metapackage_t

contains

!> Clean the metapackage structure
elemental subroutine destroy(this)
   class(metapackage_t), intent(inout) :: this

   this%has_link_libraries = .false.
   this%has_compiler_flags = .false.

   if (allocated(this%fflags)) deallocate(this%fflags)
   if (allocated(this%link_flags)) deallocate(this%link_flags)
   if (allocated(this%link_dirs)) deallocate(this%link_dirs)

end subroutine destroy

!> Initialize a metapackage from the given name
subroutine init_from_name(this,name,error)
    class(metapackage_t), intent(inout) :: this
    character(*), intent(in) :: name
    type(error_t), allocatable, intent(out) :: error

    !> Initialize metapackage by name
    select case(name)
        case("openmp"); call init_openmp(this,error)
        case default
            call syntax_error(error, "Metapackage "//name//" is not supported in [build]")
            return
    end select

end subroutine init_from_name

!> Initialize OpenMP
subroutine init_openmp(this,error)
    class(metapackage_t), intent(inout) :: this
    type(error_t), allocatable, intent(out) :: error

    call fatal_error(error,"OpenMP metapackage is not yet supported")

end subroutine init_openmp

end module fpm_meta
