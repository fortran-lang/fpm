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
use fpm_compiler
implicit none

private

!> Type for describing a source file
type, public :: metapackage_t

    logical :: has_link_libraries = .false.
    logical :: has_link_flags     = .false.
    logical :: has_build_flags    = .false.

    !> List of compiler flags and options to be added
    type(string_t), allocatable :: flags(:)
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
   this%has_link_flags     = .false.
   this%has_build_flags    = .false.

   if (allocated(this%flags)) deallocate(this%flags)
   if (allocated(this%link_flags)) deallocate(this%link_flags)
   if (allocated(this%link_dirs)) deallocate(this%link_dirs)

end subroutine destroy

!> Initialize a metapackage from the given name
subroutine init_from_name(this,name,compiler,error)
    class(metapackage_t), intent(inout) :: this
    character(*), intent(in) :: name
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    !> Initialize metapackage by name
    select case(name)
        case("openmp"); call init_openmp(this,compiler,error)
        case default
            call syntax_error(error, "Metapackage "//name//" is not supported in [build]")
            return
    end select

end subroutine init_from_name

!> Initialize OpenMP metapackage for the current system
subroutine init_openmp(this,compiler,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: flags

    !> Cleanup
    call destroy(this)

    !> OpenMP has compiler flags
    this%has_build_flags = .true.
    this%has_link_flags  = .true.

    !> OpenMP flags should be added to
    which_compiler: select case (compiler%id)
       case (id_gcc,id_f95)
            this%flags      = [string_t(flag_gnu_openmp)]
            this%link_flags = [string_t(flag_gnu_openmp)]

       case (id_intel_classic_windows,id_intel_llvm_windows)
            this%flags      = [string_t(flag_intel_openmp_win)]
            this%link_flags = [string_t(flag_intel_openmp_win)]

       case (id_intel_classic_nix,id_intel_classic_mac,&
             id_intel_llvm_nix)
            this%flags      = [string_t(flag_intel_openmp)]
            this%link_flags = [string_t(flag_intel_openmp)]

       case (id_pgi,id_nvhpc)
            this%flags      = [string_t(flag_pgi_openmp)]
            this%link_flags = [string_t(flag_pgi_openmp)]

       case (id_ibmxl)
            this%flags      = [string_t(" -qsmp=omp")]
            this%link_flags = [string_t(" -qsmp=omp")]

       case (id_nag)
            this%flags      = [string_t(flag_nag_openmp)]
            this%link_flags = [string_t(flag_nag_openmp)]

       case (id_lfortran)
            this%flags      = [string_t(flag_lfortran_openmp)]
            this%link_flags = [string_t(flag_lfortran_openmp)]

       case default

          call fatal_error(error,'openmp not supported on compiler '//compiler%name()//' yet')

    end select which_compiler


end subroutine init_openmp

end module fpm_meta
