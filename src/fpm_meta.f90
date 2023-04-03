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
use fpm_model
use fpm_manifest_dependency, only: dependency_config_t
use fpm_git, only : git_target_branch
use fpm_manifest, only: package_config_t
use iso_fortran_env, only: stdout => output_unit

implicit none

private

public :: resolve_metapackages

!> Type for describing a source file
type, public :: metapackage_t

    logical :: has_link_libraries  = .false.
    logical :: has_link_flags      = .false.
    logical :: has_build_flags     = .false.
    logical :: has_include_dirs    = .false.
    logical :: has_dependencies    = .false.

    !> List of compiler flags and options to be added
    type(string_t) :: flags
    type(string_t) :: link_flags
    type(string_t), allocatable :: link_dirs(:)
    type(string_t), allocatable :: link_libs(:)

    !> List of Development dependency meta data.
    !> Metapackage dependencies are never exported from the model
    type(dependency_config_t), allocatable :: dependency(:)

    contains

       !> Clean metapackage structure
       procedure :: destroy

       !> Initialize the metapackage structure from its given name
       procedure :: new => init_from_name

       !> Add metapackage dependencies to the model
       procedure, private :: resolve_model
       procedure, private :: resolve_package_config
       generic :: resolve => resolve_model,resolve_package_config

end type metapackage_t

interface resolve_metapackages
    module procedure resolve_metapackage_model
end interface resolve_metapackages

contains

!> Clean the metapackage structure
elemental subroutine destroy(this)
   class(metapackage_t), intent(inout) :: this

   this%has_link_libraries  = .false.
   this%has_link_flags      = .false.
   this%has_build_flags     = .false.
   this%has_include_dirs    = .false.
   this%has_dependencies    = .false.

   if (allocated(this%flags%s)) deallocate(this%flags%s)
   if (allocated(this%link_flags%s)) deallocate(this%link_flags%s)
   if (allocated(this%link_dirs)) deallocate(this%link_dirs)
   if (allocated(this%link_libs)) deallocate(this%link_libs)
   if (allocated(this%dependency)) deallocate(this%dependency)

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
        case("stdlib"); call init_stdlib(this,compiler,error)
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

    !> Cleanup
    call destroy(this)

    !> OpenMP has compiler flags
    this%has_build_flags = .true.
    this%has_link_flags  = .true.

    !> OpenMP flags should be added to
    which_compiler: select case (compiler%id)
       case (id_gcc,id_f95)
            this%flags      = string_t(flag_gnu_openmp)
            this%link_flags = string_t(flag_gnu_openmp)

       case (id_intel_classic_windows,id_intel_llvm_windows)
            this%flags      = string_t(flag_intel_openmp_win)
            this%link_flags = string_t(flag_intel_openmp_win)

       case (id_intel_classic_nix,id_intel_classic_mac,&
             id_intel_llvm_nix)
            this%flags      = string_t(flag_intel_openmp)
            this%link_flags = string_t(flag_intel_openmp)

       case (id_pgi,id_nvhpc)
            this%flags      = string_t(flag_pgi_openmp)
            this%link_flags = string_t(flag_pgi_openmp)

       case (id_ibmxl)
            this%flags      = string_t(" -qsmp=omp")
            this%link_flags = string_t(" -qsmp=omp")

       case (id_nag)
            this%flags      = string_t(flag_nag_openmp)
            this%link_flags = string_t(flag_nag_openmp)

       case (id_lfortran)
            this%flags      = string_t(flag_lfortran_openmp)
            this%link_flags = string_t(flag_lfortran_openmp)

       case default

          call fatal_error(error,'openmp not supported on compiler '//compiler%name()//' yet')

    end select which_compiler


end subroutine init_openmp

!> Initialize stdlib metapackage for the current system
subroutine init_stdlib(this,compiler,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    !> Cleanup
    call destroy(this)

    !> Stdlib is queried as a dependency from the official repository
    this%has_dependencies = .true.

    allocate(this%dependency(2))

    !> 1) Test-drive
    this%dependency(1)%name = "test-drive"
    this%dependency(1)%git = git_target_branch("https://github.com/fortran-lang/test-drive","v0.4.0")
    if (.not.allocated(this%dependency(1)%git)) then
        call fatal_error(error,'cannot initialize test-drive git dependency for stdlib metapackage')
        return
    end if

    !> 2) stdlib
    this%dependency(2)%name = "stdlib"
    this%dependency(2)%git = git_target_branch("https://github.com/fortran-lang/stdlib","stdlib-fpm")
    if (.not.allocated(this%dependency(2)%git)) then
        call fatal_error(error,'cannot initialize git repo dependency for stdlib metapackage')
        return
    end if

end subroutine init_stdlib

! Resolve metapackage dependencies into the model
subroutine resolve_model(self,model,error)
    class(metapackage_t), intent(in) :: self
    type(fpm_model_t), intent(inout) :: model
    type(error_t), allocatable, intent(out) :: error

    ! For now, additional flags are assumed to apply to all sources
    if (self%has_build_flags) then
        model%fortran_compile_flags = model%fortran_compile_flags//self%flags%s
        model%c_compile_flags       = model%c_compile_flags//self%flags%s
        model%cxx_compile_flags     = model%cxx_compile_flags//self%flags%s
    endif

    if (self%has_link_flags) then
        model%link_flags            = model%link_flags//self%link_flags%s
    end if

    if (self%has_link_libraries) then
        model%link_libraries        = [model%link_libraries,self%link_libs]
    end if

    if (self%has_include_dirs) then
        model%include_dirs          = [model%include_dirs,self%link_dirs]
    end if

    ! Dependencies are resolved in the package config

end subroutine resolve_model

subroutine resolve_package_config(self,package,error)
    class(metapackage_t), intent(in) :: self
    type(package_config_t), intent(inout) :: package
    type(error_t), allocatable, intent(out) :: error

    ! All metapackage dependencies are added as full dependencies,
    ! as upstream projects will not otherwise compile without them
    if (self%has_dependencies) then
        if (allocated(package%dependency)) then
           package%dependency = [package%dependency,self%dependency]
        else
           package%dependency = self%dependency
        end if
    end if

end subroutine resolve_package_config

! Add named metapackage dependency to the model
subroutine add_metapackage_model(model,name,error)
    type(fpm_model_t), intent(inout) :: model
    character(*), intent(in) :: name
    type(error_t), allocatable, intent(out) :: error

    type(metapackage_t) :: meta

    !> Init metapackage
    call meta%new(name,model%compiler,error)
    if (allocated(error)) return

    !> Add it to the model
    call meta%resolve(model,error)
    if (allocated(error)) return

end subroutine add_metapackage_model

! Add named metapackage dependency to the model
subroutine add_metapackage_config(package,compiler,name,error)
    type(package_config_t), intent(inout) :: package
    type(compiler_t), intent(in) :: compiler
    character(*), intent(in) :: name
    type(error_t), allocatable, intent(out) :: error

    type(metapackage_t) :: meta

    !> Init metapackage
    call meta%new(name,compiler,error)
    if (allocated(error)) return

    !> Add it to the model
    call meta%resolve(package,error)
    if (allocated(error)) return

end subroutine add_metapackage_config

!> Resolve all metapackages into the package config
subroutine resolve_metapackage_model(model,package,error)
    type(fpm_model_t), intent(inout) :: model
    type(package_config_t), intent(inout) :: package
    type(error_t), allocatable, intent(out) :: error

    ! Dependencies are added to the package config, so they're properly resolved
    ! into the dependency tree later.
    ! Flags are added to the model (whose compiler needs to be already initialized)
    if (model%compiler%is_unknown()) then
        call fatal_error(error,"compiler not initialized: cannot build metapackages")
        return
    end if

    ! OpenMP
    if (package%meta%openmp) then
        call add_metapackage_model(model,"openmp",error)
        if (allocated(error)) return
        call add_metapackage_config(package,model%compiler,"openmp",error)
        if (allocated(error)) return
    endif

    ! stdlib
    if (package%meta%stdlib) then
        call add_metapackage_model(model,"stdlib",error)
        if (allocated(error)) return
        call add_metapackage_config(package,model%compiler,"stdlib",error)
        if (allocated(error)) return
    endif

    ! Stdlib is not 100% thread safe. print a warning to the user
    if (package%meta%stdlib .and. package%meta%openmp) then
        write(stdout,'(a)')'<WARNING> both openmp and stdlib requested: some functions may not be thread-safe!'
    end if

end subroutine resolve_metapackage_model

end module fpm_meta
