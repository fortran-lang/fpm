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
!> - MPI
!> - HDF5
!> - fortran-lang stdlib
!> - fortran-lang minpack
!>
!>
!> @note Core libraries are enabled in the [build] section of the fpm.toml manifest
!>
!>
module fpm_meta
    use fpm_compiler, only: compiler_t
    use fpm_manifest, only: package_config_t
    use fpm_model, only: fpm_model_t
    use fpm_command_line, only: fpm_cmd_settings, fpm_build_settings, fpm_run_settings
    use fpm_error, only: error_t, syntax_error, fatal_error

    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_meta_openmp, only: init_openmp
    use fpm_meta_stdlib, only: init_stdlib
    use fpm_meta_minpack, only: init_minpack
    use fpm_meta_mpi, only: init_mpi
    use fpm_meta_hdf5, only: init_hdf5
    use fpm_meta_netcdf, only: init_netcdf

    use shlex_module, only: shlex_split => split
    use regex_module, only: regex
    use iso_fortran_env, only: stdout => output_unit

    implicit none

    private

    public :: resolve_metapackages

    interface resolve_metapackages
        module procedure resolve_metapackage_model
    end interface resolve_metapackages

    contains

    !> Initialize a metapackage from the given name
    subroutine init_from_name(this,name,compiler,error)
        class(metapackage_t), intent(inout) :: this
        character(*), intent(in) :: name
        type(compiler_t), intent(in) :: compiler
        type(error_t), allocatable, intent(out) :: error

        !> Initialize metapackage by name
        select case(name)
            case("openmp");  call init_openmp (this,compiler,error)
            case("stdlib");  call init_stdlib (this,compiler,error)
            case("minpack"); call init_minpack(this,compiler,error)
            case("mpi");     call init_mpi    (this,compiler,error)
            case("hdf5");    call init_hdf5   (this,compiler,error)
            case("netcdf");  call init_netcdf (this,compiler,error)
            case default
                call syntax_error(error, "Package "//name//" is not supported in [metapackages]")
                return
        end select

    end subroutine init_from_name

    !> Add named metapackage dependency to the model
    subroutine add_metapackage_model(model,package,settings,name,error)
        type(fpm_model_t), intent(inout) :: model
        type(package_config_t), intent(inout) :: package
        class(fpm_cmd_settings), intent(inout) :: settings
        character(*), intent(in) :: name
        type(error_t), allocatable, intent(out) :: error

        type(metapackage_t) :: meta

        !> Init metapackage
        call init_from_name(meta,name,model%compiler,error)
        if (allocated(error)) return

        !> Add it into the model
        call meta%resolve(model,error)
        if (allocated(error)) return

        !> Add it into the package
        call meta%resolve(package,error)
        if (allocated(error)) return

        !> Add it into the settings
        call meta%resolve(settings,error)
        if (allocated(error)) return

        ! If we need to run executables, there should be an MPI runner
        if (name=="mpi") then
            select type (settings)
               class is (fpm_run_settings) ! run, test
                  if (.not.meta%has_run_command) &
                  call fatal_error(error,"cannot find a valid mpi runner on the local host")
            end select
        endif

    end subroutine add_metapackage_model

    !> Resolve all metapackages into the package config
    subroutine resolve_metapackage_model(model,package,settings,error)
        type(fpm_model_t), intent(inout) :: model
        type(package_config_t), intent(inout) :: package
        class(fpm_build_settings), intent(inout) :: settings
        type(error_t), allocatable, intent(out) :: error

        ! Dependencies are added to the package config, so they're properly resolved
        ! into the dependency tree later.
        ! Flags are added to the model (whose compiler needs to be already initialized)
        if (model%compiler%is_unknown()) &
        write(stdout,'(a)') '<WARNING> compiler not initialized: metapackages may not be available'

        ! OpenMP
        if (package%meta%openmp%on) then
            call add_metapackage_model(model,package,settings,"openmp",error)
            if (allocated(error)) return
        endif

        ! stdlib
        if (package%meta%stdlib%on) then
            call add_metapackage_model(model,package,settings,"stdlib",error)
            if (allocated(error)) return
        endif

        ! minpack
        if (package%meta%minpack%on) then
            call add_metapackage_model(model,package,settings,"minpack",error)
            if (allocated(error)) return
        endif

        ! Stdlib is not 100% thread safe. print a warning to the user
        if (package%meta%stdlib%on .and. package%meta%openmp%on) then
            write(stdout,'(a)')'<WARNING> both openmp and stdlib requested: some functions may not be thread-safe!'
        end if

        ! MPI
        if (package%meta%mpi%on) then
            call add_metapackage_model(model,package,settings,"mpi",error)
            if (allocated(error)) return
        endif

        ! hdf5
        if (package%meta%hdf5%on) then
            call add_metapackage_model(model,package,settings,"hdf5",error)
            if (allocated(error)) return
        endif

        ! netcdf
        if (package%meta%netcdf%on) then
            call add_metapackage_model(model,package,settings,"netcdf",error)
            if (allocated(error)) return
        endif

    end subroutine resolve_metapackage_model

end module fpm_meta
