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
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_model, only: fpm_model_t
    use fpm_command_line, only: fpm_cmd_settings, fpm_build_settings, fpm_run_settings
    use fpm_error, only: error_t, syntax_error, fatal_error
    use fpm_filesystem, only: join_path

    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_meta_openmp, only: init_openmp
    use fpm_meta_stdlib, only: init_stdlib
    use fpm_meta_minpack, only: init_minpack
    use fpm_meta_mpi, only: init_mpi
    use fpm_meta_hdf5, only: init_hdf5
    use fpm_meta_netcdf, only: init_netcdf
    use fpm_meta_blas, only: init_blas
    use fpm_manifest_metapackages, only: metapackage_request_t, metapackage_config_t

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
    subroutine init_from_request(this,request,compiler,all_meta,error)
        class(metapackage_t), intent(inout) :: this
        type(metapackage_request_t), intent(in) :: request
        type(compiler_t), intent(in) :: compiler
        !> Pass a list of all metapackage requests so dependencies can be sorted out
        type(metapackage_request_t), intent(in) :: all_meta(:)
        type(error_t), allocatable, intent(out) :: error

        !> Initialize metapackage by name
        select case(request%name)
            case("openmp");  call init_openmp (this,compiler,all_meta,error)
            case("stdlib");  call init_stdlib (this,compiler,all_meta,error)
            case("minpack"); call init_minpack(this,compiler,all_meta,error)
            case("mpi");     call init_mpi    (this,compiler,all_meta,error)
            case("hdf5");    call init_hdf5   (this,compiler,all_meta,error)
            case("netcdf");  call init_netcdf (this,compiler,all_meta,error)
            case("blas");    call init_blas   (this,compiler,all_meta,error)
            case default
                call syntax_error(error, "Package "//request%name//" is not supported in [metapackages]")
                return
        end select

    end subroutine init_from_request

    !> Add named metapackage dependency to the model
    subroutine add_metapackage_model(model,package,settings,meta,error)
        type(fpm_model_t), intent(inout) :: model
        type(package_config_t), intent(inout) :: package
        class(fpm_cmd_settings), intent(inout) :: settings
        type(metapackage_t), intent(inout) :: meta
        type(error_t), allocatable, intent(out) :: error

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
        if (meta%name=="mpi") then
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

        integer :: i
        type(metapackage_t) :: meta
        type(metapackage_request_t), allocatable :: requested(:)
        type(metapackage_config_t) :: all_meta
        type(package_config_t) :: dep_pkg
        character(len=:), allocatable :: manifest_path

        ! Dependencies are added to the package config, so they're properly resolved
        ! into the dependency tree later.
        ! Flags are added to the model (whose compiler needs to be already initialized)
        if (model%compiler%is_unknown()) &
        write(stdout,'(a)') '<WARNING> compiler not initialized: metapackages may not be available'

        ! Build merged metapackage config from main package and all dependencies
        ! Start with the main package's metapackage requests
        all_meta = package%meta

        ! Merge metapackage requests from all dependencies (if dependency tree exists)
        ! Skip index 1 (the main package itself)
        if (allocated(model%deps%dep)) then
            do i = 2, model%deps%ndep
                if (.not. allocated(model%deps%dep(i)%proj_dir)) cycle

                manifest_path = join_path(model%deps%dep(i)%proj_dir, "fpm.toml")
                call get_package_data(dep_pkg, manifest_path, error)
                if (allocated(error)) return

                call all_meta%merge(dep_pkg%meta)
            end do
        end if

        ! Get all requested metapackages (including those from dependencies)
        requested = all_meta%get_requests()
        if (size(requested) < 1) return

        do i = 1, size(requested)

            call init_from_request(meta, requested(i), model%compiler, requested, error)
            if (allocated(error)) return

            call add_metapackage_model(model, package, settings, meta, error)
            if (allocated(error)) return

        end do

    end subroutine resolve_metapackage_model

end module fpm_meta
