module fpm_meta_netcdf
    use fpm_compiler, only: compiler_t, get_include_flag
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_meta_util, only: add_pkg_config_compile_options
    use fpm_pkg_config, only: assert_pkg_config, pkgcfg_has_package
    use fpm_strings, only: string_t
    use fpm_error, only: error_t, fatal_error

    implicit none

    private

    public :: init_netcdf

contains

    !> Initialize NetCDF metapackage for the current system
    subroutine init_netcdf(this, compiler, error)
        class(metapackage_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        type(error_t), allocatable, intent(out) :: error

        logical :: s
        character(len=:), allocatable :: include_flag, libdir

        include_flag = get_include_flag(compiler, "")

        !> Cleanup
        call destroy(this)
        allocate (this % link_libs(0), this % incl_dirs(0), this % external_modules(0))
        this % link_flags = string_t("")
        this % flags = string_t("")

        !> Assert pkg-config is installed
        if (.not. assert_pkg_config()) then
            call fatal_error(error, 'netcdf metapackage requires pkg-config')
            return
        end if

        if (.not. pkgcfg_has_package('netcdf')) then
            call fatal_error(error, 'pkg-config could not find a suitable netcdf package.')
            return
        end if
        call add_pkg_config_compile_options(this, 'netcdf', include_flag, libdir, error)
        if (allocated(error)) return

        if (.not. pkgcfg_has_package('netcdf-fortran')) then
            call fatal_error(error, &
                             'pkg-config could not find a suitable netcdf-fortran package.')
            return
        end if
        call add_pkg_config_compile_options(this, 'netcdf-fortran', include_flag, libdir, error)
        if (allocated(error)) return

        !> Add NetCDF modules as external
        this % has_external_modules = .true.
        this % external_modules = [string_t('netcdf'), &
                                   string_t('netcdf4_f03'), &
                                   string_t('netcdf4_nc_interfaces'), &
                                   string_t('netcdf4_nf_interfaces'), &
                                   string_t('netcdf_f03'), &
                                   string_t('netcdf_fortv2_c_interfaces'), &
                                   string_t('netcdf_nc_data'), &
                                   string_t('netcdf_nc_interfaces'), &
                                   string_t('netcdf_nf_data'), &
                                   string_t('netcdf_nf_interfaces')]
    end subroutine init_netcdf
end module fpm_meta_netcdf
