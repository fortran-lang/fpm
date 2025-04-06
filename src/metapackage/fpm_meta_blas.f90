module fpm_meta_blas
    use fpm_compiler, only: compiler_t, get_include_flag
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_meta_util, only: add_pkg_config_compile_options
    use fpm_pkg_config, only: assert_pkg_config, pkgcfg_has_package
    use fpm_strings, only: string_t
    use fpm_error, only: error_t, fatal_error

    implicit none

    private

    public :: init_blas

contains

    !> Initialize blas metapackage for the current system
    subroutine init_blas(this, compiler, error)
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
        this % has_external_modules = .false.

        !> Assert pkg-config is installed
        if (.not. assert_pkg_config()) then
            call fatal_error(error, 'blas metapackage requires pkg-config')
            return
        end if

        if (pkgcfg_has_package('openblas')) then
            call add_pkg_config_compile_options(this, 'openblas', include_flag, libdir, error)
            return
        end if
    end subroutine init_blas
end module fpm_meta_blas
