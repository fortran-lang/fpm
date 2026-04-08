module fpm_meta_blas
    use fpm_compiler, only: compiler_t, get_include_flag
    use fpm_environment, only: get_os_type, OS_MACOS, OS_WINDOWS
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_meta_util, only: add_pkg_config_compile_options
    use fpm_pkg_config, only: assert_pkg_config, pkgcfg_has_package
    use fpm_manifest_metapackages, only: metapackage_request_t
    use fpm_strings, only: string_t
    use fpm_error, only: error_t, fatal_error

    implicit none

    private

    public :: init_blas

contains

    !> Initialize blas metapackage for the current system
    subroutine init_blas(this, compiler, all_meta, error)
        class(metapackage_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        type(metapackage_request_t), intent(in) :: all_meta(:)
        type(error_t), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: include_flag, libdir
        character(*), parameter :: candidates(*) = &
                                   [character(20) :: 'mkl-dynamic-lp64-tbb', 'openblas', 'blas', 'flexiblas']

        include_flag = get_include_flag(compiler, "")

        !> Cleanup
        call destroy(this)
        allocate (this%link_libs(0), this%incl_dirs(0), this%external_modules(0))
        this%link_flags = string_t("")
        this%flags = string_t("")
        this%has_external_modules = .false.
        
        !> Set name
        this%name = ""

        if (get_os_type() == OS_MACOS) then
            if (compile_and_link_flags_supported(compiler, "-framework Accelerate")) then
                call set_compile_and_link_flags(this, compiler, "-framework Accelerate")
                return
            end if
        end if

        if (compiler%is_intel()) then
            if (get_os_type() == OS_WINDOWS) then
                if (compile_and_link_flags_supported(compiler, "/Qmkl")) then
                    call set_compile_and_link_flags(this, compiler, "/Qmkl")
                    return
                end if
            else if (compile_and_link_flags_supported(compiler, "-qmkl")) then
                call set_compile_and_link_flags(this, compiler, "-qmkl")
                return
            endif
        end if

        !> Assert pkg-config is installed
        if (.not. assert_pkg_config()) then
            call fatal_error(error, 'blas metapackage requires pkg-config to continue lookup')
            return
        end if

        do i = 1, size(candidates)
            if (pkgcfg_has_package(trim(candidates(i)))) then
                call add_pkg_config_compile_options( &
                    this, trim(candidates(i)), include_flag, libdir, error)
                print *, 'found blas package: ', trim(candidates(i))
                return
            end if
        end do

        call fatal_error(error, 'pkg-config could not find a suitable blas package.')
    end subroutine init_blas

    function compile_and_link_flags_supported(compiler, flags) result(is_supported)
        type(compiler_t), intent(in) :: compiler
        character(len=*), intent(in) :: flags
        logical :: is_supported

        is_supported = compiler%check_flags_supported(compile_flags=flags, link_flags=flags)
    end function compile_and_link_flags_supported

    subroutine set_compile_and_link_flags(this, compiler, flags)
        class(metapackage_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        character(len=*), intent(in) :: flags

        this%flags = string_t(flags)
        this%link_flags = string_t(flags)
        this%has_build_flags = .true.
        this%has_link_flags = .true.
    end subroutine set_compile_and_link_flags
end module fpm_meta_blas
