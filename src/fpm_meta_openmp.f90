module fpm_meta_openmp
    use fpm_compiler, only: compiler_t, id_gcc, id_f95, id_intel_classic_windows, &
       id_intel_llvm_windows, id_intel_classic_nix, id_intel_llvm_nix, &
       id_intel_classic_mac, id_pgi, id_nvhpc, id_ibmxl, id_nag, id_lfortran, &
       id_flang, id_flang_new, flag_gnu_openmp, flag_intel_openmp_win, &
       flag_intel_openmp, flag_pgi_openmp, flag_nag_openmp, &
       flag_lfortran_openmp, flag_flang_new_openmp
    use fpm_strings, only: string_t
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_error, only: error_t, fatal_error
    use fpm_manifest_metapackages, only: metapackage_request_t

    implicit none

    private

    public :: init_openmp

    contains

    !> Initialize OpenMP metapackage for the current system
    subroutine init_openmp(this,compiler,all_meta,error)
        class(metapackage_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        type(metapackage_request_t), intent(in) :: all_meta(:)
        type(error_t), allocatable, intent(out) :: error

        !> Cleanup
        call destroy(this)
        
        !> Set name
        this%name = "openmp"

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

           case (id_flang, id_flang_new)
                this%flags      = string_t(flag_flang_new_openmp)
                this%link_flags = string_t(flag_flang_new_openmp)

           case default

              call fatal_error(error,'openmp not supported on compiler '//compiler%name()//' yet')

        end select which_compiler


    end subroutine init_openmp
end module fpm_meta_openmp
