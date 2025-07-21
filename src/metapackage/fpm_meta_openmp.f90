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

        !> Local variables for OpenMP testing
        character(:), allocatable :: openmp_flag, link_flag
        character(len=*), parameter :: openmp_test_fortran = &
            "use omp_lib; if (omp_get_max_threads() <= 0) stop 1; end"
        character(len=*), parameter :: openmp_test_c = &
            "#include <omp.h>" // new_line('a') // &
            "int main() { return omp_get_max_threads() > 0 ? 0 : 1; }"
        character(len=*), parameter :: openmp_test_cxx = &
            "#include <omp.h>" // new_line('a') // &
            "int main() { return omp_get_max_threads() > 0 ? 0 : 1; }"

        !> Cleanup
        call destroy(this)
        
        !> Set name
        this%name = "openmp"

        !> Get OpenMP flags based on compiler
        which_compiler: select case (compiler%id)
           case (id_gcc,id_f95)
                openmp_flag = flag_gnu_openmp
                link_flag = flag_gnu_openmp

           case (id_intel_classic_windows,id_intel_llvm_windows)
                openmp_flag = flag_intel_openmp_win
                link_flag = flag_intel_openmp_win

           case (id_intel_classic_nix,id_intel_classic_mac,&
                 id_intel_llvm_nix)
                openmp_flag = flag_intel_openmp
                link_flag = flag_intel_openmp

           case (id_pgi,id_nvhpc)
                openmp_flag = flag_pgi_openmp
                link_flag = flag_pgi_openmp

           case (id_ibmxl)
                openmp_flag = " -qsmp=omp"
                link_flag = " -qsmp=omp"

           case (id_nag)
                openmp_flag = flag_nag_openmp
                link_flag = flag_nag_openmp

           case (id_lfortran)
                openmp_flag = flag_lfortran_openmp
                link_flag = flag_lfortran_openmp

           case (id_flang, id_flang_new)
                openmp_flag = flag_flang_new_openmp
                link_flag = flag_flang_new_openmp

           case default
              call fatal_error(error,'openmp not supported on compiler '//compiler%name()//' yet')
              return

        end select which_compiler

        !> Test Fortran OpenMP support
        if (compiler%check_fortran_source_runs(openmp_test_fortran, openmp_flag, link_flag)) then
            this%has_fortran_flags = .true.
            this%fflags = string_t(openmp_flag)
        endif

        !> Test C OpenMP support
        if (compiler%check_c_source_runs(openmp_test_c, openmp_flag, link_flag)) then
            this%has_c_flags = .true.
            this%cflags = string_t(openmp_flag)
        endif

        !> Test C++ OpenMP support  
        if (compiler%check_cxx_source_runs(openmp_test_cxx, openmp_flag, link_flag)) then
            this%has_cxx_flags = .true.
            this%cxxflags = string_t(openmp_flag)
        endif

        !> Always set link flags when OpenMP is requested
        !> The linker needs OpenMP flags regardless of individual compiler support
        this%has_link_flags = .true.
        this%link_flags = string_t(link_flag)

    end subroutine init_openmp
end module fpm_meta_openmp
