module fpm_meta_stdlib
    use fpm_compiler, only: compiler_t
    use fpm_error, only: error_t, fatal_error
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_git, only: git_target_branch
    use fpm_manifest_metapackages, only: metapackage_request_t
    use fpm_strings, only: string_t
    use iso_fortran_env, only: stdout => output_unit

    implicit none

    private

    public :: init_stdlib

    contains

    !> Initialize stdlib metapackage for the current system
    subroutine init_stdlib(this,compiler,all_meta,error)
        class(metapackage_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        type(metapackage_request_t), intent(in) :: all_meta(:)
        type(error_t), allocatable, intent(out) :: error
        
        integer :: i
        logical :: with_blas

        !> Cleanup
        call destroy(this)
        
        !> Set name
        this%name = "stdlib"
        
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
        
        !> If an external BLAS is available, deploy appropriate macros    
        with_blas = external_blas(all_meta)
        if (with_blas) then 
            allocate(this%preprocess)
            call this%preprocess%new([string_t('STDLIB_EXTERNAL_BLAS'),string_t('STDLIB_EXTERNAL_LAPACK')])
            call this%dependency(2)%add_preprocess(this%preprocess)
        end if

        ! Stdlib is not 100% thread safe. print a warning to the user
        do i=1,size(all_meta)
            if (all_meta(i)%name=="openmp") then 
                write(stdout,'(a)')'<WARNING> both openmp and stdlib requested: some functions may not be thread-safe!'
            end if                
        end do

    end subroutine init_stdlib
    
    logical function external_blas(all_meta)
        type(metapackage_request_t), intent(in) :: all_meta(:)
        integer :: i
        external_blas = .false.
        do i=1,size(all_meta)
            if (all_meta(i)%name=="blas") then
                external_blas = .true.
                exit 
            end if                
        end do            
    end function external_blas
    
end module fpm_meta_stdlib
