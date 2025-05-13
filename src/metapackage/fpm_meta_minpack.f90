module fpm_meta_minpack
    use fpm_compiler, only: compiler_t
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_error, only: error_t, fatal_error
    use fpm_git, only: git_target_tag

    implicit none

    private

    public :: init_minpack

    contains

    !> Initialize minpack metapackage for the current system
    subroutine init_minpack(this,compiler,error)
        class(metapackage_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        type(error_t), allocatable, intent(out) :: error

        !> Cleanup
        call destroy(this)
        
        !> Set name
        this%name = "minpack"

        !> minpack is queried as a dependency from the official repository
        this%has_dependencies = .true.

        allocate(this%dependency(1))

        !> 1) minpack. There are no true releases currently. Fetch HEAD
        this%dependency(1)%name = "minpack"
        this%dependency(1)%git = git_target_tag("https://github.com/fortran-lang/minpack", "v2.0.0-rc.1")
        if (.not.allocated(this%dependency(1)%git)) then
            call fatal_error(error,'cannot initialize git repo dependency for minpack metapackage')
            return
        end if

    end subroutine init_minpack
end module fpm_meta_minpack
