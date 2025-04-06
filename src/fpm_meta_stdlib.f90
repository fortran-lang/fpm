module fpm_meta_stdlib
    use fpm_compiler, only: compiler_t
    use fpm_error, only: error_t, fatal_error
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_git, only: git_target_branch

    implicit none

    private

    public :: init_stdlib

    contains

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
end module fpm_meta_stdlib
