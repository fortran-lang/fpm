!> Lock on a build directory, so that fpm processes working on the same package
!> wait for each other instead of building over each other.
!>
!> The lock is an exclusive OS lock on `<build-dir>/.fpm-lock`: `flock` on Unix,
!> `LockFileEx` on Windows. It belongs to the process, and all its OpenMP threads
!> work under it: acquire and release it in serial code, outside any parallel region.
!> The OS releases it when the process exits, also after an error stop or a crash,
!> so no stale lock is left behind. In the bootstrap build, which has no C, it does
!> nothing.
module fpm_build_lock
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_intptr_t, c_null_char
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    use fpm_filesystem, only: join_path, mkdir
    implicit none
    private

    public :: build_lock_t

    !> Lock on a build directory, held by the command that works on it. Release it
    !> on every way out of the command, before an error stop too. Do not copy a held
    !> lock: both copies would release it.
    type :: build_lock_t
        private
        !> Descriptor of the lock file (its HANDLE on Windows), -1 when not held
        integer(kind=c_intptr_t) :: handle = -1
    contains
        procedure :: acquire
        procedure :: release
        procedure :: held
        final :: release_on_exit
    end type build_lock_t

#ifndef FPM_BOOTSTRAP
    interface
        function c_lock_file(path, wait) result(handle) bind(c, name="c_lock_file")
            import c_char, c_int, c_intptr_t
            character(kind=c_char), intent(in) :: path(*)
            integer(kind=c_int), intent(in), value :: wait
            integer(kind=c_intptr_t) :: handle
        end function c_lock_file

        subroutine c_unlock_file(handle) bind(c, name="c_unlock_file")
            import c_intptr_t
            integer(kind=c_intptr_t), intent(in), value :: handle
        end subroutine c_unlock_file
    end interface
#endif

contains

    !> Lock the build directory `build_dir`, creating it if needed. While another
    !> fpm process holds the lock, say so and wait for it. If the lock cannot be
    !> taken, warn and go on without it. Does nothing if this lock is already held.
    subroutine acquire(self, build_dir)
        class(build_lock_t), intent(inout) :: self
        character(len=*), intent(in) :: build_dir
#ifndef FPM_BOOTSTRAP
        character(len=:), allocatable :: lock_file

        if (self%held()) return

        call mkdir(build_dir, echo=.false.)
        lock_file = join_path(build_dir, '.fpm-lock')

        self%handle = c_lock_file(lock_file//c_null_char, 0_c_int)
        if (self%handle == -2) then
            write(stderr, '(a)') '<INFO> Waiting for another fpm process to release '//lock_file
            self%handle = c_lock_file(lock_file//c_null_char, 1_c_int)
        end if
        if (self%handle < 0) then
            write(stderr, '(a)') '<WARN> Cannot lock '//lock_file// &
                                 ': fpm processes running at the same time on this package are not safe'
            self%handle = -1
        end if
#endif
    end subroutine acquire

    !> Release the lock, if held
    subroutine release(self)
        class(build_lock_t), intent(inout) :: self
#ifndef FPM_BOOTSTRAP
        if (.not. self%held()) return
        call c_unlock_file(self%handle)
        self%handle = -1
#endif
    end subroutine release

    !> Release the lock when it goes out of scope, as on a return from the command
    !> that holds it. Compilers that do not finalize local variables, like gfortran
    !> before 13, skip this, so commands release the lock themselves as well.
    subroutine release_on_exit(self)
        type(build_lock_t), intent(inout) :: self
        call self%release()
    end subroutine release_on_exit

    !> Whether this lock is held
    logical function held(self)
        class(build_lock_t), intent(in) :: self
        held = self%handle >= 0
    end function held

end module fpm_build_lock
