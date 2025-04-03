!> Lock package directories before working on them.
!>
!># Synopsis
!>
!> Use the functions [[fpm_lock_acquire]] and [[fpm_lock_release]] to "lock" a
!> `fpm` package directory to prevent issues when multiple `fpm` process want
!> to work on the same package at the same time. Here's an example of how this
!> module is used in the rest of the codebase:
!>
!>```fortran
!> !> Entry point for the update subcommand
!>subroutine cmd_update(settings)
!> type(error_t), allocatable :: error
!> fpm_lock_acquire(error)
!> ! Do things here
!> fpm_lock_release(error)
!>end subroutine cmd_update
!>```
!>
!># Background
!>
!> This module exists to fix a buggy behavior that exists in many package
!> managers (however, most users never experience issues with it).
!>
!> The buggy behaviors is that when many `fpm` processes try to work on the same
!> package at the same time the different processes sort of step on one another
!> an it leads to problems, for instance two processes might try to compile the
!> same file  at the same time.
!>
!> Also see this issue:
!> [https://github.com/fortran-lang/fpm/issues/957](https://github.com/fortran-lang/fpm/issues/957)
!> for some
!> more details.
!>
!> What we need is for an `fpm` process \(A\) to see if another `fpm` process
!> \(B\) is already working on a package, and if so, wait for \(B\) to finish
!> that work before \(A\) steps in. The way we do this is with so-called
!> *lock-files*. Basically \(B\) creates a special file named
!> `.fpm-package-lock` in the package directory so that \(A\) will see that this
!> file exists and wait for it to be deleted by \(B\), when that is done it
!> means that the package directory is free, and so \(A\) now creates
!> `.fpm-package-lock` itself and does it's thing, after \(A\) is done it
!> deletes the lock-file again.
!>
!> That's pretty much the gist of it. It's complicated somewhat by the fact that
!> we need to consider certain rare cases (what if the program crashes and
!> leaves the lock-file behind for instance). Also, the lock-file operations
!> have to be what's called "atomic". For instance, consider this non-atomic way
!> of creating a lock-file: (in pseudocode)
!>```
!>1)   if file_exists('.fpm-package-lock') then
!>         wait_for_file_to_be_deleted('.fpm-package-lock')
!>2)   create_file('.fpm-package-lock')
!>3)   do_something()
!>4)   delete_file('.fpm-package-lock')
!>```
!> The problem with this code is that `.fpm-packge-lock` may be created by some
!> other process after the check on line (1), but before line (2) has executed,
!> and then it's not very clear what will happen, both processes might think
!> that they are have acquired a lock on the package directory. A better piece
!> of code could be:
!>```
!>error = create_file('.fpm-package-lock')
!>if error == ALREADY_EXISTS then
!>    create_this_file_again_after_deletion('.fpm-package-lock')
!>do_something()
!>delete_file('.fpm-package-lock')
!>```

! IMPLEMENTATION NOTES(@emmabastas)
!
! There are many ways to lock a directory, and the approach we're using here
! is maybe the simplest: If `.fpm-package-lock` exists in the package directory
! then the directory is locked, if we manage to create `.fpm-package-lock` then
! we have the lock.
!
! The problem with this approach is that if `fpm` doesn't terminate normally, or
! maybe if there's some bug we might leave the lock-file behind, and later fpm
! process might wait indefinitely for the package to be unlocked with no way
! of knowing that the file was left behind by accident.
!
! The approach taken here is to simply print a warning/info message to the user
! about the lock-file, and that they can remove it manually if they suspect it's
! been left behind by accident, this is how `git` does it.
!
! (RANT STARTS HERE)
!
! A common attempt at improving this situation might be to write the PID +
! process start time into the lock-file, that way other processes can verify
! that the lock-file wasn't left behind on accident. However this adds quite a
! bit of complexity to the code, and it is very difficult / maybe even
! impossible to do without race-conditions: There is no atomic way to tell the
! OS to:
!    > Open this file for reading with a shared lock in case it exists and
!    > if it doesn't exists then create the file with for writing with an
!    > exclusive lock.
! Even if we we're able to do it there is a conceptual flaw: Distributed file
! systems. If the package lives on another machine that you're accessing through
! something like NFS then you'll end up writing your PID to a file living on
! another machine that's not running the process, and so processes on that
! machine can't know whether the lock is valid or not.
!
! Now we might turn to actual OS file-locking primitives such as `fcntl` on UNIX
! and `LockFile` on Windows. This is again a step-up in complexity, and I don't
! know about `LockFile` but `fcntl` is fraught with problems:
! https://chris.improbable.org/2010/12/16/everything-you-never-wanted-to-know-about-file-locking/
!
! My conclusion is that anything more advanced than the current implementation
! might just not be worth it, but I'm happy to be proven wrong! :-)

module fpm_lock

use :: fpm_error, only : error_t, fatal_error
use :: fpm_os, only : get_current_directory
use :: fpm_filesystem, only : join_path, delete_file
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use iso_c_binding, only : c_int, c_char, c_null_char, c_ptr, c_funptr, &
                          c_funloc, c_f_pointer
use fpm_strings, only: f_string


implicit none
private
public :: fpm_lock_acquire, fpm_lock_acquire_noblock, fpm_lock_release

interface
    ! This function is defined in `fpm_lock.c`.
    subroutine c_create(path, iostat, iomsg, exists) bind(c, name='c_create')
        import c_int, c_char, c_ptr
        character(kind=c_char), intent(in)  :: path(*)
        integer(kind=c_int),    intent(out) :: iostat
        type(c_ptr),            intent(out) :: iomsg
        integer(kind=c_int),    intent(out) :: exists
    end subroutine c_create

    ! atexit is a standard C90 function.
    subroutine atexit(fptr) bind(c, name='atexit')
        import c_funptr
        type(c_funptr), value :: fptr
    end subroutine atexit

    ! free is also standard C.
    subroutine c_free(ptr) BIND(C, name='free')
        import
        type(c_ptr), value :: ptr
    end subroutine c_free
end interface

contains

! This routine is called when fpm terminates normally and is used to remove
! the .fpm-package-lock in case we created it.
!
! Of note is that this only works when fpm is terminated "normally", meaning
! if a user manually kills the process this function won't be called.
subroutine atexit_cleanup()
    type(error_t), allocatable :: error
    call fpm_lock_release(error)
    ! If there is an error there isn't all that much for us to do, we're exiting
    ! the program after all.
end subroutine atexit_cleanup

!> Like [[fpm_lock_acquire]] but it some other process already has a lock it
!> returns immediately instead of waiting indefinitely.
subroutine fpm_lock_acquire_noblock(error, success)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    !> `.true.` if a package lock was acquired, `.false.` otherwise.
    logical, optional, intent(out) :: success

    ! unit for open lock-file.
    integer :: lock_unit

    ! Error status and message.
    integer :: iostat
    character(:), allocatable :: iomsg
    character(len=1), pointer :: c_iomsg(:)
    type(c_ptr) :: c_iomsg_ptr

    ! Did the lock-file exist already or not.
    integer :: exists

    ! NOTE(@emmabastas) as far as I can tell there is no atomic way to tell
    ! Fortran to "create this file and let me know if it already exists", I
    ! initially thought that the snippet bellow would do the trick but I think
    ! that
    !   * status='unknown' makes the open operation implementation-defined.
    !   * status='replace' gives no way of telling if the file existed already
    !     or not.
    !
    !open(file='.fpm-package-lock', &
    !     action='read', &
    !     status='unknown', &
    !     newunit=lock_unit, &
    !     iostat=iostat, &
    !     iomsg=iomsg)
    !inquire(unit=lock_unit, exist=exists)

    call c_create('.fpm-package-lock'//c_null_char, iostat, c_iomsg_ptr, exists)

    ! An error occurred when opening the file.
    if (iostat /= 0) then
        if (present(success)) success = .false.

        ! Convert C pointer to Fortran pointer.
        call c_f_pointer(c_iomsg_ptr, c_iomsg, [1024])
        ! Convert Fortran pointer to Fortran string.
        iomsg = f_string(c_iomsg)
        !iomsg = f_string(c_iomsg_ptr)
        call fatal_error(error, "Error trying to delete lock-file: "//iomsg)

        call c_free(c_iomsg_ptr)
        return
    end if

    ! The lock-file already exists, so some other process probably has the lock.
    if (exists /= 0) then
        if (present(success)) success = .false.
        return
    end if

    ! At this point we have the lock.
    if (present(success)) success = .true.

    ! Setup the atexit handler
    call atexit(c_funloc(atexit_cleanup))
end subroutine fpm_lock_acquire_noblock

!> Try to acquire a lock on the current package directory. If some other process
!> already has a lock this function blocks until it can get the lock.
!> @note
!> You cannot use this function multiple times without calling
!> [[fpm_lock_release]] first.
!> @endnote
subroutine fpm_lock_acquire(error)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    logical :: got_lock

    character(len=:), allocatable :: cwd
    character(len=:), allocatable :: lockfile_path

    call get_current_directory(cwd, error)
    if (allocated(error)) return

    lockfile_path = join_path(cwd, '.fpm-pakage-lock')

    call fpm_lock_acquire_noblock(error, success=got_lock)
    if (allocated(error)) return

    if (.not. got_lock) then
        write(stderr, *) "Warning: file "//lockfile_path//" exists."
        write(stderr, *) ""
        write(stderr, *) "Another process seams to be working on this package"
        write(stderr, *) "already and this process will wait for .fpm-package-lock"
        write(stderr, *) "to be removed before proceeding. If you think that a"
        write(stderr, *) "previous process crashed/terminated without removing"
        write(stderr, *) ".fpm-package-lock then you can try removing it manually."
        write(stderr, *)
        write(stderr, *) "If the problem persists then please file a bug report"
        write(stderr, *) "at https://github.com/fortran-lang/fpm/issues"
    end if

    do while (.not. got_lock)
        call sleep(1) ! not very sophisticated but it works :-)
        call fpm_lock_acquire_noblock(error, success=got_lock)
        if (allocated(error)) return
    end do
end subroutine fpm_lock_acquire

!> Release a lock on the current package directory
!> @note
!> You can only release a lock if you acquired it with [[fpm_lock_acquire]]
!> first.
!> @endnote
subroutine fpm_lock_release(error)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: unit
    integer :: iostat
    character(len=256) :: iomsg

    open(file='.fpm-package-lock', &
              action='read', &
              status='old', &
              newunit=unit, &
              iostat=iostat, &
              iomsg=iomsg)

    if (iostat /= 0) then
        call fatal_error(error, "Error opening lock-file for deletion: "//iomsg)
        return
    end if

    close(unit=unit, &
          status='delete', &
          iostat=iostat)

    if (iostat /= 0) then
        call fatal_error(error, "Error closing lock-file")
        return
    end if
end subroutine fpm_lock_release

end module fpm_lock
