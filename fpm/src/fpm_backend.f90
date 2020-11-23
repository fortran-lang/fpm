!> Implements the native fpm build backend
module fpm_backend

use fpm_environment, only: run
use fpm_filesystem, only: dirname, join_path, exists, mkdir
use fpm_model, only: fpm_model_t, build_target_t, build_target_ptr, &
                     FPM_TARGET_OBJECT, FPM_TARGET_ARCHIVE, FPM_TARGET_EXECUTABLE
                     
use fpm_strings, only: string_cat

implicit none

private
public :: build_package, sort_target, schedule_targets

contains

!> Top-level routine to build package described by `model`
subroutine build_package(model)
    type(fpm_model_t), intent(inout) :: model

    integer :: i, j
    type(build_target_ptr), allocatable :: queue(:)
    integer, allocatable :: schedule_ptr(:)

    ! Need to make output directory for include (mod) files
    if (.not.exists(join_path(model%output_directory,model%package_name))) then
        call mkdir(join_path(model%output_directory,model%package_name))
    end if

    ! Perform depth-first topological sort of targets
    do i=1,size(model%targets)
        
        call sort_target(model%targets(i)%ptr)
        
    end do

    ! Construct build schedule queue
    call schedule_targets(queue, schedule_ptr, model%targets)

    ! Loop over parallel schedule regions
    do i=1,size(schedule_ptr)-1

        ! Build targets in schedule region i
        !$OMP PARALLEL DO DEFAULT(SHARED)
        do j=schedule_ptr(i),(schedule_ptr(i+1)-1)

            call build_target(model,queue(j)%ptr)

        end do
        !$OMP END PARALLEL DO

    end do
    
end subroutine build_package


!> Topologically sort a target for scheduling by 
!>  recursing over it's dependencies.
!> 
!> Checks disk-cached source hashes to determine if objects are
!>  up-to-date. Up-to-date sources are tagged as skipped.
!>
recursive subroutine sort_target(target)
    type(build_target_t), intent(inout), target :: target

    integer :: i, j, fh, stat
    type(build_target_t), pointer :: exe_obj

    ! Check if target has already been processed (as a dependency)
    if (target%sorted .or. target%skip) then
        return
    end if

    ! Check for a circular dependency
    ! (If target has been touched but not processed)
    if (target%touched) then
        write(*,*) '(!) Circular dependency found with: ',target%output_file
        stop
    else
        target%touched = .true.  ! Set touched flag
    end if

    ! Load cached source file digest if present
    if (.not.allocated(target%digest_cached) .and. &
         exists(target%output_file) .and. &
         exists(target%output_file//'.digest')) then

        allocate(target%digest_cached)
        open(newunit=fh,file=target%output_file//'.digest',status='old')
        read(fh,*,iostat=stat) target%digest_cached
        close(fh)

        if (stat /= 0) then    ! Cached digest is not recognized
            deallocate(target%digest_cached)
        end if

    end if

    if (allocated(target%source)) then

        ! Skip if target is source-based and source file is unmodified
        if (allocated(target%digest_cached)) then
            if (target%digest_cached == target%source%digest) target%skip = .true.
        end if

    elseif (exists(target%output_file)) then

        ! Skip if target is not source-based and already exists
        target%skip = .true.

    end if

    ! Loop over target dependencies
    target%schedule = 1
    do i=1,size(target%dependencies)

        ! Sort dependency
        call sort_target(target%dependencies(i)%ptr)

        if (.not.target%dependencies(i)%ptr%skip) then

            ! Can't skip target if any dependency is not skipped
            target%skip = .false.

            ! Set target schedule after all of its dependencies
            target%schedule = max(target%schedule,target%dependencies(i)%ptr%schedule+1)

        end if

    end do
    
    ! Mark flag as processed: either sorted or skipped
    target%sorted = .not.target%skip

end subroutine sort_target


!> Construct a build schedule from the sorted targets.
!>
!> The schedule is broken into regions, described by `schedule_ptr`,
!>  where targets in each region can be compiled in parallel.
!>
subroutine schedule_targets(queue, schedule_ptr, targets)
    type(build_target_ptr), allocatable, intent(out) :: queue(:)
    integer, allocatable :: schedule_ptr(:)
    type(build_target_ptr), intent(in) :: targets(:)

    integer :: i, j
    integer :: n_schedule, n_sorted

    n_schedule = 0   ! Number of schedule regions
    n_sorted = 0     ! Total number of targets to build
    do i=1,size(targets)

        if (targets(i)%ptr%sorted) then
            n_sorted = n_sorted + 1
        end if
        n_schedule = max(n_schedule, targets(i)%ptr%schedule)

    end do

    allocate(queue(n_sorted))
    allocate(schedule_ptr(n_schedule+1))

    ! Construct the target queue and schedule region pointer
    n_sorted = 1
    schedule_ptr(n_sorted) = 1
    do i=1,n_schedule

        do j=1,size(targets)

            if (targets(j)%ptr%sorted) then
                if (targets(j)%ptr%schedule == i) then

                    queue(n_sorted)%ptr => targets(j)%ptr
                    n_sorted = n_sorted + 1
                end if
            end if

        end do

        schedule_ptr(i+1) = n_sorted

    end do

end subroutine schedule_targets


!> Call compile/link command for a single target.
!>
!> If successful, also caches the source file digest to disk.
!>
subroutine build_target(model,target)
    type(fpm_model_t), intent(in) :: model
    type(build_target_t), intent(in), target :: target

    integer :: ilib, fh
    character(:), allocatable :: link_flags

    if (.not.exists(dirname(target%output_file))) then
        call mkdir(dirname(target%output_file))
    end if

    select case(target%target_type)

    case (FPM_TARGET_OBJECT)
        call run("gfortran -c " // target%source%file_name // model%fortran_compile_flags &
              // " -o " // target%output_file)

    case (FPM_TARGET_EXECUTABLE)

        link_flags = string_cat(target%link_objects," ") 

        if (allocated(model%library_file)) then
            link_flags = link_flags//" "//model%library_file//" "//model%link_flags
        else
            link_flags = link_flags//" "//model%link_flags
        end if
        
        if (allocated(target%link_libraries)) then
            if (size(target%link_libraries) > 0) then
                link_flags = link_flags // " -l" // string_cat(target%link_libraries," -l")
            end if
        end if
        
        call run("gfortran " // model%fortran_compile_flags &
              //" "//link_flags// " -o " // target%output_file)

    case (FPM_TARGET_ARCHIVE)
        call run("ar -rs " // target%output_file // " " // string_cat(target%link_objects," "))

    end select

    if (allocated(target%source)) then
        open(newunit=fh,file=target%output_file//'.digest',status='unknown')
        write(fh,*) target%source%digest
        close(fh)
    end if

end subroutine build_target

end module fpm_backend
