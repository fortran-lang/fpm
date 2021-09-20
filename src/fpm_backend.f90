!># Build backend
!> Uses a list of `[[build_target_ptr]]` and a valid `[[fpm_model]]` instance
!> to schedule and execute the compilation and linking of package targets.
!>
!> The package build process (`[[build_package]]`) comprises three steps:
!>
!> 1. __Target sorting:__ topological sort of the target dependency graph (`[[sort_target]]`)
!> 2. __Target scheduling:__ group targets into schedule regions based on the sorting (`[[schedule_targets]]`)
!> 3. __Target building:__ generate targets by compilation or linking
!>
!> @note If compiled with OpenMP, targets will be build in parallel where possible.
!>
!>### Incremental compilation
!> The backend process supports *incremental* compilation whereby targets are not
!> re-compiled if their corresponding dependencies have not been modified.
!>
!> - Source-based targets (*i.e.* objects) are not re-compiled if the corresponding source
!>   file is unmodified AND all of the target dependencies are not marked for re-compilation
!>
!> - Link targets (*i.e.* executables and libraries) are not re-compiled if the
!>   target output file already exists AND all of the target dependencies are not marked for
!>   re-compilation
!>
!> Source file modification is determined by a file digest (hash) which is calculated during
!> the source parsing phase ([[fpm_source_parsing]]) and cached to disk after a target is
!> successfully generated.
!>
module fpm_backend

use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use fpm_error, only : fpm_stop
use fpm_environment, only: run, get_os_type, OS_WINDOWS
use fpm_filesystem, only: basename, dirname, join_path, exists, mkdir
use fpm_model, only: fpm_model_t
use fpm_strings, only: string_t, operator(.in.)
use fpm_targets, only: build_target_t, build_target_ptr, FPM_TARGET_OBJECT, &
                       FPM_TARGET_C_OBJECT, FPM_TARGET_ARCHIVE, FPM_TARGET_EXECUTABLE
implicit none

private
public :: build_package, sort_target, schedule_targets

contains

!> Top-level routine to build package described by `model`
subroutine build_package(targets,model)
    type(build_target_ptr), intent(inout) :: targets(:)
    type(fpm_model_t), intent(in) :: model

    integer :: i, j
    type(build_target_ptr), allocatable :: queue(:)
    integer, allocatable :: schedule_ptr(:), stat(:)
    logical :: build_failed, skip_current
    type(string_t), allocatable :: build_dirs(:)
    type(string_t) :: temp

    ! Need to make output directory for include (mod) files
    !if (.not.exists(join_path(model%output_directory,model%package_name))) then
        !call mkdir(join_path(model%output_directory,model%package_name))
    !end if
    allocate(build_dirs(0))
    do i = 1, size(targets)
       associate(target => targets(i)%ptr)
          if (target%output_dir .in. build_dirs) cycle
          temp%s = target%output_dir
          build_dirs = [build_dirs, temp]
       end associate
    end do

    do i = 1, size(build_dirs)
       call mkdir(build_dirs(i)%s)
    end do

    ! Perform depth-first topological sort of targets
    do i=1,size(targets)

        call sort_target(targets(i)%ptr)

    end do

    ! Construct build schedule queue
    call schedule_targets(queue, schedule_ptr, targets)

    ! Initialise build status flags
    allocate(stat(size(queue)))
    stat(:) = 0
    build_failed = .false.

    ! Loop over parallel schedule regions
    do i=1,size(schedule_ptr)-1

        ! Build targets in schedule region i
        !$omp parallel do default(shared) private(skip_current) schedule(dynamic,1)
        do j=schedule_ptr(i),(schedule_ptr(i+1)-1)

            ! Check if build already failed
            !$omp atomic read
            skip_current = build_failed

            if (.not.skip_current) then
                call build_target(model,queue(j)%ptr,stat(j))
            end if

            ! Set global flag if this target failed to build
            if (stat(j) /= 0) then
                !$omp atomic write
                build_failed = .true.
            end if

        end do

        ! Check if this schedule region failed: exit with message if failed
        if (build_failed) then
            do j=1,size(stat)
                if (stat(j) /= 0) then
                    write(stderr,'(*(g0:,1x))') '<ERROR> Compilation failed for object "',basename(queue(j)%ptr%output_file),'"'
                end if
            end do
            call fpm_stop(1,'stopping due to failed compilation')
        end if

    end do

end subroutine build_package


!> Topologically sort a target for scheduling by
!>  recursing over its dependencies.
!>
!> Checks disk-cached source hashes to determine if objects are
!>  up-to-date. Up-to-date sources are tagged as skipped.
!>
!> On completion, `target` should either be marked as
!> sorted (`target%sorted=.true.`) or skipped (`target%skip=.true.`)
!>
!> If `target` is marked as sorted, `target%schedule` should be an
!> integer greater than zero indicating the region for scheduling
!>
recursive subroutine sort_target(target)
    type(build_target_t), intent(inout), target :: target

    integer :: i, fh, stat

    ! Check if target has already been processed (as a dependency)
    if (target%sorted .or. target%skip) then
        return
    end if

    ! Check for a circular dependency
    ! (If target has been touched but not processed)
    if (target%touched) then
        call fpm_stop(1,'(!) Circular dependency found with: '//target%output_file)
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
subroutine build_target(model,target,stat)
    type(fpm_model_t), intent(in) :: model
    type(build_target_t), intent(in), target :: target
    integer, intent(out) :: stat

    integer :: fh

    if (.not.exists(dirname(target%output_file))) then
        call mkdir(dirname(target%output_file))
    end if

    select case(target%target_type)

    case (FPM_TARGET_OBJECT)
        call model%compiler%compile_fortran(target%source%file_name, target%output_file, &
            & target%compile_flags, stat)

    case (FPM_TARGET_C_OBJECT)
        call model%compiler%compile_c(target%source%file_name, target%output_file, &
            & target%compile_flags, stat)

    case (FPM_TARGET_EXECUTABLE)
        call model%compiler%link(target%output_file, &
            & target%compile_flags//" "//target%link_flags, stat)

    case (FPM_TARGET_ARCHIVE)
        call model%archiver%make_archive(target%output_file, target%link_objects, stat)

    end select

    if (stat == 0 .and. allocated(target%source)) then
        open(newunit=fh,file=target%output_file//'.digest',status='unknown')
        write(fh,*) target%source%digest
        close(fh)
    end if

end subroutine build_target


end module fpm_backend
