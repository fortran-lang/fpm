!># Build backend
!> Uses a list of `[[build_target_ptr]]` and a valid `[[fpm_model]]` instance
!> to schedule and execute the compilation and linking of package targets.
!>
!> The package build process (`[[build_package]]`) comprises three steps:
!>
!> 1. __Target sorting:__ topological sort of the target dependency graph (`[[sort_target]]`)
!> 2. __Target scheduling:__ group targets into schedule regions based on the sorting (`[[schedule_targets]]`),
!>    and link the queued targets to the ones waiting on them (`[[schedule_graph]]`)
!> 3. __Target building:__ generate targets by compilation or linking, each as soon as
!>    the targets it depends on are built
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
use fpm_error, only : fpm_stop, error_t
use fpm_filesystem, only: basename, dirname, join_path, exists, mkdir, run, getline, is_newer
use fpm_model, only: fpm_model_t
use fpm_compiler, only: append_clean_flags
use fpm_strings, only: string_t, operator(.in.)
use fpm_targets, only: build_target_t, build_target_ptr, FPM_TARGET_OBJECT, &
                       FPM_TARGET_C_OBJECT, FPM_TARGET_ARCHIVE, FPM_TARGET_EXECUTABLE, &
                       FPM_TARGET_CPP_OBJECT, FPM_TARGET_SHARED
use fpm_backend_output
use fpm_compile_commands, only: compile_command_table_t
implicit none

private
public :: build_package, sort_target, schedule_targets, schedule_graph, take_ready_target

#ifndef FPM_BOOTSTRAP
interface
    function c_isatty() bind(C, name = 'c_isatty')
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: c_isatty
    end function

    subroutine c_sleep_ms(ms) bind(C, name = 'c_sleep_ms')
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int), value :: ms
    end subroutine
end interface
#endif

contains

!> Top-level routine to build package described by `model`
subroutine build_package(targets,model,verbose,dry_run)
    type(build_target_ptr), intent(inout) :: targets(:)
    type(fpm_model_t), intent(in) :: model
    logical, intent(in) :: verbose
    
    !> If dry_run, the build process is only mocked, but the list of compile_commands 
    !> is still created
    logical, intent(in) :: dry_run
 
    integer :: i, j, k, n_ready, n_running
    type(build_target_ptr), allocatable :: queue(:)
    integer, allocatable :: schedule_ptr(:), stat(:)
    integer, allocatable :: n_waiting(:), next_ptr(:), next_idx(:), height(:), ready(:)
    logical :: build_failed, finished
    type(string_t), allocatable :: build_dirs(:)
    type(string_t) :: temp
    type(error_t), allocatable :: error

    type(build_progress_t) :: progress
    logical :: plain_output

    ! Need to make output directory for include (mod) files
    allocate(build_dirs(0))
    do i = 1, size(targets)
       associate(target => targets(i)%ptr)
          if (target%output_dir .in. build_dirs) cycle
          temp%s = target%output_dir
          build_dirs = [build_dirs, temp]
       end associate
    end do

    do i = 1, size(build_dirs)
       if (.not.dry_run) call mkdir(build_dirs(i)%s,verbose)
    end do

    ! Perform depth-first topological sort of targets
    do i=1,size(targets)

        call sort_target(targets(i)%ptr, dry_run)

    end do

    ! Construct build schedule queue
    call schedule_targets(queue, schedule_ptr, targets)

    ! Check if queue is empty
    if (.not.verbose .and. size(queue) < 1 .and. .not.dry_run) then
        write(stderr, '(a)') 'Project is up to date'
        return
    end if

    ! Initialise build status flags
    allocate(stat(size(queue)),source=0)
    build_failed = .false.

    ! Set output mode
#ifndef FPM_BOOTSTRAP
    plain_output = (.not.(c_isatty()==1)) .or. verbose
#else
    plain_output = .true.
#endif

    progress = build_progress_t(queue,plain_output,model%build_dir)

    ! Build each target as soon as the targets it depends on are built. Building one
    ! schedule region at a time would hold every region open for its slowest target,
    ! while targets of later regions whose own dependencies are done could already run.
    call schedule_graph(queue, schedule_ptr, n_waiting, next_ptr, next_idx, height)

    allocate(ready(size(queue)))
    n_ready = 0
    do j = 1, size(queue)
        if (n_waiting(j) == 0) then
            n_ready = n_ready + 1
            ready(n_ready) = j
        end if
    end do
    n_running = 0

    !$omp parallel default(shared) private(j, k, finished)
    do

        ! Take the ready target to build next; after a failure, start no more
        !$omp critical (fpm_build_schedule)
        j = 0
        if (.not.build_failed) call take_ready_target(ready, n_ready, height, j)
        if (j > 0) n_running = n_running + 1
        finished = j == 0 .and. (build_failed .or. n_running == 0)
        !$omp end critical (fpm_build_schedule)

        if (finished) exit

        ! Nothing is ready yet: a running target will release the ones waiting on it
        if (j == 0) then
            call wait_for_ready_target()
            cycle
        end if

        if (.not.dry_run) call progress%compiling_status(j)
        call build_target(model,queue(j)%ptr,verbose,dry_run, &
                          progress%compile_commands,stat(j))
        if (.not.dry_run) call progress%completed_status(j,stat(j))

        ! Count this target as built for the targets waiting on it, and release
        ! those for which it was the last one
        !$omp critical (fpm_build_schedule)
        n_running = n_running - 1
        if (stat(j) /= 0) then
            build_failed = .true.
        else
            do k = next_ptr(j), next_ptr(j+1) - 1
                n_waiting(next_idx(k)) = n_waiting(next_idx(k)) - 1
                if (n_waiting(next_idx(k)) == 0) then
                    n_ready = n_ready + 1
                    ready(n_ready) = next_idx(k)
                end if
            end do
        end if
        !$omp end critical (fpm_build_schedule)

    end do
    !$omp end parallel

    ! Exit with a message if any target failed
    if (build_failed) then
        write(*,*)
        do j=1,size(stat)
            if (stat(j) /= 0) Then
                call print_build_log(queue(j)%ptr)
            end if
        end do
        do j=1,size(stat)
            if (stat(j) /= 0) then
                write(stderr,'(*(g0:,1x))') '<ERROR> Compilation failed for object "',basename(queue(j)%ptr%output_file),'"'
            end if
        end do
        call fpm_stop(1,'stopping due to failed compilation')
    end if

    if (.not.dry_run) call progress%success()
    call progress%dump_commands(error)
    if (allocated(error)) call fpm_stop(1,'error writing compile_commands.json: '//trim(error%message))

end subroutine build_package


!> Topologically sort a target for scheduling by
!>  recursing over its dependencies.
!>
!> Checks disk-cached source hashes to determine if objects are
!>  up-to-date. Up-to-date sources are tagged as skipped. A target is
!>  rebuilt nonetheless when the output of one of its dependencies is
!>  newer than its own: a dependency rebuilt by an earlier call that left
!>  this target out.
!>
!> On completion, `target` should either be marked as
!> sorted (`target%sorted=.true.`) or skipped (`target%skip=.true.`)
!>
!> If `target` is marked as sorted, `target%schedule` should be an
!> integer greater than zero indicating the region for scheduling
!>
recursive subroutine sort_target(target, mock)
    type(build_target_t), intent(inout), target :: target
    !> Optionally sort ALL targets if this is a dry run
    logical, optional, intent(in) :: mock

    integer :: i, fh, stat
    logical :: dry_run
    
    dry_run = .false.
    if (present(mock)) dry_run = mock

    ! Check if target has already been processed (as a dependency)
    if (target%sorted .or. target%skip) return

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
         exists(target%output_file//'.digest') .and. &
         (.not.dry_run)) then 

        allocate(target%digest_cached)
        open(newunit=fh,file=target%output_file//'.digest',status='old')
        read(fh,*,iostat=stat) target%digest_cached
        close(fh)

        ! Cached digest is not recognized
        if (stat /= 0) deallocate(target%digest_cached)

    end if
    
    if (dry_run) then 
        
        target%skip = .false.
        
    elseif (allocated(target%source)) then

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
        call sort_target(target%dependencies(i)%ptr, dry_run)

        if (.not.target%dependencies(i)%ptr%skip) then

            ! Can't skip target if any dependency is not skipped
            target%skip = .false.

            ! Set target schedule after all of its dependencies
            target%schedule = max(target%schedule,target%dependencies(i)%ptr%schedule+1)

        elseif (target%skip) then

            ! Nor if a dependency's output is newer than this target's: an earlier call rebuilt
            ! the dependency without this target -- `fpm build`, which leaves the tests out,
            ! before `fpm test` -- and skipping would leave this target linked or compiled
            ! against the old one
            if (is_newer(target%dependencies(i)%ptr%output_file, target%output_file)) &
                target%skip = .false.

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


!> Link each queued target to the queued targets waiting on it, for building every
!> target as soon as the targets it depends on are built.
!>
!> `queue` and `schedule_ptr` are as returned by `[[schedule_targets]]`. A dependency that
!> is up to date was never queued and is already satisfied, so `n_waiting(j)` counts only
!> the queued dependencies of `queue(j)`. The targets waiting on `queue(j)` are
!> `queue(next_idx(next_ptr(j):next_ptr(j+1)-1))`, all later in the queue than `j`.
!> `height(j)` is the number of targets in the longest chain of queued targets that starts
!> at `queue(j)` and runs through the targets waiting on it: building the ready target
!> with the largest height first keeps the longest chains moving.
subroutine schedule_graph(queue, schedule_ptr, n_waiting, next_ptr, next_idx, height)
    !> Build queue
    type(build_target_ptr), intent(in) :: queue(:)
    !> Start of each schedule region in `queue`, and one past its end
    integer, intent(in) :: schedule_ptr(:)
    !> Number of queued dependencies of each queued target
    integer, allocatable, intent(out) :: n_waiting(:)
    !> Start of each target's waiting targets in `next_idx`, and one past the last
    integer, allocatable, intent(out) :: next_ptr(:)
    !> Queue positions of the targets waiting on each target
    integer, allocatable, intent(out) :: next_idx(:)
    !> Length of the longest chain of queued targets starting at each target
    integer, allocatable, intent(out) :: height(:)

    integer :: i, j, k, n_edge
    integer, allocatable :: edge_from(:), edge_to(:), fill(:)

    ! Find the queue position of every queued dependency once
    n_edge = 0
    do j = 1, size(queue)
        n_edge = n_edge + size(queue(j)%ptr%dependencies)
    end do
    allocate(edge_from(n_edge), edge_to(n_edge))

    n_edge = 0
    do j = 1, size(queue)
        do i = 1, size(queue(j)%ptr%dependencies)
            k = queue_position(queue, schedule_ptr, queue(j)%ptr%dependencies(i))
            if (k == 0) cycle
            n_edge = n_edge + 1
            edge_from(n_edge) = k
            edge_to(n_edge) = j
        end do
    end do

    ! Group the edges by the target they start from
    allocate(n_waiting(size(queue)), source=0)
    allocate(next_ptr(size(queue)+1), source=0)
    do i = 1, n_edge
        n_waiting(edge_to(i)) = n_waiting(edge_to(i)) + 1
        next_ptr(edge_from(i)+1) = next_ptr(edge_from(i)+1) + 1
    end do
    next_ptr(1) = 1
    do j = 1, size(queue)
        next_ptr(j+1) = next_ptr(j+1) + next_ptr(j)
    end do

    allocate(next_idx(n_edge))
    fill = next_ptr(1:size(queue))
    do i = 1, n_edge
        next_idx(fill(edge_from(i))) = edge_to(i)
        fill(edge_from(i)) = fill(edge_from(i)) + 1
    end do

    ! A waiting target is in a later schedule region, so later in the queue
    allocate(height(size(queue)))
    do j = size(queue), 1, -1
        height(j) = 1
        do k = next_ptr(j), next_ptr(j+1) - 1
            height(j) = max(height(j), height(next_idx(k)) + 1)
        end do
    end do

end subroutine schedule_graph


!> Position of target `dep` in the build queue, or 0 when it is not queued.
!>
!> A queued target sits in the schedule region numbered by its `schedule`, so only
!> that region is searched.
function queue_position(queue, schedule_ptr, dep) result(k)
    !> Build queue
    type(build_target_ptr), intent(in) :: queue(:)
    !> Start of each schedule region in `queue`, and one past its end
    integer, intent(in) :: schedule_ptr(:)
    !> Target to look for
    type(build_target_ptr), intent(in) :: dep
    !> Position of `dep` in `queue`, or 0
    integer :: k

    integer :: s

    k = 0
    if (.not.dep%ptr%sorted) return

    s = dep%ptr%schedule
    if (s < 1 .or. s >= size(schedule_ptr)) return

    do k = schedule_ptr(s), schedule_ptr(s+1) - 1
        if (associated(queue(k)%ptr, dep%ptr)) return
    end do
    k = 0

end function queue_position


!> Remove from `ready` the target to build next and return its queue position in `j`,
!> or 0 when no target is ready.
!>
!> The target chosen has the largest `height` (the longest chain of targets waiting on
!> it), and is the earliest in the queue among those with that height.
subroutine take_ready_target(ready, n_ready, height, j)
    !> Queue positions of the targets ready to build, in `ready(1:n_ready)`
    integer, intent(inout) :: ready(:)
    !> Number of targets ready to build
    integer, intent(inout) :: n_ready
    !> Length of the longest chain of queued targets starting at each target
    integer, intent(in) :: height(:)
    !> Queue position of the target taken, or 0
    integer, intent(out) :: j

    integer :: i, best

    j = 0
    if (n_ready < 1) return

    best = 1
    do i = 2, n_ready
        if (height(ready(i)) > height(ready(best))) then
            best = i
        else if (height(ready(i)) == height(ready(best))) then
            if (ready(i) < ready(best)) best = i
        end if
    end do

    j = ready(best)
    ready(best) = ready(n_ready)
    n_ready = n_ready - 1

end subroutine take_ready_target


!> Pause a worker that found no target ready to build, until a running target may
!> have released the ones waiting on it.
!>
!> The bootstrap build has no C helpers: it does not pause, which is harmless when it
!> is built without OpenMP and so has a single worker that never waits.
subroutine wait_for_ready_target()
#ifndef FPM_BOOTSTRAP
    use, intrinsic :: iso_c_binding, only: c_int

    !> Milliseconds to pause between looks at the ready targets
    integer(c_int), parameter :: pause_ms = 2_c_int

    call c_sleep_ms(pause_ms)
#endif
end subroutine wait_for_ready_target


!> Call compile/link command for a single target.
!>
!> If successful, also caches the source file digest to disk.
!>
subroutine build_target(model,target,verbose,dry_run,table,stat)
    type(fpm_model_t), intent(in) :: model
    type(build_target_t), intent(in), target :: target
    logical, intent(in) :: verbose
    !> If dry_run, the build process is only mocked, but compile_commands are still created
    logical, intent(in) :: dry_run    
    type(compile_command_table_t), intent(inout) :: table
    integer, intent(out) :: stat

    integer :: fh
    character(len=:), allocatable :: exe_flags

    !$omp critical
    if (.not.exists(dirname(target%output_file)) .and. .not.dry_run) then
        call mkdir(dirname(target%output_file),verbose)
    end if
    !$omp end critical

    select case(target%target_type)

    case (FPM_TARGET_OBJECT)
        call model%compiler%compile_fortran(target%source%file_name, target%output_file, &
            & target%compile_flags, target%output_log_file, stat, table, dry_run)

    case (FPM_TARGET_C_OBJECT)
        call model%compiler%compile_c(target%source%file_name, target%output_file, &
            & target%compile_flags, target%output_log_file, stat, table, dry_run)

    case (FPM_TARGET_CPP_OBJECT)
        call model%compiler%compile_cpp(target%source%file_name, target%output_file, &
            & target%compile_flags, target%output_log_file, stat, table, dry_run)

    case (FPM_TARGET_EXECUTABLE)
        ! Executables link with the compile and link flags combined, and a metapackage
        ! legitimately contributes the same flag to both (OpenMP sets a compile flag and
        ! a link flag). Merge them cleanly so a strict compiler is not handed the option
        ! twice: NAG rejects a repeated `-openmp` outright
        exe_flags = target%compile_flags
        call append_clean_flags(exe_flags, target%link_flags)
        call model%compiler%link(target%output_file, &
            & exe_flags, target%output_log_file, stat, dry_run)

    case (FPM_TARGET_ARCHIVE)
        call model%archiver%make_archive(target%output_file, target%link_objects, &
            & target%output_log_file, stat, dry_run)
            
    case (FPM_TARGET_SHARED)

        call model%compiler%link_shared(target%output_file, target%link_flags, &
            & target%output_log_file, stat, dry_run)

    end select

    if (stat == 0 .and. allocated(target%source) .and. .not.dry_run) then
        open(newunit=fh,file=target%output_file//'.digest',status='unknown')
        write(fh,*) target%source%digest
        close(fh)
    end if

end subroutine build_target


!> Read and print the build log for target
!>
subroutine print_build_log(target)
    type(build_target_t), intent(in), target :: target

    integer :: fh, ios
    character(:), allocatable :: line

    if (exists(target%output_log_file)) then

        open(newunit=fh,file=target%output_log_file,status='old')
        do
            call getline(fh, line, ios)
            if (ios /= 0) exit
            write(*,'(A)') trim(line)
        end do
        close(fh)

    else

        write(stderr,'(*(g0:,1x))') '<ERROR> Unable to find build log "',basename(target%output_log_file),'"'

    end if

end subroutine print_build_log

end module fpm_backend
