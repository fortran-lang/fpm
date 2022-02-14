!># Build Backend Progress Output
!> This module provides a derived type `build_progress_t` for printing build status
!> and progress messages to the console while the backend is building the package.
!>
!> The `build_progress_t` type supports two modes: `normal` and `plain`
!> where the former does 'pretty' output and the latter does not.
!> The `normal` mode is intended for typical interactive usage whereas
!> 'plain' mode is used with the `--verbose` flag or when `stdout` is not attached
!> to a terminal (e.g. when piping or redirecting `stdout`). In these cases,
!> the pretty output must be suppressed to avoid control codes being output.

module fpm_backend_output
use iso_fortran_env, only: stdout=>output_unit
use fpm_filesystem, only: basename
use fpm_targets, only: build_target_ptr
use fpm_backend_console, only: console_t, LINE_RESET, COLOR_RED, COLOR_GREEN, COLOR_YELLOW, COLOR_RESET
implicit none

private
public build_progress_t

!> Build progress object
type build_progress_t
    !> Console object for updating console lines
    type(console_t) :: console
    !> Number of completed targets
    integer :: n_complete
    !> Total number of targets scheduled
    integer :: n_target
    !> 'Plain' output (no colors or updating)
    logical :: plain_mode = .true.
    !> Store needed when updating previous console lines
    integer, allocatable :: output_lines(:)
    !> Queue of scheduled build targets
    type(build_target_ptr), pointer :: target_queue(:)
contains
    !> Output 'compiling' status for build target
    procedure :: compiling_status => output_status_compiling
    !> Output 'complete' status for build target
    procedure :: completed_status => output_status_complete
    !> Output finished status for whole package
    procedure :: success => output_progress_success
end type build_progress_t

!> Constructor for build_progress_t
interface build_progress_t
    procedure :: new_build_progress
end interface build_progress_t

contains
    
    !> Initialise a new build progress object
    function new_build_progress(target_queue,plain_mode) result(progress)
        !> The queue of scheduled targets
        type(build_target_ptr), intent(in), target :: target_queue(:)
        !> Enable 'plain' output for progress object
        logical, intent(in), optional :: plain_mode
        !> Progress object to initialise
        type(build_progress_t) :: progress

        progress%n_target = size(target_queue,1)
        progress%target_queue => target_queue
        progress%plain_mode = plain_mode
        progress%n_complete = 0

        allocate(progress%output_lines(progress%n_target))

    end function new_build_progress

    !> Output 'compiling' status for build target and overall percentage progress
    subroutine output_status_compiling(progress, queue_index)
        !> Progress object
        class(build_progress_t), intent(inout) :: progress
        !> Index of build target in the target queue
        integer, intent(in) :: queue_index

        character(:), allocatable :: target_name
        character(100) :: output_string
        character(100) :: overall_progress

        associate(target=>progress%target_queue(queue_index)%ptr)

            if (allocated(target%source)) then
                target_name = basename(target%source%file_name)
            else
                target_name = basename(target%output_file)
            end if

            write(overall_progress,'(A,I4,A)') '[',100*progress%n_complete/progress%n_target,'%]'

            if (progress%plain_mode) then ! Plain output

                !$omp critical
                write(*,'(A8,A30)') trim(overall_progress),target_name
                !$omp end critical

            else ! Pretty output

                write(output_string,'(A,T40,A,A)') target_name, COLOR_YELLOW//'compiling...'//COLOR_RESET

                call progress%console%write_line(trim(output_string),progress%output_lines(queue_index))

                call progress%console%write_line(trim(overall_progress)//'Compiling...',advance=.false.)

            end if

        end associate

    end subroutine output_status_compiling

    !> Output 'complete' status for build target and update overall percentage progress
    subroutine output_status_complete(progress, queue_index, build_stat)
        !> Progress object
        class(build_progress_t), intent(inout) :: progress
        !> Index of build target in the target queue
        integer, intent(in) :: queue_index
        !> Build status flag
        integer, intent(in) :: build_stat

        character(:), allocatable :: target_name
        character(100) :: output_string
        character(100) :: overall_progress

        !$omp critical 
        progress%n_complete = progress%n_complete + 1
        !$omp end critical

        associate(target=>progress%target_queue(queue_index)%ptr)

            if (allocated(target%source)) then
                target_name = basename(target%source%file_name)
            else
                target_name = basename(target%output_file)
            end if

            if (build_stat == 0) then
                write(output_string,'(A,T40,A,A)') target_name,COLOR_GREEN//'done.'//COLOR_RESET
            else
                write(output_string,'(A,T40,A,A)') target_name,COLOR_RED//'failed.'//COLOR_RESET
            end if

            write(overall_progress,'(A,I4,A)') '[',100*progress%n_complete/progress%n_target,'%] '

            if (progress%plain_mode) then  ! Plain output

                !$omp critical
                write(*,'(A8,A30,A7)') trim(overall_progress),target_name, 'done.'
                !$omp end critical

            else ! Pretty output

                call progress%console%update_line(progress%output_lines(queue_index),trim(output_string))

                call progress%console%write_line(trim(overall_progress)//'Compiling...',advance=.false.)

            end if

        end associate

    end subroutine output_status_complete

    !> Output finished status for whole package
    subroutine output_progress_success(progress)
        class(build_progress_t), intent(inout) :: progress

        if (progress%plain_mode) then ! Plain output

            write(*,'(A)') '[100%] Project compiled successfully.'

        else ! Pretty output

            write(*,'(A)') LINE_RESET//COLOR_GREEN//'[100%] Project compiled successfully.'//COLOR_RESET

        end if

    end subroutine output_progress_success

end module fpm_backend_output