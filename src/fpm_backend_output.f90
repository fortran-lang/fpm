module fpm_backend_output
use iso_fortran_env, only: stdout=>output_unit
use fpm_filesystem, only: basename
use fpm_targets, only: build_target_ptr
use fpm_backend_console, only: console_t
use M_attr, only: attr, attr_mode
implicit none

type build_progress_t

    type(console_t) :: console

    integer :: n_complete
    
    integer :: n_target

    logical :: plain_mode = .true.

    integer, allocatable :: output_lines(:)

    type(build_target_ptr), pointer :: target_queue(:)

contains
    procedure :: init => output_init
    procedure :: compiling_status => output_status_compiling
    procedure :: completed_status => output_status_complete
    procedure :: success => output_progress_success

end type build_progress_t

contains
    
    subroutine output_init(progress,target_queue,plain_mode)
        class(build_progress_t), intent(out) :: progress
        type(build_target_ptr), intent(in), target :: target_queue(:)
        logical, intent(in), optional :: plain_mode
        
        if (plain_mode) then
            call attr_mode('plain')
        else
            call attr_mode('color')
        end if

        call progress%console%init(plain_mode)

        progress%n_target = size(target_queue,1)
        progress%target_queue => target_queue
        progress%plain_mode = plain_mode

        allocate(progress%output_lines(progress%n_target))

    end subroutine output_init

    subroutine output_status_compiling(progress, queue_index)
        class(build_progress_t), intent(inout) :: progress
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

            if (progress%plain_mode) then

                !$omp critical
                write(*,'(A8,A30)') trim(overall_progress),target_name
                !$omp end critical

            else

                write(output_string,'(A,T40,A,A)') target_name,attr('<yellow>compiling...</yellow>')
                call progress%console%write_line(trim(output_string),progress%output_lines(queue_index))

                call progress%console%write_line(trim(overall_progress)//'Compiling...',advance=.false.)

            end if

        end associate

    end subroutine output_status_compiling


    subroutine output_status_complete(progress, queue_index, build_stat)
        class(build_progress_t), intent(inout) :: progress
        integer, intent(in) :: queue_index
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
                write(output_string,'(A,T40,A,A)') target_name,attr('<green>done.</green>')
            else
                write(output_string,'(A,T40,A,A)') target_name,attr('<red>failed.</red>')
            end if

            write(overall_progress,'(A,I4,A)') '[',100*progress%n_complete/progress%n_target,'%] '

            if (progress%plain_mode) then

                !$omp critical
                write(*,'(A8,A30,A7)') trim(overall_progress),target_name, 'done.'
                !$omp end critical

            else

                call progress%console%update_line(progress%output_lines(queue_index),trim(output_string))

                call progress%console%write_line(trim(overall_progress)//'Compiling...',advance=.false.)

            end if

        end associate

    end subroutine output_status_complete

    subroutine output_progress_success(progress)
        class(build_progress_t), intent(inout) :: progress

        if (progress%plain_mode) then

            write(*,'(A)') attr('<green>[100%] Project compiled successfully.</green>')

        else

            write(*,'(A)') progress%console%LINE_RESET//attr('<green>[100%] Project compiled successfully.</green>')

        end if

    end subroutine output_progress_success

end module fpm_backend_output