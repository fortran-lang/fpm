module fpm_backend_output
use iso_fortran_env, only: stdout=>output_unit
use fpm_filesystem, only: basename
use fpm_targets, only: build_target_t
use fpm_backend_console, only: console_t
use M_attr, only: attr, attr_mode
implicit none


contains
    
    subroutine output_init(plain_mode)
        logical, intent(in), optional :: plain_mode
        
        if (plain_mode) then
            call attr_mode('plain')
        else
            call attr_mode('color')
        end if

    end subroutine output_init

    subroutine output_status_compiling(console, line, target)
        type(console_t), intent(inout), target :: console
        integer, intent(inout) :: line
        type(build_target_t), intent(in) :: target

        character(:), allocatable :: target_name
        character(100) :: output_string

        if (allocated(target%source)) then
            target_name = basename(target%source%file_name)
        else
            target_name = basename(target%output_file)
        end if

        write(output_string,'(A,T40,A,A)') target_name,attr('<yellow>compiling...</yellow>')

        line = console%write_line(trim(output_string))

    end subroutine output_status_compiling

    subroutine output_status_complete(console, line, target, build_stat, n_complete)
        type(console_t), intent(inout), target :: console
        integer, intent(in) :: line
        type(build_target_t), intent(in) :: target
        integer, intent(in) :: build_stat
        integer, intent(inout) :: n_complete

        character(:), allocatable :: target_name
        character(100) :: output_string

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
        
        call console%update_line(line,trim(output_string))

        !$omp critical 
        n_complete = n_complete + 1
        !$omp end critical

    end subroutine output_status_complete

    subroutine output_progress(n_complete, total, plain_mode)
        integer, intent(in) :: n_complete, total
        logical :: plain_mode

        character(:), allocatable :: advance

        if (plain_mode) then
            advance = "yes"
        else
            advance = "no"
        end if

        !$omp critical
        write(*,'(A,I4,A,A)',advance=advance) '[',100*n_complete/total,'%] Compiling project...'
        !$omp end critical

    end subroutine output_progress

    subroutine output_progress_complete()

        write(*,'(A)') char(27)//"[2K"//char(27)//"[1G"//attr('<green>[100%] Project compiled successfully.</green>')

    end subroutine output_progress_complete

end module fpm_backend_output