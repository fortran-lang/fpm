module FPM_Backend
use FPM_Strings
use FPM_Model
use environment
implicit none


contains

recursive subroutine build_source(source_file,linking)
    ! Compile Fortran source, called recursively on it dependents
    !  
    type(srcfile_t), intent(inout) :: source_file
    character(:), allocatable, intent(inout) :: linking

    integer :: n, i
    character(:), allocatable :: file_parts(:)
    character(:), allocatable :: basename

    if (source_file%built) then
        return
    end if

    if (source_file%touched) then
        write(*,*) '(!) Circular dependency found with: ',source_file%unit_name
        stop
    else
        source_file%touched = .true.
    end if

    do i=1,size(source_file%file_dependencies)

        call build_source(source_file%file_dependencies(i)%ptr,linking)

    end do

    call split(source_file%file_name,file_parts,delimiters='\/.')
    basename = file_parts(size(file_parts)-1)
    
    call run("gfortran -c " // source_file%file_name // " -o " // basename // ".o")
    linking = linking // " " // basename // ".o"

    source_file%built = .true.

end subroutine build_source

end module FPM_Backend