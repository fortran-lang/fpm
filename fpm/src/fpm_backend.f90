module fpm_backend
use fpm_strings
use fpm_environment
use fpm_sources
use fpm_model
implicit none


contains


subroutine build_package(model)
    type(fpm_model_t), intent(inout) :: model

    integer :: i
    character(:), allocatable :: basename, linking
    character(:), allocatable :: file_parts(:)

    linking = ""
    do i=1,size(model%sources)

        if (model%sources(i)%unit_type == FPM_UNIT_MODULE .or. &
            model%sources(i)%unit_type == FPM_UNIT_SUBMODULE .or. &
            model%sources(i)%unit_type == FPM_UNIT_SUBPROGRAM .or. &
            model%sources(i)%unit_type == FPM_UNIT_CSOURCE) then
        
            call build_source(model%sources(i),linking)

        end if
        
    end do

    do i=1,size(model%sources)

        if (model%sources(i)%unit_type == FPM_UNIT_PROGRAM) then
            
            call split(model%sources(i)%file_name,file_parts,delimiters='\/.')
            basename = file_parts(size(file_parts)-1)
            
            call run("gfortran " // model%sources(i)%file_name // linking // " -o " // basename)

        end if

    end do

end subroutine build_package



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
        write(*,*) '(!) Circular dependency found with: ',source_file%file_name
        stop
    else
        source_file%touched = .true.
    end if

    do i=1,size(source_file%file_dependencies)

        if (associated(source_file%file_dependencies(i)%ptr)) then
            call build_source(source_file%file_dependencies(i)%ptr,linking)
        end if

    end do

    call split(source_file%file_name,file_parts,delimiters='\/.')
    basename = file_parts(size(file_parts)-1)
    
    call run("gfortran -c " // source_file%file_name // " -o " // basename // ".o")
    linking = linking // " " // basename // ".o"

    source_file%built = .true.

end subroutine build_source

end module fpm_backend