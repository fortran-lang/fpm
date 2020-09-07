module fpm_backend

! Implements the native fpm build backend

use fpm_environment, only: run
use fpm_filesystem, only: basename, join_path, exists, mkdir
use fpm_model, only: fpm_model_t
use fpm_sources, only: srcfile_t, FPM_UNIT_MODULE, FPM_UNIT_SUBMODULE, &
                       FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, FPM_UNIT_PROGRAM
use fpm_strings, only: split

implicit none

private
public :: build_package

contains


subroutine build_package(model)
    type(fpm_model_t), intent(inout) :: model

    integer :: i
    character(:), allocatable :: base, linking, subdir

    if (.not.exists(model%output_directory)) then
        call mkdir(model%output_directory)
    end if
    if (.not.exists(join_path(model%output_directory,model%package_name))) then
        call mkdir(join_path(model%output_directory,model%package_name))
    end if

    linking = ""
    do i=1,size(model%sources)

        if (model%sources(i)%unit_type == FPM_UNIT_MODULE .or. &
            model%sources(i)%unit_type == FPM_UNIT_SUBMODULE .or. &
            model%sources(i)%unit_type == FPM_UNIT_SUBPROGRAM .or. &
            model%sources(i)%unit_type == FPM_UNIT_CSOURCE) then
        
            call build_source(model,model%sources(i),linking)

        end if
        
    end do

    if (any([(model%sources(i)%unit_type == FPM_UNIT_PROGRAM,i=1,size(model%sources))])) then
        if (.not.exists(join_path(model%output_directory,'test'))) then
            call mkdir(join_path(model%output_directory,'test'))
        end if
        if (.not.exists(join_path(model%output_directory,'app'))) then
            call mkdir(join_path(model%output_directory,'app'))
        end if
    end if

    do i=1,size(model%sources)

        if (model%sources(i)%unit_type == FPM_UNIT_PROGRAM) then
            
            base = basename(model%sources(i)%file_name,suffix=.false.)
            
            if (model%sources(i)%is_test) then
                subdir = 'test'
            else
                subdir = 'app'
            end if
            
            call run("gfortran -c " // model%sources(i)%file_name // ' '//model%fortran_compile_flags &
                      // " -o " // join_path(model%output_directory,subdir,base) // ".o")

            call run("gfortran " // join_path(model%output_directory, subdir, base) // ".o "// &
                     linking //" " //model%link_flags // " -o " // &
                      join_path(model%output_directory,subdir,model%sources(i)%exe_name) )

        end if

    end do

end subroutine build_package



recursive subroutine build_source(model,source_file,linking)
    ! Compile Fortran source, called recursively on it dependents
    !  
    type(fpm_model_t), intent(in) :: model
    type(srcfile_t), intent(inout) :: source_file
    character(:), allocatable, intent(inout) :: linking

    integer :: i
    character(:), allocatable :: object_file

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
            call build_source(model,source_file%file_dependencies(i)%ptr,linking)
        end if

    end do

    object_file = join_path(model%output_directory, model%package_name, &
                        basename(source_file%file_name,suffix=.false.)//'.o')
    
    call run("gfortran -c " // source_file%file_name // model%fortran_compile_flags &
              // " -o " // object_file)
    linking = linking // " " // object_file

    source_file%built = .true.

end subroutine build_source

end module fpm_backend
