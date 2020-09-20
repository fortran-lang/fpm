module fpm_backend

! Implements the native fpm build backend

use fpm_environment, only: run, get_os_type, OS_WINDOWS
use fpm_filesystem, only: basename, join_path, exists, mkdir
use fpm_model, only: fpm_model_t, srcfile_t, FPM_UNIT_MODULE, &
                     FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                     FPM_UNIT_CSOURCE, FPM_UNIT_PROGRAM
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

    object_file = get_object_name(model,source_file%file_name)
    
    call run("gfortran -c " // source_file%file_name // model%fortran_compile_flags &
              // " -o " // object_file)
    linking = linking // " " // object_file

    source_file%built = .true.

end subroutine build_source


function get_object_name(model,source_file_name) result(object_file)
    ! Generate object target path from source name and model params
    !  
    !  src/test.f90        ->  <output-dir>/<package-name>/test.o
    !  src/subdir/test.f90 ->  <output-dir>/<package-name>/subdir_test.o
    !
    type(fpm_model_t), intent(in) :: model
    character(*), intent(in) :: source_file_name
    character(:), allocatable :: object_file

    integer :: i
    character(1) :: filesep

    select case(get_os_type())
    case (OS_WINDOWS)
        filesep = '\'
    case default
        filesep = '/'
    end select

    ! Exclude first directory level from path
    object_file = source_file_name(index(source_file_name,filesep)+1:)

    ! Convert remaining directory separators to underscores
    i = index(object_file,filesep)
    do while(i > 0)
        object_file(i:i) = '_'
        i = index(object_file,filesep)
    end do

    ! Construct full target path
    object_file = join_path(model%output_directory, model%package_name, &
                        basename(object_file,suffix=.false.)//'.o')

end function get_object_name

end module fpm_backend
