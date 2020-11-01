module fpm_backend

! Implements the native fpm build backend

use fpm_environment, only: run, get_os_type, OS_WINDOWS
use fpm_filesystem, only: basename, dirname, join_path, exists, mkdir
use fpm_model, only: fpm_model_t, srcfile_t, build_target_t, FPM_UNIT_MODULE, &
                     FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                     FPM_UNIT_CSOURCE, FPM_UNIT_PROGRAM, &
                     FPM_SCOPE_TEST, FPM_TARGET_OBJECT, FPM_TARGET_ARCHIVE, FPM_TARGET_EXECUTABLE
                     
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

    if (model%targets(1)%ptr%target_type == FPM_TARGET_ARCHIVE) then
        linking = ' -l'//model%package_name//" -L"//&
                    join_path(model%output_directory,model%package_name)
    else
        linking = " "
    end if

    do i=1,size(model%targets)
        
        call build_target(model,model%targets(i)%ptr,linking)
        
    end do

end subroutine build_package



recursive subroutine build_target(model,target,linking)
    ! Compile Fortran source, called recursively on it dependents
    !  
    type(fpm_model_t), intent(in) :: model
    type(build_target_t), intent(inout) :: target
    character(:), allocatable, intent(in) :: linking

    integer :: i, j
    type(build_target_t), pointer :: exe_obj
    character(:), allocatable :: objs

    if (target%built) then
        return
    end if

    if (target%touched) then
        write(*,*) '(!) Circular dependency found with: ',target%output_file
        stop
    else
        target%touched = .true.
    end if

    objs = " "

    do i=1,size(target%dependencies)

        if (associated(target%dependencies(i)%ptr)) then
            call build_target(model,target%dependencies(i)%ptr,linking)
        end if

        if (target%target_type == FPM_TARGET_ARCHIVE ) then

            ! Construct object list for archive
            objs = objs//" "//target%dependencies(i)%ptr%output_file

        else if (target%target_type == FPM_TARGET_EXECUTABLE .and. &
                 target%dependencies(i)%ptr%target_type ==  FPM_TARGET_OBJECT) then

            exe_obj => target%dependencies(i)%ptr
                
            ! Construct object list for executable
            objs = " "//exe_obj%output_file
                
            ! Include non-library object dependencies
            do j=1,size(exe_obj%dependencies)

                if (allocated(exe_obj%dependencies(j)%ptr%source)) then
                    if (exe_obj%dependencies(j)%ptr%source%unit_scope == exe_obj%source%unit_scope) then
                        objs = objs//" "//exe_obj%dependencies(j)%ptr%output_file
                    end if
                end if

            end do

        end if

    end do
    
    if (.not.exists(dirname(target%output_file))) then
        call mkdir(dirname(target%output_file))
    end if

    select case(target%target_type)

    case (FPM_TARGET_OBJECT)
        call run("gfortran -c " // target%source%file_name // model%fortran_compile_flags &
              // " -o " // target%output_file)

    case (FPM_TARGET_EXECUTABLE)
        call run("gfortran " // objs // model%fortran_compile_flags &
              //linking// " -o " // target%output_file)

    case (FPM_TARGET_ARCHIVE)
        call run("ar -rs " // target%output_file // objs)

    end select

    target%built = .true.

end subroutine build_target

end module fpm_backend
