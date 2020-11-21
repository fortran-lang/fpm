module fpm_backend

! Implements the native fpm build backend

use fpm_environment, only: run, get_os_type, OS_WINDOWS
use fpm_filesystem, only: basename, dirname, join_path, exists, mkdir
use fpm_model, only: fpm_model_t, srcfile_t, build_target_t, build_target_ptr, &
                      FPM_UNIT_MODULE, FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                     FPM_UNIT_CSOURCE, FPM_UNIT_PROGRAM, &
                     FPM_SCOPE_TEST, FPM_TARGET_OBJECT, FPM_TARGET_ARCHIVE, FPM_TARGET_EXECUTABLE
                     
use fpm_strings, only: split

implicit none

private
public :: build_package

contains


subroutine build_package(model)
    type(fpm_model_t), intent(inout) :: model

    integer :: i, j
    type(build_target_ptr), allocatable :: queue(:)
    integer, allocatable :: region_ptr(:)

    if (.not.exists(model%output_directory)) then
        call mkdir(model%output_directory)
    end if
    if (.not.exists(join_path(model%output_directory,model%package_name))) then
        call mkdir(join_path(model%output_directory,model%package_name))
    end if

    do i=1,size(model%targets)
        
        call schedule_target(model%targets(i)%ptr)
        
    end do

    call get_build_queue(queue, region_ptr, model%targets)

    do i=1,size(region_ptr)-1

        !$OMP PARALLEL DO DEFAULT(SHARED)
        do j=region_ptr(i),(region_ptr(i+1)-1)

            call build_target(model,queue(j)%ptr)

        end do
        !$OMP END PARALLEL DO

    end do

    
end subroutine build_package



recursive subroutine schedule_target(target)
    ! 
    !  
    type(build_target_t), intent(inout), target :: target

    integer :: i, j, fh, stat
    type(build_target_t), pointer :: exe_obj

    if (target%scheduled .or. target%skip) then
        return
    end if

    if (.not.exists(dirname(target%output_file))) then
        call mkdir(dirname(target%output_file))
    end if

    if (target%touched) then
        write(*,*) '(!) Circular dependency found with: ',target%output_file
        stop
    else
        target%touched = .true.
    end if

    if (.not.allocated(target%digest_cached) .and. &
         exists(target%output_file) .and. &
         exists(target%output_file//'.digest')) then

        allocate(target%digest_cached)
        open(newunit=fh,file=target%output_file//'.digest',status='old')
        read(fh,*,iostat=stat) target%digest_cached
        close(fh)

        if (stat /= 0) then
            write(*,*) 'Internal error: unable to read cached source hash'
            write(*,*) target%output_file//'.digest',' stat = ', stat
            error stop
        end if

    end if

    if (allocated(target%source)) then
        if (allocated(target%digest_cached)) then
            if (target%digest_cached == target%source%digest) target%skip = .true.
        end if
    elseif (exists(target%output_file)) then
        target%skip = .true.
    end if

    target%link_objects = " "
    target%region = 1
    do i=1,size(target%dependencies)

        call schedule_target(target%dependencies(i)%ptr)

        if (.not.target%dependencies(i)%ptr%skip) then

            target%skip = .false.
            target%region = max(target%region,target%dependencies(i)%ptr%region+1)

        end if

        if (target%target_type == FPM_TARGET_ARCHIVE ) then

            ! Construct object list for archive
            target%link_objects = target%link_objects//" "//target%dependencies(i)%ptr%output_file

        else if (target%target_type == FPM_TARGET_EXECUTABLE .and. &
                 target%dependencies(i)%ptr%target_type ==  FPM_TARGET_OBJECT) then

            exe_obj => target%dependencies(i)%ptr
                
            ! Construct object list for executable
            target%link_objects = " "//exe_obj%output_file
                
            ! Include non-library object dependencies
            do j=1,size(exe_obj%dependencies)

                if (allocated(exe_obj%dependencies(j)%ptr%source)) then
                    if (exe_obj%dependencies(j)%ptr%source%unit_scope == exe_obj%source%unit_scope) then
                        target%link_objects = target%link_objects//" "//exe_obj%dependencies(j)%ptr%output_file
                    end if
                end if

            end do

        end if

    end do
    
    target%scheduled = .not.target%skip

end subroutine schedule_target


subroutine get_build_queue(queue, region_ptr, targets)
    type(build_target_ptr), allocatable, intent(out) :: queue(:)
    integer, allocatable :: region_ptr(:)
    type(build_target_ptr), intent(in) :: targets(:)

    integer :: i, j
    integer :: nRegion, n_scheduled

    nRegion = 0
    n_scheduled = 0
    do i=1,size(targets)

        if (targets(i)%ptr%scheduled) then
            n_scheduled = n_scheduled + 1
        end if
        nRegion = max(nRegion, targets(i)%ptr%region)

    end do

    allocate(queue(n_scheduled))
    allocate(region_ptr(nRegion+1))

    n_scheduled = 1
    region_ptr(n_scheduled) = 1
    do i=1,nRegion

        do j=1,size(targets)

            if (targets(j)%ptr%scheduled) then
                if (targets(j)%ptr%region == i) then

                    queue(n_scheduled)%ptr => targets(j)%ptr
                    n_scheduled = n_scheduled + 1
                end if
            end if

        end do

        region_ptr(i+1) = n_scheduled

    end do

end subroutine get_build_queue


subroutine build_target(model,target)
    type(fpm_model_t), intent(in) :: model
    type(build_target_t), intent(in), target :: target

    integer :: ilib, fh
    character(:), allocatable :: link_flags

    select case(target%target_type)

    case (FPM_TARGET_OBJECT)
        call run("gfortran -c " // target%source%file_name // model%fortran_compile_flags &
              // " -o " // target%output_file)

    case (FPM_TARGET_EXECUTABLE)
        if (allocated(model%library_file)) then
            link_flags = " "//model%library_file//" "//model%link_flags
        else
            link_flags = " "//model%link_flags
        end if
        
        if (allocated(target%link_libraries)) then
            do ilib = 1, size(target%link_libraries)
                link_flags = link_flags // " -l" // target%link_libraries(ilib)%s
            end do
        end if

        call run("gfortran " // target%link_objects // model%fortran_compile_flags &
              //link_flags// " -o " // target%output_file)

    case (FPM_TARGET_ARCHIVE)
        call run("ar -rs " // target%output_file // target%link_objects)

    end select

    if (allocated(target%source)) then
        open(newunit=fh,file=target%output_file//'.digest',status='unknown')
        write(fh,*) target%source%digest
        close(fh)
    end if

end subroutine build_target

end module fpm_backend
