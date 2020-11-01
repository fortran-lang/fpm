module fpm_targets
use fpm_error, only: error_t, fatal_error
use fpm_model!, only: srcfile_t, build_target_t, FPM_UNIT_PROGRAM, &
                    ! FPM_TARGET_EXECUTABLE, FPM_TARGET_ARCHIVE, FPM_TARGET_OBJECT
use fpm_environment, only: get_os_type, OS_WINDOWS
use fpm_filesystem, only: dirname, join_path
use fpm_strings, only: operator(.in.)
implicit none

contains

subroutine targets_from_sources(targets,sources,package_name)
    type(build_target_ptr), allocatable, intent(out), target :: targets(:)
    type(srcfile_t), intent(in) :: sources(:)
    character(*), intent(in) :: package_name

    integer :: i
    type(build_target_t), pointer :: dep
    logical :: with_lib

    with_lib = any([(sources(i)%unit_scope == FPM_SCOPE_LIB,i=1,size(sources))])

    if (with_lib) call add_target(targets,type = FPM_TARGET_ARCHIVE,&
                            output_file = package_name//'.a')

    do i=1,size(sources)
        
        select case (sources(i)%unit_type)
        case (FPM_UNIT_MODULE,FPM_UNIT_SUBMODULE,FPM_UNIT_SUBPROGRAM,FPM_UNIT_CSOURCE)

            call add_target(targets,source = sources(i), &
                        type = FPM_TARGET_OBJECT,&
                        output_file = get_object_name(sources(i)%file_name))
            
            if (with_lib .and. sources(i)%unit_scope == FPM_SCOPE_LIB) then
                ! Archive depends on object
                call add_dependency(targets(1)%ptr, targets(size(targets))%ptr)
            end if

        case (FPM_UNIT_PROGRAM)

            call add_target(targets,type = FPM_TARGET_OBJECT,&
                        output_file = get_object_name(sources(i)%file_name), &
                        source = sources(i) &
                        )

            call add_target(targets,type = FPM_TARGET_EXECUTABLE,&
                        output_file = join_path('app',sources(i)%exe_name))


            ! Executable depends on object
            call add_dependency(targets(size(targets))%ptr, targets(size(targets)-1)%ptr)

            if (with_lib) then
                ! Executable depends on library
                call add_dependency(targets(size(targets))%ptr, targets(1)%ptr)
            end if
            
        end select

    end do

end subroutine targets_from_sources


subroutine add_target(targets,type,output_file,source)
    type(build_target_ptr), allocatable, intent(inout) :: targets(:)
    integer, intent(in) :: type
    character(*), intent(in) :: output_file
    type(srcfile_t), intent(in), optional :: source

    type(build_target_ptr), allocatable :: temp(:)
    type(build_target_t), pointer :: new_target

    allocate(new_target)
    new_target%target_type = type
    new_target%output_file = output_file
    if (present(source)) new_target%source = source
    allocate(new_target%dependencies(0))

    if (.not.allocated(targets)) allocate(targets(0))
    targets = [targets, build_target_ptr(new_target)]

end subroutine add_target


subroutine add_dependency(target, dependency)
    type(build_target_t), intent(inout) :: target
    type(build_target_t) , intent(in), target :: dependency

    type(build_target_ptr) :: depend

    depend%ptr => dependency

    ! if (.not.allocated(target%dependencies)) then
    !     allocate(target%dependencies(0))
    ! end if

    target%dependencies = [target%dependencies, depend]
    ! target%dependencies(size(target%dependencies))%ptr => dependency

end subroutine add_dependency


function get_object_name(source_file_name) result(object_file)
    ! Generate object target path from source name and model params
    !  
    !  src/test.f90        ->  <output-dir>/<package-name>/test.o
    !  src/subdir/test.f90 ->  <output-dir>/<package-name>/subdir_test.o
    !
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
    object_file = source_file_name(index(source_file_name,filesep)+1:)//'.o'

end function get_object_name


subroutine resolve_module_dependencies(targets,error)
    ! After enumerating all source files: resolve file dependencies
    !  by searching on module names
    !
    type(build_target_ptr), intent(inout), target :: targets(:)
    type(error_t), allocatable, intent(out) :: error

    type(build_target_ptr) :: dep

    integer :: i, j

    do i=1,size(targets)
        
        if (.not.allocated(targets(i)%ptr%source)) cycle

            do j=1,size(targets(i)%ptr%source%modules_used)

                if (targets(i)%ptr%source%modules_used(j)%s .in. targets(i)%ptr%source%modules_provided) then
                    ! Dependency satisfied in same file, skip
                    cycle
                end if
            
                if (targets(i)%ptr%source%unit_scope == FPM_SCOPE_APP .OR. &
                    targets(i)%ptr%source%unit_scope == FPM_SCOPE_TEST ) then
                    dep%ptr => &
                        find_module_dependency(targets,targets(i)%ptr%source%modules_used(j)%s, &
                                            include_dir = dirname(targets(i)%ptr%source%file_name))
                else
                    dep%ptr => &
                        find_module_dependency(targets,targets(i)%ptr%source%modules_used(j)%s)
                end if

                if (.not.associated(dep%ptr)) then
                    call fatal_error(error, &
                            'Unable to find source for module dependency: "' // &
                            targets(i)%ptr%source%modules_used(j)%s // &
                            '" used by "'//targets(i)%ptr%source%file_name//'"')
                    return
                end if

                call add_dependency(targets(i)%ptr, dep%ptr)

            end do

    end do    

end subroutine resolve_module_dependencies

function find_module_dependency(targets,module_name,include_dir) result(target_ptr)
    ! Find a module dependency in the library or a dependency library
    !
    ! 'include_dir' specifies an allowable non-library search directory
    !   (Used for executable dependencies)
    !
    type(build_target_ptr), intent(in), target :: targets(:)
    character(*), intent(in) :: module_name
    character(*), intent(in), optional :: include_dir
    type(build_target_t), pointer :: target_ptr

    integer :: k, l

    target_ptr => NULL()

    do k=1,size(targets)

        if (.not.allocated(targets(k)%ptr%source)) cycle

        do l=1,size(targets(k)%ptr%source%modules_provided)

            if (module_name == targets(k)%ptr%source%modules_provided(l)%s) then
                select case(targets(k)%ptr%source%unit_scope)
                case (FPM_SCOPE_LIB, FPM_SCOPE_DEP)
                    target_ptr => targets(k)%ptr
                    exit
                case default
                    if (present(include_dir)) then
                        if (dirname(targets(k)%ptr%source%file_name) == include_dir) then
                            target_ptr => targets(k)%ptr
                            exit
                        end if
                    end if
                end select
            end if

        end do
        
    end do

end function find_module_dependency

end module fpm_targets