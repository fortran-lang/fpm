module fpm_targets
use fpm_error, only: error_t, fatal_error
use fpm_model
use fpm_environment, only: get_os_type, OS_WINDOWS
use fpm_filesystem, only: dirname, join_path, canon_path
use fpm_strings, only: operator(.in.)
implicit none

contains

subroutine targets_from_sources(model,sources)
    type(fpm_model_t), intent(inout), target :: model
    type(srcfile_t), intent(in) :: sources(:)

    integer :: i
    type(build_target_t), pointer :: dep
    logical :: with_lib

    with_lib = any([(sources(i)%unit_scope == FPM_SCOPE_LIB,i=1,size(sources))])

    if (with_lib) call add_target(model%targets,type = FPM_TARGET_ARCHIVE,&
                            output_file = join_path(model%output_directory,&
                                      'lib','lib'//model%package_name//'.a'))

    do i=1,size(sources)
        
        select case (sources(i)%unit_type)
        case (FPM_UNIT_MODULE,FPM_UNIT_SUBMODULE,FPM_UNIT_SUBPROGRAM,FPM_UNIT_CSOURCE)

            call add_target(model%targets,source = sources(i), &
                        type = FPM_TARGET_OBJECT,&
                        output_file = get_object_name(sources(i)))
            
            if (with_lib .and. sources(i)%unit_scope == FPM_SCOPE_LIB) then
                ! Archive depends on object
                call add_dependency(model%targets(1)%ptr, model%targets(size(model%targets))%ptr)
            end if

        case (FPM_UNIT_PROGRAM)

            call add_target(model%targets,type = FPM_TARGET_OBJECT,&
                        output_file = get_object_name(sources(i)), &
                        source = sources(i) &
                        )
            
            if (sources(i)%unit_scope == FPM_SCOPE_APP) then
                call add_target(model%targets,type = FPM_TARGET_EXECUTABLE,&
                            output_file = join_path(model%output_directory,'app',sources(i)%exe_name))
            else
                call add_target(model%targets,type = FPM_TARGET_EXECUTABLE,&
                            output_file = join_path(model%output_directory,'test',sources(i)%exe_name))
            
            end if

            ! Executable depends on object
            call add_dependency(model%targets(size(model%targets))%ptr, model%targets(size(model%targets)-1)%ptr)

            if (with_lib) then
                ! Executable depends on library
                call add_dependency(model%targets(size(model%targets))%ptr, model%targets(1)%ptr)
            end if
            
        end select

    end do

    contains

    function get_object_name(source) result(object_file)
        ! Generate object target path from source name and model params
        !  
        !
        type(srcfile_t), intent(in) :: source
        character(:), allocatable :: object_file
    
        integer :: i
        character(1), parameter :: filesep = '/'
        character(:), allocatable :: dir
        
        object_file = canon_path(source%file_name)

        ! Ignore first directory level
        object_file = object_file(index(object_file,filesep)+1:)
        
        ! Convert any remaining directory separators to underscores
        i = index(object_file,filesep)
        do while(i > 0)
            object_file(i:i) = '_'
            i = index(object_file,filesep)
        end do

        select case(source%unit_scope)

        case (FPM_SCOPE_APP)
            object_file = join_path(model%output_directory,'app',object_file)//'.o'

        case (FPM_SCOPE_TEST)
            object_file = join_path(model%output_directory,'test',object_file)//'.o'

        case default
            object_file = join_path(model%output_directory,'lib',object_file)//'.o'
            
        end select
    
    end function get_object_name

end subroutine targets_from_sources


!> Add new target to target list
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


!> Add pointer to dependeny in target%dependencies
subroutine add_dependency(target, dependency)
    type(build_target_t), intent(inout) :: target
    type(build_target_t) , intent(in), target :: dependency

    target%dependencies = [target%dependencies, build_target_ptr(dependency)]

end subroutine add_dependency


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