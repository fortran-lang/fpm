!># Build target handling
!>
!> This module handles the construction of the build target list
!> from the sources list (`[[targets_from_sources]]`), the 
!> resolution of module-dependencies between build targets
!> (`[[resolve_module_dependencies]]`), and the enumeration of
!> objects required for link targets (`[[resolve_target_linking]]`).
!>
!> A build target (`[[build_target_t]]`) is a file to be generated
!> by the backend (compilation and linking).
!> 
!> @note The current implementation is ignorant to the existence of
!> module files (`.mod`,`.smod`). Dependencies arising from modules
!> are based on the corresponding object files (`.o`) only.
!>
!> For more information, please read the documentation for the procedures:
!>
!> - `[[targets_from_sources]]`
!> - `[[resolve_module_dependencies]]`
!>
module fpm_targets
use fpm_error, only: error_t, fatal_error
use fpm_model
use fpm_environment, only: get_os_type, OS_WINDOWS
use fpm_filesystem, only: dirname, join_path, canon_path
use fpm_strings, only: string_t, operator(.in.)
implicit none

private
public targets_from_sources, resolve_module_dependencies
public resolve_target_linking, add_target, add_dependency

contains

!> Constructs a list of build targets from a list of source files
!>
!>### Source-target mapping
!>
!> One compiled object target (`FPM_TARGET_OBJECT`) is generated for each
!> non-executable source file (`FPM_UNIT_MODULE`,`FPM_UNIT_SUBMODULE`,
!>  `FPM_UNIT_SUBPROGRAM`,`FPM_UNIT_CSOURCE`).
!>
!> If any source file has scope `FPM_SCOPE_LIB` (*i.e.* there are library sources)
!> then the first target in the target list will be a library archive target
!> (`FPM_TARGET_ARCHIVE`). The archive target will have a dependency on every
!> compiled object target corresponding to a library source file.
!>
!> One compiled object target (`FPM_TARGET_OBJECT`) and one executable target (`FPM_TARGET_EXECUTABLE`) is
!> generated for each exectuable source file (`FPM_UNIT_PROGRAM`). The exectuble target
!> always has a dependency on the corresponding compiled object target. If there
!> is a library, then the executable target has an additional dependency on the library
!> archive target.
!>
!> @note Inter-object dependencies based on modules used and provided are generated separately
!> in `[[resolve_module_dependencies]]` after all targets have been enumerated.
subroutine targets_from_sources(model,sources)

    !> The package model within which to construct the target list
    type(fpm_model_t), intent(inout), target :: model

    !> The list of sources from which to construct the target list
    type(srcfile_t), intent(in) :: sources(:)

    integer :: i
    character(:), allocatable :: xsuffix, exe_dir
    type(build_target_t), pointer :: dep
    logical :: with_lib

    if (get_os_type() == OS_WINDOWS) then
        xsuffix = '.exe'
    else
        xsuffix = ''
    end if

    with_lib = any([(sources(i)%unit_scope == FPM_SCOPE_LIB,i=1,size(sources))])

    if (with_lib) call add_target(model%targets,type = FPM_TARGET_ARCHIVE,&
                            output_file = join_path(model%output_directory,&
                                   model%package_name,'lib'//model%package_name//'.a'))

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

                exe_dir = 'app'

            else if (sources(i)%unit_scope == FPM_SCOPE_EXAMPLE) then

                exe_dir = 'example'

            else

                exe_dir = 'test'

            end if

            call add_target(model%targets,type = FPM_TARGET_EXECUTABLE,&
                            link_libraries = sources(i)%link_libraries, &
                            output_file = join_path(model%output_directory,exe_dir, &
                            sources(i)%exe_name//xsuffix))

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

        ! Convert any remaining directory separators to underscores
        i = index(object_file,filesep)
        do while(i > 0)
            object_file(i:i) = '_'
            i = index(object_file,filesep)
        end do

        object_file = join_path(model%output_directory,model%package_name,object_file)//'.o'
    
    end function get_object_name

end subroutine targets_from_sources


!> Allocate a new target and append to target list
subroutine add_target(targets,type,output_file,source,link_libraries)
    type(build_target_ptr), allocatable, intent(inout) :: targets(:)
    integer, intent(in) :: type
    character(*), intent(in) :: output_file
    type(srcfile_t), intent(in), optional :: source
    type(string_t), intent(in), optional :: link_libraries(:)

    integer :: i
    type(build_target_ptr), allocatable :: temp(:)
    type(build_target_t), pointer :: new_target

    if (.not.allocated(targets)) allocate(targets(0))

    ! Check for duplicate outputs
    do i=1,size(targets)

        if (targets(i)%ptr%output_file == output_file) then

            write(*,*) 'Error while building target list: duplicate output object "',&
                        output_file,'"'
            if (present(source)) write(*,*) ' Source file: "',source%file_name,'"'
            stop 1

        end if

    end do

    allocate(new_target)
    new_target%target_type = type
    new_target%output_file = output_file
    if (present(source)) new_target%source = source
    if (present(link_libraries)) new_target%link_libraries = link_libraries
    allocate(new_target%dependencies(0))
    
    targets = [targets, build_target_ptr(new_target)]

end subroutine add_target


!> Add pointer to dependeny in target%dependencies
subroutine add_dependency(target, dependency)
    type(build_target_t), intent(inout) :: target
    type(build_target_t) , intent(in), target :: dependency

    target%dependencies = [target%dependencies, build_target_ptr(dependency)]

end subroutine add_dependency


!> Add dependencies to source-based targets (`FPM_TARGET_OBJECT`) 
!> based on any modules used by the corresponding source file.
!>
!>### Source file scoping
!> 
!> Source files are assigned a scope of either `FPM_SCOPE_LIB`, 
!> `FPM_SCOPE_APP` or `FPM_SCOPE_TEST`. The scope controls which
!> modules may be used by the source file:
!> 
!> - Library sources (`FPM_SCOPE_LIB`) may only use modules
!>   also with library scope. This includes library modules
!>   from dependencies.
!>
!> - Executable sources (`FPM_SCOPE_APP`,`FPM_SCOPE_TEST`) may use
!>   library modules (including dependencies) as well as any modules
!>   corresponding to source files __in the same directory__ as the 
!>   executable source.
!>
!> @warning If a module used by a source file cannot be resolved to
!> a source file in the package of the correct scope, then a __fatal error__
!> is returned by the procedure and model construction fails.
!>
subroutine resolve_module_dependencies(targets,error)
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
            
                if (any(targets(i)%ptr%source%unit_scope == &
                    [FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST])) then
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


!> For libraries and executables, build a list of objects required for linking
!>
!> stored in `target%link_objects`
!>
subroutine resolve_target_linking(targets)
    type(build_target_ptr), intent(inout), target :: targets(:)

    integer :: i

    do i=1,size(targets)

        associate(target => targets(i)%ptr)

            allocate(target%link_objects(0))

            if (target%target_type == FPM_TARGET_ARCHIVE) then

                call get_link_objects(target%link_objects,target,is_exe=.false.)

            else if (target%target_type == FPM_TARGET_EXECUTABLE) then

                call get_link_objects(target%link_objects,target,is_exe=.true.)

            end if

        end associate

    end do

contains

    !> Wrapper to build link object list
    !>
    !>  For libraries: just list dependency objects of lib target
    !>
    !>  For executables: need to recursively discover non-library
    !>   dependency objects. (i.e. modules in same dir as program)
    !>
    recursive subroutine get_link_objects(link_objects,target,is_exe)
        type(string_t), intent(inout), allocatable :: link_objects(:)
        type(build_target_t), intent(in) :: target
        logical, intent(in) :: is_exe

        integer :: i
        type(string_t) :: temp_str

        if (.not.allocated(target%dependencies)) return

        do i=1,size(target%dependencies)

            associate(dep => target%dependencies(i)%ptr)
            
                if (.not.allocated(dep%source)) cycle
                
                ! Skip library dependencies for executable targets
                !  since the library archive will always be linked 
                if (is_exe.and.(dep%source%unit_scope == FPM_SCOPE_LIB)) cycle
                
                ! Skip if dependency object already listed
                if (dep%output_file .in. link_objects) cycle

                ! Add dependency object file to link object list
                temp_str%s = dep%output_file
                link_objects = [link_objects, temp_str]

                ! For executable objects, also need to include non-library 
                !  dependencies from dependencies (recurse)
                if (is_exe) call get_link_objects(link_objects,dep,is_exe=.true.)
            
            end associate

        end do

    end subroutine get_link_objects

end subroutine resolve_target_linking


end module fpm_targets
