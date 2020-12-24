!># The fpm package model
!>
!> Defines the fpm model data types which encapsulate all information 
!> required to correctly build a package and its dependencies.
!>
!> The process (see `[[build_model(subroutine)]]`) for generating a valid `[[fpm_model]]` is as follows:
!>
!> 1. Source files are discovered ([[fpm_sources]]) and parsed ([[fpm_source_parsing]])
!> 2. A list of build targets is generated (`[[targets_from_sources]]`) from the sources
!> 3. Inter-target dependencies are resolved (`[[resolve_module_dependencies]]`) based on modules used and provided
!> 4. Object link lists are generated for link targets (executables and libraries) (`[[resolve_target_linking]]`)
!>
!> Once a valid `[[fpm_model]]` has been constructed, it may be passed to `[[fpm_backend:build_package]]` to
!> build the package.
!>
!>### Enumerations
!>
!> __Source type:__ `FPM_UNIT_*`
!> Describes the type of source file — determines build target generation
!>
!> __Source scope:__ `FPM_SCOPE_*`
!> Describes the scoping rules for using modules — controls module dependency resolution
!>
!> __Target type:__ `FPM_TARGET_*`
!> Describes the type of build target — determines backend build rules
!>
module fpm_model
use iso_fortran_env, only: int64
use fpm_strings, only: string_t, str
use fpm_dependency, only: dependency_tree_t
implicit none

private
public :: fpm_model_t, srcfile_t, build_target_t, build_target_ptr, &
    show_model

public :: FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
          FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
          FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
          FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST, &
          FPM_TARGET_UNKNOWN, FPM_TARGET_EXECUTABLE, FPM_TARGET_ARCHIVE, &
          FPM_TARGET_OBJECT

!> Source type unknown
integer, parameter :: FPM_UNIT_UNKNOWN = -1
!> Source type is fortran program
integer, parameter :: FPM_UNIT_PROGRAM = 1
!> Source type is fortran module
integer, parameter :: FPM_UNIT_MODULE = 2
!> Source type is fortran submodule
integer, parameter :: FPM_UNIT_SUBMODULE = 3
!> Source type is fortran subprogram
integer, parameter :: FPM_UNIT_SUBPROGRAM = 4
!> Source type is c source file
integer, parameter :: FPM_UNIT_CSOURCE = 5
!> Source type is c header file
integer, parameter :: FPM_UNIT_CHEADER = 6


!> Source has no module-use scope
integer, parameter :: FPM_SCOPE_UNKNOWN = -1
!> Module-use scope is library/dependency modules only
integer, parameter :: FPM_SCOPE_LIB = 1
!> Module-use scope is library/dependency modules only
integer, parameter :: FPM_SCOPE_DEP = 2
!> Module-use scope is library/dependency and app modules
integer, parameter :: FPM_SCOPE_APP = 3
!> Module-use scope is library/dependency and test modules
integer, parameter :: FPM_SCOPE_TEST = 4
integer, parameter :: FPM_SCOPE_EXAMPLE = 5


!> Target type is unknown (ignored)
integer, parameter :: FPM_TARGET_UNKNOWN = -1
!> Target type is executable
integer, parameter :: FPM_TARGET_EXECUTABLE = 1
!> Target type is library archive
integer, parameter :: FPM_TARGET_ARCHIVE = 2
!> Target type is compiled object
integer, parameter :: FPM_TARGET_OBJECT = 3


!> Type for describing a source file
type srcfile_t
    !> File path relative to cwd
    character(:), allocatable :: file_name

    !> Name of executable for FPM_UNIT_PROGRAM
    character(:), allocatable :: exe_name

    !> Target module-use scope
    integer :: unit_scope = FPM_SCOPE_UNKNOWN

    !> Modules provided by this source file (lowerstring)
    type(string_t), allocatable :: modules_provided(:)

    !> Type of source unit
    integer :: unit_type = FPM_UNIT_UNKNOWN

    !>  Modules USEd by this source file (lowerstring)
    type(string_t), allocatable :: modules_used(:)

    !> Files INCLUDEd by this source file
    type(string_t), allocatable :: include_dependencies(:)

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)

    !> Current hash
    integer(int64) :: digest

end type srcfile_t


!> Type for describing a single package
type package_t

    !> Name of package
    character(:), allocatable :: name

    !> Array of sources
    type(srcfile_t), allocatable :: sources(:)

end type package_t


!> Wrapper type for constructing arrays of `[[build_target_t]]` pointers
type build_target_ptr

    type(build_target_t), pointer :: ptr => null()

end type build_target_ptr


!> Type describing a generated build target
type build_target_t

    !> File path of build target object relative to cwd
    character(:), allocatable :: output_file

    !> Primary source for this build target
    type(srcfile_t), allocatable :: source

    !> Resolved build dependencies
    type(build_target_ptr), allocatable :: dependencies(:)

    !> Target type
    integer :: target_type = FPM_TARGET_UNKNOWN

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)

    !> Objects needed to link this target
    type(string_t), allocatable :: link_objects(:)
    
    !> Flag set when first visited to check for circular dependencies
    logical :: touched = .false.
    
    !> Flag set if build target is sorted for building
    logical :: sorted = .false.

    !> Flag set if build target will be skipped (not built)
    logical :: skip = .false.

    !> Targets in the same schedule group are guaranteed to be independent
    integer :: schedule = -1

    !> Previous source file hash
    integer(int64), allocatable :: digest_cached

end type build_target_t


!> Type describing everything required to build
!>  the root package and its dependencies.
type :: fpm_model_t

    !> Name of root package
    character(:), allocatable :: package_name

    !> Array of packages (including the root package)
    type(package_t), allocatable :: packages(:)

    !> Array of targets with module-dependencies resolved
    type(build_target_ptr), allocatable :: targets(:)

    !> Command line name to invoke fortran compiler
    character(:), allocatable :: fortran_compiler

    !> Command line flags passed to fortran for compilation
    character(:), allocatable :: fortran_compile_flags

    !> Command line flags pass for linking
    character(:), allocatable :: link_flags

    !> Output file for library archive
    character(:), allocatable :: library_file

    !> Base directory for build
    character(:), allocatable :: output_directory

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)
    
    !> Project dependencies
    type(dependency_tree_t) :: deps

end type fpm_model_t

contains

function info_build_target(t) result(s)
    type(build_target_t), intent(in) :: t
    character(:), allocatable :: s
    integer :: i
    !type build_target_t
    s = "build_target_t("
    !    character(:), allocatable :: output_file
    s = s // 'output_file="' // t%output_file // '"'
    !    type(srcfile_t), allocatable :: source
    if (allocated(t%source)) then
        s = s // ", source=" // info_srcfile_short(t%source)
    else
        s = s // ", source=()"
    end if
    !    type(build_target_ptr), allocatable :: dependencies(:)
    s = s // ", dependencies=["
    if (allocated(t%dependencies)) then
        do i = 1, size(t%dependencies)
            s = s // info_build_target_short(t%dependencies(i)%ptr)
            if (i < size(t%dependencies)) s = s // ", "
        end do
    end if
    s = s // "]"
    !    integer :: target_type = FPM_TARGET_UNKNOWN
    s = s // ", target_type="
    select case(t%target_type)
    case (FPM_TARGET_UNKNOWN)
        s = s // "FPM_TARGET_UNKNOWN"
    case (FPM_TARGET_EXECUTABLE)
        s = s // "FPM_TARGET_EXECUTABLE"
    case (FPM_TARGET_ARCHIVE)
        s = s // "FPM_TARGET_ARCHIVE"
    case (FPM_TARGET_OBJECT)
        s = s // "FPM_TARGET_OBJECT"
    case default
        s = s // "INVALID"
    end select
    !    type(string_t), allocatable :: link_libraries(:)
    s = s // ", link_libraries=["
    if (allocated(t%link_libraries)) then
        do i = 1, size(t%link_libraries)
            s = s // '"' // t%link_libraries(i)%s // '"'
            if (i < size(t%link_libraries)) s = s // ", "
        end do
    end if
    s = s // "]"
    !    type(string_t), allocatable :: link_objects(:)
    s = s // ", link_objects=["
    if (allocated(t%link_objects)) then
        do i = 1, size(t%link_objects)
            s = s // '"' // t%link_objects(i)%s // '"'
            if (i < size(t%link_objects)) s = s // ", "
        end do
    end if
    s = s // "]"
    !    logical :: touched = .false.
    s = s // ", touched=" // str(t%touched)
    !    logical :: sorted = .false.
    s = s // ", sorted=" // str(t%sorted)
    !    logical :: skip = .false.
    s = s // ", skip=" // str(t%skip)
    !    integer :: schedule = -1
    s = s // ", schedule=" // str(t%schedule)
    !    integer(int64), allocatable :: digest_cached
    if (allocated(t%digest_cached)) then
        s = s // ", digest_cached=" // str(t%digest_cached)
    else
        s = s // ", digest_cached=()"
    end if
    !end type build_target_t
    s = s // ")"
end function

function info_build_target_short(t) result(s)
    ! Prints a shortened representation of build_target_t
    type(build_target_t), intent(in) :: t
    character(:), allocatable :: s
    integer :: i
    s = "build_target_t("
    s = s // 'output_file="' // t%output_file // '"'
    s = s // ", ...)"
end function

function info_package(p) result(s)
    ! Returns representation of package_t
    type(package_t), intent(in) :: p
    character(:), allocatable :: s

    integer :: i

    s = s // 'package_t('
    s = s // 'name="' // p%name //'"'
    s = s // ', sources=['
    do i = 1, size(p%sources)
        s = s // info_srcfile(p%sources(i))
        if (i < size(p%sources)) s = s // ", "
    end do
    s = s // "]"
    s = s // ")"

end function info_package

function info_srcfile(source) result(s)
    type(srcfile_t), intent(in) :: source
    character(:), allocatable :: s
    integer :: i
    !type srcfile_t
    s = "srcfile_t("
    !    character(:), allocatable :: file_name
    s = s // 'file_name="' // source%file_name // '"'
    !    character(:), allocatable :: exe_name
    s = s // ', exe_name="' // source%exe_name // '"'
    !    integer :: unit_scope = FPM_SCOPE_UNKNOWN
    s = s // ", unit_scope="
    select case(source%unit_scope)
    case (FPM_SCOPE_UNKNOWN)
        s = s // "FPM_SCOPE_UNKNOWN"
    case (FPM_SCOPE_LIB)
        s = s // "FPM_SCOPE_LIB"
    case (FPM_SCOPE_DEP)
        s = s // "FPM_SCOPE_DEP"
    case (FPM_SCOPE_APP)
        s = s // "FPM_SCOPE_APP"
    case (FPM_SCOPE_TEST)
        s = s // "FPM_SCOPE_TEST"
    case (FPM_SCOPE_EXAMPLE)
        s = s // "FPM_SCOPE_EXAMPLE"
    case default
        s = s // "INVALID"
    end select
    !    type(string_t), allocatable :: modules_provided(:)
    s = s // ", modules_provided=["
    do i = 1, size(source%modules_provided)
        s = s // '"' // source%modules_provided(i)%s // '"'
        if (i < size(source%modules_provided)) s = s // ", "
    end do
    s = s // "]"
    !    integer :: unit_type = FPM_UNIT_UNKNOWN
    s = s // ", unit_type="
    select case(source%unit_type)
    case (FPM_UNIT_UNKNOWN)
        s = s // "FPM_UNIT_UNKNOWN"
    case (FPM_UNIT_PROGRAM)
        s = s // "FPM_UNIT_PROGRAM"
    case (FPM_UNIT_MODULE)
        s = s // "FPM_UNIT_MODULE"
    case (FPM_UNIT_SUBMODULE)
        s = s // "FPM_UNIT_SUBMODULE"
    case (FPM_UNIT_SUBPROGRAM)
        s = s // "FPM_UNIT_SUBPROGRAM"
    case (FPM_UNIT_CSOURCE)
        s = s // "FPM_UNIT_CSOURCE"
    case (FPM_UNIT_CHEADER)
        s = s // "FPM_UNIT_CHEADER"
    case default
        s = s // "INVALID"
    end select
    !    type(string_t), allocatable :: modules_used(:)
    s = s // ", modules_used=["
    do i = 1, size(source%modules_used)
        s = s // '"' // source%modules_used(i)%s // '"'
        if (i < size(source%modules_used)) s = s // ", "
    end do
    s = s // "]"
    !    type(string_t), allocatable :: include_dependencies(:)
    s = s // ", include_dependencies=["
    do i = 1, size(source%include_dependencies)
        s = s // '"' // source%include_dependencies(i)%s // '"'
        if (i < size(source%include_dependencies)) s = s // ", "
    end do
    s = s // "]"
    !    type(string_t), allocatable :: link_libraries(:)
    s = s // ", link_libraries=["
    do i = 1, size(source%link_libraries)
        s = s // '"' // source%link_libraries(i)%s // '"'
        if (i < size(source%link_libraries)) s = s // ", "
    end do
    s = s // "]"
    !    integer(int64) :: digest
    s = s // ", digest=" // str(source%digest)
    !end type srcfile_t
    s = s // ")"
end function

function info_srcfile_short(source) result(s)
    ! Prints a shortened version of srcfile_t
    type(srcfile_t), intent(in) :: source
    character(:), allocatable :: s
    integer :: i
    s = "srcfile_t("
    s = s // 'file_name="' // source%file_name // '"'
    s = s // ", ...)"
end function

function info_model(model) result(s)
    type(fpm_model_t), intent(in) :: model
    character(:), allocatable :: s
    integer :: i
    !type :: fpm_model_t
    s = "fpm_model_t("
    !    character(:), allocatable :: package_name
    s = s // 'package_name="' // model%package_name // '"'
    !    type(srcfile_t), allocatable :: sources(:)
    s = s // ", packages=["
    do i = 1, size(model%packages)
        s = s // info_package(model%packages(i))
        if (i < size(model%packages)) s = s // ", "
    end do
    s = s // "]"
    !    type(build_target_ptr), allocatable :: targets(:)
    s = s // ", targets=["
    do i = 1, size(model%targets)
        s = s // info_build_target(model%targets(i)%ptr)
        if (i < size(model%targets)) s = s // ", "
    end do
    s = s // "]"
    !    character(:), allocatable :: fortran_compiler
    s = s // ', fortran_compiler="' // model%fortran_compiler // '"'
    !    character(:), allocatable :: fortran_compile_flags
    s = s // ', fortran_compile_flags="' // model%fortran_compile_flags // '"'
    !    character(:), allocatable :: link_flags
    s = s // ', link_flags="' // model%link_flags // '"'
    !    character(:), allocatable :: library_file
    s = s // ', library_file="' // model%library_file // '"'
    !    character(:), allocatable :: output_directory
    s = s // ', output_directory="' // model%output_directory // '"'
    !    type(string_t), allocatable :: link_libraries(:)
    s = s // ", link_libraries=["
    do i = 1, size(model%link_libraries)
        s = s // '"' // model%link_libraries(i)%s // '"'
        if (i < size(model%link_libraries)) s = s // ", "
    end do
    s = s // "]"
    !    type(dependency_tree_t) :: deps
    ! TODO: print `dependency_tree_t` properly, which should become part of the
    !       model, not imported from another file
    s = s // ", deps=dependency_tree_t(...)"
    !end type fpm_model_t
    s = s // ")"
end function

subroutine show_model(model)
    ! Prints a human readable representation of the Model
    type(fpm_model_t), intent(in) :: model
    print *, info_model(model)
end subroutine

end module fpm_model
