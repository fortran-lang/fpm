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
public :: fpm_model_t, srcfile_t, show_model

public :: FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
          FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
          FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
          FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST

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


!> Type describing everything required to build
!>  the root package and its dependencies.
type :: fpm_model_t

    !> Name of root package
    character(:), allocatable :: package_name

    !> Array of packages (including the root package)
    type(package_t), allocatable :: packages(:)

    !> Command line name to invoke fortran compiler
    character(:), allocatable :: fortran_compiler

    !> Command line flags passed to fortran for compilation
    character(:), allocatable :: fortran_compile_flags

    !> Base directory for build
    character(:), allocatable :: output_directory

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)
    
    !> Project dependencies
    type(dependency_tree_t) :: deps

end type fpm_model_t

contains


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
end function info_srcfile

function info_srcfile_short(source) result(s)
    ! Prints a shortened version of srcfile_t
    type(srcfile_t), intent(in) :: source
    character(:), allocatable :: s
    integer :: i
    s = "srcfile_t("
    s = s // 'file_name="' // source%file_name // '"'
    s = s // ", ...)"
end function info_srcfile_short

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
    !    character(:), allocatable :: fortran_compiler
    s = s // ', fortran_compiler="' // model%fortran_compiler // '"'
    !    character(:), allocatable :: fortran_compile_flags
    s = s // ', fortran_compile_flags="' // model%fortran_compile_flags // '"'
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
end function info_model

subroutine show_model(model)
    ! Prints a human readable representation of the Model
    type(fpm_model_t), intent(in) :: model
    print *, info_model(model)
end subroutine show_model

end module fpm_model
