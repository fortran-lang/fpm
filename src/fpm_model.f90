!># The fpm package model
!>
!> Defines the fpm model data types which encapsulate all information
!> required to correctly build a package and its dependencies.
!>
!> The process (see `[[build_model(subroutine)]]`) for generating a valid `[[fpm_model]]` involves
!>  source files discovery ([[fpm_sources]]) and parsing ([[fpm_source_parsing]]).
!>
!> Once a valid `[[fpm_model]]` has been constructed, it may be passed to `[[fpm_targets:targets_from_sources]]` to
!> generate a list of build targets for the backend.
!>
!>### Enumerations
!>
!> __Source type:__ `FPM_UNIT_*`
!> Describes the type of source file — determines build target generation
!>
!> The logical order of precedence for assigning `unit_type` is as follows:
!>
!>```
!> if source-file contains program then
!>   unit_type = FPM_UNIT_PROGRAM
!> else if source-file contains non-module subroutine/function then
!>   unit_type = FPM_UNIT_SUBPROGRAM
!> else if source-file contains submodule then
!>   unit_type = FPM_UNIT_SUBMODULE
!> else if source-file contains module then
!>   unit_type = FPM_UNIT_MODULE
!> end if
!>```
!>
!> @note A source file is only designated `FPM_UNIT_MODULE` if it **only** contains modules - no non-module subprograms.
!> (This allows tree-shaking/pruning of build targets based on unused module dependencies.)
!>
!> __Source scope:__ `FPM_SCOPE_*`
!> Describes the scoping rules for using modules — controls module dependency resolution
!>
module fpm_model
use iso_fortran_env, only: int64
use fpm_compiler, only: compiler_t, archiver_t, debug
use fpm_dependency, only: dependency_tree_t
use fpm_strings, only: string_t, str, len_trim
use fpm_manifest_preprocess, only: preprocess_config_t
implicit none

private
public :: fpm_model_t, srcfile_t, show_model, fortran_features_t

public :: FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
          FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
          FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
          FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST, &
          FPM_UNIT_CPPSOURCE

!> Source type unknown
integer, parameter :: FPM_UNIT_UNKNOWN = -1
!> Source contains a fortran program
integer, parameter :: FPM_UNIT_PROGRAM = 1
!> Source **only** contains one or more fortran modules
integer, parameter :: FPM_UNIT_MODULE = 2
!> Source contains one or more fortran submodules
integer, parameter :: FPM_UNIT_SUBMODULE = 3
!> Source contains one or more fortran subprogram not within modules
integer, parameter :: FPM_UNIT_SUBPROGRAM = 4
!> Source type is c source file
integer, parameter :: FPM_UNIT_CSOURCE = 5
!> Source type is c header file
integer, parameter :: FPM_UNIT_CHEADER = 6
!> Souce type is c++ source file.
integer, parameter :: FPM_UNIT_CPPSOURCE = 7

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

!> Enabled Fortran language features
type :: fortran_features_t

    !> Use default implicit typing
    logical :: implicit_typing = .false.

    !> Use implicit external interface
    logical :: implicit_external = .false.

    !> Form to use for all Fortran sources
    character(:), allocatable :: source_form
end type fortran_features_t

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

    !> Parent modules (submodules only)
    type(string_t), allocatable :: parent_modules(:)

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

    !> List of macros.
    type(preprocess_config_t) :: preprocess

    !> Package version number.
    character(:), allocatable :: version

    !> Module naming conventions
    logical :: enforce_module_names

    !> Prefix for all module names
    type(string_t) :: module_prefix

    !> Language features
    type(fortran_features_t) :: features

end type package_t


!> Type describing everything required to build
!>  the root package and its dependencies.
type :: fpm_model_t

    !> Name of root package
    character(:), allocatable :: package_name

    !> Array of packages (including the root package)
    type(package_t), allocatable :: packages(:)

    !> Compiler object
    type(compiler_t) :: compiler

    !> Archiver object
    type(archiver_t) :: archiver

    !> Command line flags passed to fortran for compilation
    character(:), allocatable :: fortran_compile_flags

    !> Command line flags passed to C for compilation
    character(:), allocatable :: c_compile_flags

    !> Command line flags passed to C++ for compilation
    character(:), allocatable :: cxx_compile_flags

    !> Command line flags passed to the linker
    character(:), allocatable :: link_flags

    !> Base directory for build
    character(:), allocatable :: build_prefix

    !> Include directories
    type(string_t), allocatable :: include_dirs(:)

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)

    !> External modules used
    type(string_t), allocatable :: external_modules(:)

    !> Project dependencies
    type(dependency_tree_t) :: deps

    !> Whether tests should be added to the build list
    logical :: include_tests = .true.

    !> Whether module names should be prefixed with the package name
    logical :: enforce_module_names = .false.

    !> Prefix for all module names
    type(string_t) :: module_prefix

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

    ! Print module naming convention
    s = s // ', enforce_module_names="' // merge('T','F',p%enforce_module_names) // '"'

    ! Print custom prefix
    if (p%enforce_module_names .and. len_trim(p%module_prefix)>0) &
    s = s // ', custom_prefix="' // p%module_prefix%s // '"'

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
    s = s // ", parent_modules=["
    do i = 1, size(source%parent_modules)
        s = s // '"' // source%parent_modules(i)%s // '"'
        if (i < size(source%parent_modules)) s = s // ", "
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
    case (FPM_UNIT_CPPSOURCE)
        s = s // "FPM_UNIT_CPPSOURCE"
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
    s = s // ', compiler=(' // debug(model%compiler) // ')'
    s = s // ', archiver=(' // debug(model%archiver) // ')'
    !    character(:), allocatable :: fortran_compile_flags
    s = s // ', fortran_compile_flags="' // model%fortran_compile_flags // '"'
    s = s // ', c_compile_flags="' // model%c_compile_flags // '"'
    s = s // ', cxx_compile_flags="' // model%cxx_compile_flags // '"'
    s = s // ', link_flags="' // model%link_flags // '"'
    s = s // ', build_prefix="' // model%build_prefix // '"'
    !    type(string_t), allocatable :: link_libraries(:)
    s = s // ", link_libraries=["
    do i = 1, size(model%link_libraries)
        s = s // '"' // model%link_libraries(i)%s // '"'
        if (i < size(model%link_libraries)) s = s // ", "
    end do
    s = s // "]"
    !    type(string_t), allocatable :: external_modules(:)
    s = s // ", external_modules=["
    do i = 1, size(model%external_modules)
        s = s // '"' // model%external_modules(i)%s // '"'
        if (i < size(model%external_modules)) s = s // ", "
    end do
    s = s // "]"
    !    type(dependency_tree_t) :: deps
    ! TODO: print `dependency_tree_t` properly, which should become part of the
    !       model, not imported from another file
    s = s // ", deps=dependency_tree_t(...)"

    ! Print module naming convention
    s = s // ', enforce_module_names="' // merge('T','F',model%enforce_module_names) // '"'

    ! Print custom prefix
    if (model%enforce_module_names .and. len_trim(model%module_prefix)>0) &
    s = s // ', custom_prefix="' // model%module_prefix%s // '"'

    !end type fpm_model_t
    s = s // ")"
end function info_model

subroutine show_model(model)
    ! Prints a human readable representation of the Model
    type(fpm_model_t), intent(in) :: model
    print *, info_model(model)
end subroutine show_model

end module fpm_model
