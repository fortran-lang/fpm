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
use fpm_strings, only: string_t
implicit none

private
public :: fpm_model_t, srcfile_t, build_target_t, build_target_ptr

public :: FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
          FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
          FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
          FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST, &
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


!> Type describing everything required to build a package
!> and its dependencies.
type :: fpm_model_t

    !> Name of package
    character(:), allocatable :: package_name

    !> Array of sources
    type(srcfile_t), allocatable :: sources(:)

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
    
end type fpm_model_t

end module fpm_model
