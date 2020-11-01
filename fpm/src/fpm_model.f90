module fpm_model
! Definition and validation of the backend model
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

integer, parameter :: FPM_UNIT_UNKNOWN = -1
integer, parameter :: FPM_UNIT_PROGRAM = 1
integer, parameter :: FPM_UNIT_MODULE = 2
integer, parameter :: FPM_UNIT_SUBMODULE = 3
integer, parameter :: FPM_UNIT_SUBPROGRAM = 4
integer, parameter :: FPM_UNIT_CSOURCE = 5
integer, parameter :: FPM_UNIT_CHEADER = 6

integer, parameter :: FPM_SCOPE_UNKNOWN = -1
integer, parameter :: FPM_SCOPE_LIB = 1
integer, parameter :: FPM_SCOPE_DEP = 2
integer, parameter :: FPM_SCOPE_APP = 3
integer, parameter :: FPM_SCOPE_TEST = 4

integer, parameter :: FPM_TARGET_UNKNOWN = -1
integer, parameter :: FPM_TARGET_EXECUTABLE = 1
integer, parameter :: FPM_TARGET_ARCHIVE = 2
integer, parameter :: FPM_TARGET_OBJECT = 3

type srcfile_t
    ! Type for encapsulating a source file 
    !  and it's metadata
    character(:), allocatable :: file_name
        ! File path relative to cwd
    character(:), allocatable :: exe_name
        ! Name of executable for FPM_UNIT_PROGRAM
    integer :: unit_scope = FPM_SCOPE_UNKNOWN
        ! app/test/lib/dependency
    logical :: is_test = .false.
        ! Is executable a test?
    type(string_t), allocatable :: modules_provided(:)
        ! Modules provided by this source file (lowerstring)
    integer :: unit_type = FPM_UNIT_UNKNOWN
        ! Type of program unit
    type(string_t), allocatable :: modules_used(:)
        ! Modules USEd by this source file (lowerstring)
    type(string_t), allocatable :: include_dependencies(:)
        ! Files INCLUDEd by this source file
end type srcfile_t

type build_target_ptr
    ! For constructing arrays of build_target_t pointers
    type(build_target_t), pointer :: ptr => null()
end type build_target_ptr

type build_target_t
    character(:), allocatable :: output_file
        ! File path of build target object relative to cwd
    type(srcfile_t), allocatable :: source
        ! Primary source for this build target
    type(build_target_ptr), allocatable :: dependencies(:)
        ! Resolved build dependencies
    integer :: target_type = FPM_TARGET_UNKNOWN

    logical :: built = .false.
    logical :: touched = .false.

end type build_target_t

type :: fpm_model_t
    character(:), allocatable :: package_name
        ! Name of package
    type(srcfile_t), allocatable :: sources(:)
        ! Array of sources
    type(build_target_ptr), allocatable :: targets(:)
        ! Array of targets with module-dependencies resolved
    character(:), allocatable :: fortran_compiler
        ! Command line name to invoke fortran compiler
    character(:), allocatable :: fortran_compile_flags
        ! Command line flags passed to fortran for compilation
    character(:), allocatable :: link_flags
        ! Command line flags pass for linking
    character(:), allocatable :: output_directory
        ! Base directory for build
end type fpm_model_t

end module fpm_model
