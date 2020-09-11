module fpm_model
! Definition and validation of the backend model
use fpm_strings, only: string_t
implicit none

private
public :: srcfile_ptr, srcfile_t, fpm_model_t

public :: FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
          FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
          FPM_UNIT_CHEADER

integer, parameter :: FPM_UNIT_UNKNOWN = -1
integer, parameter :: FPM_UNIT_PROGRAM = 1
integer, parameter :: FPM_UNIT_MODULE = 2
integer, parameter :: FPM_UNIT_SUBMODULE = 3
integer, parameter :: FPM_UNIT_SUBPROGRAM = 4
integer, parameter :: FPM_UNIT_CSOURCE = 5
integer, parameter :: FPM_UNIT_CHEADER = 6

type srcfile_ptr
    ! For constructing arrays of src_file pointers
    type(srcfile_t), pointer :: ptr => null()
end type srcfile_ptr

type srcfile_t
    ! Type for encapsulating a source file 
    !  and it's metadata
    character(:), allocatable :: file_name
        ! File path relative to cwd
    character(:), allocatable :: exe_name
        ! Name of executable for FPM_UNIT_PROGRAM
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
    type(srcfile_ptr), allocatable :: file_dependencies(:)
        ! Resolved source file dependencies

    logical :: built = .false.
    logical :: touched = .false.
end type srcfile_t

type :: fpm_model_t
    character(:), allocatable :: package_name
        ! Name of package
    type(srcfile_t), allocatable :: sources(:)
        ! Array of sources with module-dependencies resolved
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
