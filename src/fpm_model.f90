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
use fpm_strings, only: string_t, str, len_trim, upper, operator(==)
use fpm_toml, only: serializable_t, toml_table, toml_stat, set_value, set_list, get_value, get_list
use fpm_error, only: error_t, fatal_error
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
type, extends(serializable_t) :: fortran_features_t

    !> Use default implicit typing
    logical :: implicit_typing = .false.

    !> Use implicit external interface
    logical :: implicit_external = .false.

    !> Form to use for all Fortran sources
    character(:), allocatable :: source_form

    contains

        !> Serialization interface
        procedure :: serializable_is_same => fft_is_same
        procedure :: dump_to_toml   => fft_dump_to_toml
        procedure :: load_from_toml => fft_load_from_toml

end type fortran_features_t

!> Type for describing a source file
type, extends(serializable_t) :: srcfile_t
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

    contains

        !> Serialization interface
        procedure :: serializable_is_same => srcfile_is_same
        procedure :: dump_to_toml   => srcfile_dump_to_toml
        procedure :: load_from_toml => srcfile_load_from_toml

end type srcfile_t


!> Type for describing a single package
type, extends(serializable_t) :: package_t

    !> Name of package
    character(:), allocatable :: name

    !> Array of sources
    type(srcfile_t), allocatable :: sources(:)

    !> List of macros.
    type(string_t), allocatable :: macros(:)

    !> Package version number.
    character(:), allocatable :: version

    !> Module naming conventions
    logical :: enforce_module_names

    !> Prefix for all module names
    type(string_t) :: module_prefix

    !> Language features
    type(fortran_features_t) :: features

    contains

        !> Serialization interface
        procedure :: serializable_is_same => package_is_same
        procedure :: dump_to_toml   => package_dump_to_toml
        procedure :: load_from_toml => package_load_from_toml

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
    s = s // ', unit_scope="' // FPM_SCOPE_NAME(source%unit_scope) // '"'
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
    s = s // ', unit_type="' // FPM_UNIT_NAME(source%unit_type) // '"'
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

!> Return the character name of a scope flag
function FPM_SCOPE_NAME(flag) result(name)
    integer, intent(in) :: flag
    character(len=:), allocatable :: name

    select case (flag)
       case (FPM_SCOPE_UNKNOWN); name = "FPM_SCOPE_UNKNOWN"
       case (FPM_SCOPE_LIB);     name = "FPM_SCOPE_LIB"
       case (FPM_SCOPE_DEP);     name = "FPM_SCOPE_DEP"
       case (FPM_SCOPE_APP);     name = "FPM_SCOPE_APP"
       case (FPM_SCOPE_TEST);    name = "FPM_SCOPE_TEST"
       case (FPM_SCOPE_EXAMPLE); name = "FPM_SCOPE_EXAMPLE"
       case default;             name = "INVALID"
    end select
end function FPM_SCOPE_NAME

!> Parse git FPM_SCOPE identifier from a string
integer function parse_scope(name) result(scope)
    character(len=*), intent(in) :: name

    character(len=len(name)) :: uppercase

    !> Make it Case insensitive
    uppercase = upper(name)

    select case (trim(uppercase))
       case ("FPM_SCOPE_UNKNOWN"); scope = FPM_SCOPE_UNKNOWN
       case ("FPM_SCOPE_LIB");     scope = FPM_SCOPE_LIB
       case ("FPM_SCOPE_DEP");     scope = FPM_SCOPE_DEP
       case ("FPM_SCOPE_APP");     scope = FPM_SCOPE_APP
       case ("FPM_SCOPE_TEST");    scope = FPM_SCOPE_TEST
       case ("FPM_SCOPE_EXAMPLE"); scope = FPM_SCOPE_EXAMPLE
       case default;               scope = -9999
    end select

end function parse_scope

!> Return the character name of a unit flag
function FPM_UNIT_NAME(flag) result(name)
    integer, intent(in) :: flag
    character(len=:), allocatable :: name

    select case (flag)
       case (FPM_UNIT_UNKNOWN);    name = "FPM_UNIT_UNKNOWN"
       case (FPM_UNIT_PROGRAM);    name = "FPM_UNIT_PROGRAM"
       case (FPM_UNIT_MODULE);     name = "FPM_UNIT_MODULE"
       case (FPM_UNIT_SUBMODULE);  name = "FPM_UNIT_SUBMODULE"
       case (FPM_UNIT_SUBPROGRAM); name = "FPM_UNIT_SUBPROGRAM"
       case (FPM_UNIT_CSOURCE);    name = "FPM_UNIT_CSOURCE"
       case (FPM_UNIT_CPPSOURCE);  name = "FPM_UNIT_CPPSOURCE"
       case (FPM_UNIT_CHEADER);    name = "FPM_UNIT_CHEADER"
       case default;               name = "INVALID"
    end select
end function FPM_UNIT_NAME

!> Parse git FPM_UNIT identifier from a string
integer function parse_unit(name) result(unit)
    character(len=*), intent(in) :: name

    character(len=len(name)) :: uppercase

    !> Make it Case insensitive
    uppercase = upper(name)

    select case (trim(uppercase))
       case ("FPM_UNIT_UNKNOWN");    unit = FPM_UNIT_UNKNOWN
       case ("FPM_UNIT_PROGRAM");    unit = FPM_UNIT_PROGRAM
       case ("FPM_UNIT_MODULE");     unit = FPM_UNIT_MODULE
       case ("FPM_UNIT_SUBMODULE");  unit = FPM_UNIT_SUBMODULE
       case ("FPM_UNIT_SUBPROGRAM"); unit = FPM_UNIT_SUBPROGRAM
       case ("FPM_UNIT_CSOURCE");    unit = FPM_UNIT_CSOURCE
       case ("FPM_UNIT_CPPSOURCE");  unit = FPM_UNIT_CPPSOURCE
       case ("FPM_UNIT_CHEADER");    unit = FPM_UNIT_CHEADER
       case default;                 unit = -9999
    end select

end function parse_unit

!> Check that two source files are equal
logical function srcfile_is_same(this,that)
    class(srcfile_t), intent(in) :: this
    class(serializable_t), intent(in) :: that

    srcfile_is_same = .false.

    select type (other=>that)
       type is (srcfile_t)

          if (.not.(this%file_name==other%file_name)) return
          if (.not.(this%exe_name==other%exe_name)) return
          if (.not.(this%unit_scope==other%unit_scope)) return
          if (.not.(this%modules_provided==other%modules_provided)) return
          if (.not.(this%unit_type==other%unit_type)) return
          if (.not.(this%parent_modules==other%parent_modules)) return
          if (.not.(this%modules_used==other%modules_used)) return
          if (.not.(this%include_dependencies==other%include_dependencies)) return
          if (.not.(this%link_libraries==other%link_libraries)) return
          if (.not.(this%digest==other%digest)) return

       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    srcfile_is_same = .true.

end function srcfile_is_same

!> Dump dependency to toml table
subroutine srcfile_dump_to_toml(self, table, error)

    !> Instance of the serializable object
    class(srcfile_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ierr

    if (allocated(self%file_name)) then
        call set_value(table, "file-name", self%file_name, ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'srcfile_t: cannot set file-name in TOML table')
            return
        end if
    endif

    if (allocated(self%exe_name)) then
        call set_value(table, "exe-name", self%exe_name, ierr)
        if (ierr/=toml_stat%success) then
            call fatal_error(error,'srcfile_t: cannot set exe-name in TOML table')
            return
        end if
    endif

    call set_value(table,"digest",self%digest)

    ! unit_scope and unit_type are saved as strings so the output is independent
    ! of the internal representation
    call set_value(table,"unit-scope",FPM_SCOPE_NAME(self%unit_scope))
    call set_value(table,"unit-type",FPM_UNIT_NAME(self%unit_type))

    call set_list(table,"modules-provided",self%modules_provided, error)
    if (allocated(error)) return

    call set_list(table,"parent-modules",self%parent_modules, error)
    if (allocated(error)) return

    call set_list(table,"modules-used",self%modules_used, error)
    if (allocated(error)) return

    call set_list(table,"include-dependencies",self%include_dependencies, error)
    if (allocated(error)) return

    call set_list(table,"link-libraries",self%link_libraries, error)
    if (allocated(error)) return


end subroutine srcfile_dump_to_toml

!> Read dependency from toml table (no checks made at this stage)
subroutine srcfile_load_from_toml(self, table, error)

    !> Instance of the serializable object
    class(srcfile_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: flag

    call get_value(table, "file-name", self%file_name)
    call get_value(table, "exe-name", self%exe_name)
    call get_value(table, "digest", self%digest)

    ! unit_scope and unit_type are saved as strings so the output is independent
    ! of the internal representation
    call get_value(table, "unit-scope", flag)
    if (allocated(flag)) self%unit_scope = parse_scope(flag)
    call get_value(table, "unit-type", flag)
    if (allocated(flag)) self%unit_type = parse_unit(flag)

    call get_list(table,"modules-provided",self%modules_provided, error)
    if (allocated(error)) return

    call get_list(table,"parent-modules",self%parent_modules, error)
    if (allocated(error)) return

    call get_list(table,"modules-used",self%modules_used, error)
    if (allocated(error)) return

    call get_list(table,"include-dependencies",self%include_dependencies, error)
    if (allocated(error)) return

    call get_list(table,"link-libraries",self%link_libraries, error)
    if (allocated(error)) return



end subroutine srcfile_load_from_toml

!> Check that two fortran feature objects are equal
logical function fft_is_same(this,that)
    class(fortran_features_t), intent(in) :: this
    class(serializable_t), intent(in) :: that

    fft_is_same = .false.

    select type (other=>that)
       type is (fortran_features_t)

           if (.not.(this%implicit_typing.eqv.other%implicit_typing)) return
           if (.not.(this%implicit_external.eqv.other%implicit_external)) return
           if (.not.(this%source_form==other%source_form)) return

       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    fft_is_same = .true.

end function fft_is_same

!> Dump fortran features to toml table
subroutine fft_dump_to_toml(self, table, error)

    !> Instance of the serializable object
    class(fortran_features_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call fatal_error(error,' fortran-features-t serialization not yet implemented ' )

end subroutine fft_dump_to_toml

!> Read dependency from toml table (no checks made at this stage)
subroutine fft_load_from_toml(self, table, error)

    !> Instance of the serializable object
    class(fortran_features_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: flag

    call fatal_error(error, ' fortran-features-t serialization not yet implemented ')

end subroutine fft_load_from_toml

!> Check that two package objects are equal
logical function package_is_same(this,that)
    class(package_t), intent(in) :: this
    class(serializable_t), intent(in) :: that

    integer :: ii

    package_is_same = .false.

    select type (other=>that)
       type is (package_t)

           if (.not.(this%name==other%name)) return
           if (.not.(allocated(this%sources).eqv.allocated(other%sources))) return
           if (allocated(this%sources)) then
              if (.not.(size(this%sources)==size(other%sources))) return
              do ii = 1, size(this%sources)
                  if (.not.(this%sources(ii)==other%sources(ii))) return
              end do
           end if

           if (.not.(this%macros==other%macros)) return
           if (.not.(this%version==other%version)) return

           !> Module naming
           if (.not.(this%enforce_module_names.eqv.other%enforce_module_names)) return
           if (.not.(this%module_prefix==other%module_prefix)) return

           !> Fortran features
           if (.not.(this%features==other%features)) return

       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    package_is_same = .true.

end function package_is_same

!> Dump dependency to toml table
subroutine package_dump_to_toml(self, table, error)

    !> Instance of the serializable object
    class(package_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call fatal_error(error,' not yet implemented ' )

end subroutine package_dump_to_toml

!> Read dependency from toml table (no checks made at this stage)
subroutine package_load_from_toml(self, table, error)

    !> Instance of the serializable object
    class(package_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: flag

    call fatal_error(error, ' not yet implemented ')

end subroutine package_load_from_toml


end module fpm_model
