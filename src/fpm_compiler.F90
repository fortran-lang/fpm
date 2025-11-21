!># Define compiler command options
!!
!! This module defines compiler options to use for the debug and release builds.

! vendor            Fortran   C         Module output   Module include OpenMP    Free for OSS
!                   compiler  compiler  directory       directory
! Gnu               gfortran   gcc     -J              -I            -fopenmp   X
! Intel             ifort      icc     -module         -I            -qopenmp   X
! Intel(Windows)    ifort      icc     /module:path    /I            /Qopenmp   X
! Intel oneAPI      ifx        icx     -module         -I            -qopenmp   X
! PGI               pgfortran  pgcc    -module         -I            -mp        X
! NVIDIA            nvfortran  nvc     -module         -I            -mp        X
! LLVM flang        flang      clang   -module-dir     -I            -fopenmp   X
! AMD flang         amdflang   amdclang -module-dir    -I            -fopenmp   X
! LFortran          lfortran   ---     -J              -I            --openmp   X
! Lahey/Futjitsu    lfc        ?       -M              -I            -openmp    ?
! NAG               nagfor     ?       -mdir           -I            -openmp    x
! Cray              crayftn    craycc  -J              -I            -homp      ?
! IBM               xlf90      ?       -qmoddir        -I            -qsmp      X
! Oracle/Sun        ?          ?       -moddir=        -M            -xopenmp   ?
! Silverfrost FTN95 ftn95      ?       ?               /MOD_PATH     ?          ?
! Elbrus            ?          lcc     -J              -I            -fopenmp   ?
! Hewlett Packard   ?          ?       ?               ?             ?          discontinued
! Watcom            ?          ?       ?               ?             ?          discontinued
! PathScale         ?          ?       -module         -I            -mp        discontinued
! G95               ?          ?       -fmod=          -I            -fopenmp   discontinued
! Open64            ?          ?       -module         -I            -mp        discontinued
! Unisys            ?          ?       ?               ?             ?          discontinued
module fpm_compiler
use,intrinsic :: iso_fortran_env, only: stderr=>error_unit
use fpm_environment, only: &
        get_os_type, &
        OS_LINUX, &
        OS_MACOS, &
        OS_WINDOWS, &
        OS_CYGWIN, &
        OS_SOLARIS, &
        OS_FREEBSD, &
        OS_OPENBSD, &
        OS_UNKNOWN, &
        library_filename
use fpm_filesystem, only: join_path, basename, get_temp_filename, delete_file, unix_path, &
    & getline, run
use fpm_strings, only: split, string_cat, string_t, str_ends_with, str_begins_with_str, &
    & string_array_contains, lower, add_strings
use fpm_error, only: error_t, fatal_error, fpm_stop
use tomlf, only: toml_table
use fpm_toml, only: serializable_t, set_string, set_value, toml_stat, get_value
use fpm_compile_commands, only: compile_command_t, compile_command_table_t
use fpm_versioning, only: version_t
use shlex_module, only: sh_split => split, ms_split, quote => ms_quote
implicit none
public :: compiler_t, new_compiler, archiver_t, new_archiver, get_macros
public :: append_clean_flags, append_clean_flags_array
public :: debug
public :: id_gcc,id_all
public :: match_compiler_type, compiler_id_name, validate_compiler_name, is_cxx_gnu_based

enum, bind(C)
    enumerator :: &
        id_all = -1, &
        id_unknown, &
        id_gcc, &
        id_f95, &
        id_caf, &
        id_intel_classic_nix, &
        id_intel_classic_mac, &
        id_intel_classic_windows, &
        id_intel_llvm_nix, &
        id_intel_llvm_windows, &
        id_intel_llvm_unknown, &
        id_pgi, &
        id_nvhpc, &
        id_nag, &
        id_flang_classic, &
        id_flang, &
        id_amdflang, &
        id_f18, &
        id_ibmxl, &
        id_cray, &
        id_lahey, &
        id_lfortran
end enum
integer, parameter :: compiler_enum = kind(id_unknown)

!> Definition of compiler object
type, extends(serializable_t) :: compiler_t
    !> Identifier of the compiler
    integer(compiler_enum) :: id = id_unknown
    !> Path to the Fortran compiler
    character(len=:), allocatable :: fc
    !> Path to the C compiler
    character(len=:), allocatable :: cc
    !> Path to the C++ compiler
    character(len=:), allocatable :: cxx
    !> Print all commands
    logical :: echo = .true.
    !> Verbose output of command
    logical :: verbose = .true.
contains
    !> Get default compiler flags
    procedure :: get_default_flags
    !> Get flag for module output directories
    procedure :: get_module_flag
    !> Get flag for include directories
    procedure :: get_include_flag
    !> Get feature flag
    procedure :: get_feature_flag
    !> Get flags for the main linking command
    procedure :: get_main_flags
    !> Get library export flags
    procedure :: get_export_flags    
    !> Get library install name flags
    procedure :: get_install_name_flags
    !> Generate header padding flags for macOS executables
    procedure :: get_headerpad_flags
    !> Compile a Fortran object
    procedure :: compile_fortran
    !> Compile a C object
    procedure :: compile_c
    !> Compile a CPP object
    procedure :: compile_cpp
    !> Link a shared library
    procedure :: link_shared
    !> Link executable
    procedure :: link => link_executable
    !> Check whether compiler is recognized
    procedure :: is_unknown
    !> Check whether this is an Intel compiler
    procedure :: is_intel
    !> Check whether this is a GNU compiler
    procedure :: is_gnu
    !> Enumerate libraries, based on compiler and platform
    procedure :: enumerate_libraries


    !> Serialization interface
    procedure :: serializable_is_same => compiler_is_same
    procedure :: dump_to_toml => compiler_dump
    procedure :: load_from_toml => compiler_load
    !> Fortran feature support
    procedure :: check_fortran_source_runs
    procedure :: check_flags_supported
    procedure :: with_xdp
    procedure :: with_qp
    !> C feature support
    procedure :: check_c_source_runs
    procedure :: check_c_flags_supported
    !> C++ feature support
    procedure :: check_cxx_source_runs
    procedure :: check_cxx_flags_supported
    !> Return compiler name
    procedure :: name => compiler_name

end type compiler_t


!> Definition of archiver object
type, extends(serializable_t) :: archiver_t
    !> Path to archiver
    character(len=:), allocatable :: ar
    !> Use response files to pass arguments
    logical :: use_response_file = .false.
    !> Print all command
    logical :: echo = .true.
    !> Verbose output of command
    logical :: verbose = .true.
contains
    !> Create static archive
    procedure :: make_archive

    !> Serialization interface
    procedure :: serializable_is_same => ar_is_same
    procedure :: dump_to_toml
    procedure :: load_from_toml

end type archiver_t


!> Create debug printout
interface debug
    module procedure :: debug_compiler
    module procedure :: debug_archiver
end interface debug

character(*), parameter :: &
    flag_gnu_coarray = " -fcoarray=single", &
    flag_gnu_backtrace = " -fbacktrace", &
    flag_gnu_opt = " -O3 -funroll-loops", &
    flag_gnu_debug = " -g", &
    flag_gnu_pic = " -fPIC", &
    flag_gnu_warn = " -Wall -Wextra -Wno-external-argument-mismatch", & ! do not check interfaces due to gcc 15.0-15.1 bug
    flag_gnu_check = " -fcheck=bounds -fcheck=array-temps", &
    flag_gnu_limit = " -fmax-errors=1", &
    flag_gnu_external = " -Wimplicit-interface", &
    flag_gnu_openmp = " -fopenmp", &
    flag_gnu_no_implicit_typing = " -fimplicit-none", &
    flag_gnu_no_implicit_external = " -Werror=implicit-interface", &
    flag_gnu_free_form = " -ffree-form", &
    flag_gnu_fixed_form = " -ffixed-form"

character(*), parameter :: &
    flag_pgi_backslash = " -Mbackslash", &
    flag_pgi_traceback = " -traceback", &
    flag_pgi_debug = " -g", &
    flag_pgi_check = " -Mbounds -Mchkptr -Mchkstk", &
    flag_pgi_warn = " -Minform=inform", &
    flag_pgi_openmp = " -mp", &
    flag_pgi_free_form = " -Mfree", &
    flag_pgi_fixed_form = " -Mfixed"

character(*), parameter :: &
    flag_ibmxl_backslash = " -qnoescape"

character(*), parameter :: &
    flag_intel_backtrace = " -traceback", &
    flag_intel_warn = " -warn all", &
    flag_intel_check = " -check all", &
    flag_intel_debug = " -O0 -g", &
    flag_intel_opt = " -O3", &
    flag_intel_fp = " -fp-model precise -pc64", &
    flag_intel_align = " -align all", &
    flag_intel_limit = " -error-limit 1", &
    flag_intel_pthread = " -reentrancy threaded", &
    flag_intel_nogen = " -nogen-interfaces", &
    flag_intel_byterecl = " -assume byterecl", &
    flag_intel_openmp = " -qopenmp", &
    flag_intel_free_form = " -free", &
    flag_intel_fixed_form = " -fixed", &
    flag_intel_standard_compliance = " -standard-semantics", &
    flag_intel_unknown_cmd_err = " -diag-error 10006"

character(*), parameter :: &
    flag_intel_llvm_check = " -check all,nouninit"

character(*), parameter :: &
    flag_intel_backtrace_win = " /traceback", &
    flag_intel_warn_win = " /warn:all", &
    flag_intel_check_win = " /check:all", &
    flag_intel_debug_win = " /Od /Z7", &
    flag_intel_opt_win = " /O3", &
    flag_intel_fp_win = " /fp:precise", &
    flag_intel_align_win = " /align:all", &
    flag_intel_limit_win = " /error-limit:1", &
    flag_intel_pthread_win = " /reentrancy:threaded", &
    flag_intel_nogen_win = " /nogen-interfaces", &
    flag_intel_byterecl_win = " /assume:byterecl", &
    flag_intel_openmp_win = " /Qopenmp", &
    flag_intel_free_form_win = " /free", &
    flag_intel_fixed_form_win = " /fixed", &
    flag_intel_standard_compliance_win = " /standard-semantics", &
    flag_intel_unknown_cmd_err_win = " /Qdiag-error:10006"

character(*), parameter :: &
    flag_nag_coarray = " -coarray=single", &
    flag_nag_pic = " -PIC", &
    flag_nag_check = " -C", &
    flag_nag_debug = " -g -O0", &
    flag_nag_opt = " -O4", &
    flag_nag_backtrace = " -gline", &
    flag_nag_openmp = " -openmp", &
    flag_nag_free_form = " -free", &
    flag_nag_fixed_form = " -fixed", &
    flag_nag_no_implicit_typing = " -u"

character(*), parameter :: &
    flag_lfortran_opt = " --fast", &
    flag_lfortran_openmp = " --openmp", &
    flag_lfortran_implicit_typing = " --implicit-typing", &
    flag_lfortran_implicit_external = " --implicit-interface", &
    flag_lfortran_fixed_form = " --fixed-form"

character(*), parameter :: &
    flag_cray_no_implicit_typing = " -dl", &
    flag_cray_implicit_typing = " -el", &
    flag_cray_fixed_form = " -ffixed", &
    flag_cray_free_form = " -ffree"

character(*), parameter :: &
    flag_flang_openmp = " -fopenmp", &
    flag_flang_debug = " -g", &
    flag_flang_opt = " -O3", &
    flag_flang_pic = " -fPIC", &
    flag_flang_free_form = " -ffree-form", &
    flag_flang_fixed_form = " -ffixed-form", &
    flag_flang_no_implicit_typing = " -fimplicit-none"

contains


function get_default_flags(self, release) result(flags)
    class(compiler_t), intent(in) :: self
    logical, intent(in) :: release
    character(len=:), allocatable :: flags

    character(len=:), allocatable :: pic_flag

    if (release) then
        call get_release_compile_flags(self%id, flags)
    else
        call get_debug_compile_flags(self%id, flags)
    end if

    ! Append position-independent code (PIC) flag, that is necessary 
    ! building shared libraries
    select case (self%id)
    case (id_gcc, id_f95, id_caf, id_flang_classic, id_amdflang, id_f18, id_lfortran, &
          id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix, &
          id_intel_llvm_unknown, id_pgi, id_nvhpc, id_nag, id_cray, id_ibmxl)
        pic_flag = " -fPIC"
    case (id_flang)
        ! LLVM Flang doesn't support -fPIC on Windows MSVC target
        if (get_os_type() == OS_WINDOWS) then
            pic_flag = ""
        else
            pic_flag = " -fPIC"
        end if
    case (id_intel_classic_windows, id_intel_llvm_windows)
        pic_flag = ""  ! Windows does not use -fPIC
    case default
        pic_flag = " -fPIC"  ! Conservative fallback
    end select

    flags = flags // pic_flag

end function get_default_flags

subroutine get_release_compile_flags(id, flags)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(out) :: flags

    select case(id)
    case default
        flags = ""
    case(id_caf)
        flags = &
            flag_gnu_opt//&
            flag_gnu_external//&
            flag_gnu_pic//&
            flag_gnu_limit

    case(id_gcc)
        flags = &
            flag_gnu_opt//&
            flag_gnu_external//&
            flag_gnu_pic//&
            flag_gnu_limit//&
            flag_gnu_coarray

    case(id_f95)
        flags = &
            flag_gnu_opt//&
            flag_gnu_external//&
            flag_gnu_pic//&
            flag_gnu_limit

    case(id_nvhpc)
        flags = &
            flag_pgi_backslash

    case(id_ibmxl)
        flags = &
            flag_ibmxl_backslash

    case(id_intel_classic_nix)
        flags = &
            flag_intel_opt//&
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_classic_mac)
        flags = &
            flag_intel_opt//&
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_classic_windows)
        flags = &
            flag_intel_opt_win//&
            flag_intel_fp_win//&
            flag_intel_align_win//&
            flag_intel_limit_win//&
            flag_intel_pthread_win//&
            flag_intel_nogen_win//&
            flag_intel_byterecl_win

    case(id_intel_llvm_nix)
        flags = &
            flag_intel_opt//&
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_llvm_windows)
        flags = &
            flag_intel_opt_win//&
            flag_intel_fp_win//&
            flag_intel_align_win//&
            flag_intel_limit_win//&
            flag_intel_pthread_win//&
            flag_intel_nogen_win//&
            flag_intel_byterecl_win

    case(id_nag)
        flags = &
            flag_nag_opt//&
            flag_nag_coarray//&
            flag_nag_pic

    case(id_lfortran)
        flags = &
            flag_lfortran_opt

    case(id_flang)
        flags = &
            flag_flang_opt//&

            flag_flang_pic
    case(id_amdflang)
        flags = &
            flag_flang_opt//&
            flag_flang_pic

    end select
end subroutine get_release_compile_flags

subroutine get_debug_compile_flags(id, flags)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(out) :: flags

    select case(id)
    case default
        flags = ""
    case(id_caf)
        flags = &
            flag_gnu_warn//&
            flag_gnu_pic//&
            flag_gnu_limit//&
            flag_gnu_debug//&
            flag_gnu_check//&
            flag_gnu_backtrace
    case(id_gcc)
        flags = &
            flag_gnu_warn//&
            flag_gnu_pic//&
            flag_gnu_limit//&
            flag_gnu_debug//&
            flag_gnu_check//&
            flag_gnu_backtrace//&
            flag_gnu_coarray
    case(id_f95)
        flags = &
            flag_gnu_warn//&
            flag_gnu_pic//&
            flag_gnu_limit//&
            flag_gnu_debug//&
            flag_gnu_check//&
            ' -Wno-maybe-uninitialized -Wno-uninitialized'//&
            flag_gnu_backtrace
    case(id_nvhpc)
        flags = &
            flag_pgi_warn//&
            flag_pgi_backslash//&
            flag_pgi_check//&
            flag_pgi_traceback
    case(id_ibmxl)
        flags = &
            flag_ibmxl_backslash
    case(id_intel_classic_nix)
        flags = &
            flag_intel_warn//&
            flag_intel_check//&
            flag_intel_limit//&
            flag_intel_debug//&
            flag_intel_byterecl//&
            flag_intel_backtrace

    case(id_intel_classic_mac)
        flags = &
            flag_intel_warn//&
            flag_intel_check//&
            flag_intel_limit//&
            flag_intel_debug//&
            flag_intel_byterecl//&
            flag_intel_backtrace
    case(id_intel_classic_windows)
        flags = &
            flag_intel_warn_win//&
            flag_intel_check_win//&
            flag_intel_limit_win//&
            flag_intel_debug_win//&
            flag_intel_byterecl_win//&
            flag_intel_backtrace_win
    case(id_intel_llvm_nix)
        flags = &
            flag_intel_unknown_cmd_err//&
            flag_intel_llvm_check//&
            flag_intel_limit//&
            flag_intel_debug//&
            flag_intel_byterecl//&
            flag_intel_backtrace
    case(id_intel_llvm_windows)
        flags = &
            flag_intel_unknown_cmd_err_win//&
            flag_intel_check_win//&
            flag_intel_limit_win//&
            flag_intel_debug_win//&
            flag_intel_byterecl_win
    case(id_nag)
        flags = &
            flag_nag_debug//&
            flag_nag_check//&
            flag_nag_backtrace//&
            flag_nag_coarray//&
            flag_nag_pic

    case(id_lfortran)
        flags = ""

    case(id_flang)
        flags = &
            flag_flang_debug//&
            flag_flang_pic

    case(id_amdflang)
        flags = &
            flag_flang_debug//&
            flag_flang_pic

    end select
end subroutine get_debug_compile_flags

pure subroutine set_cpp_preprocessor_flags(id, flags)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(inout) :: flags
    character(len=:), allocatable :: flag_cpp_preprocessor

    !> Modify the flag_cpp_preprocessor on the basis of the compiler.
    select case(id)
    case default
        flag_cpp_preprocessor = ""
    case(id_caf, id_gcc, id_f95, id_nvhpc, id_flang, id_amdflang)
        flag_cpp_preprocessor = "-cpp"
    case(id_intel_classic_windows, id_intel_llvm_windows)
        flag_cpp_preprocessor = "/fpp"
    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix, id_nag)
        flag_cpp_preprocessor = "-fpp"
    case(id_lfortran)
        flag_cpp_preprocessor = "--cpp"
    end select

    flags = flag_cpp_preprocessor// flags

end subroutine set_cpp_preprocessor_flags

!> This function will parse and read the macros list and
!> return them as defined flags.
function get_macros(id, macros_list, version) result(macros)
    integer(compiler_enum), intent(in) :: id
    type(version_t), optional, intent(in) :: version
    type(string_t), allocatable, intent(in) :: macros_list(:)

    character(len=:), allocatable :: macros
    character(len=:), allocatable :: macro_definition_symbol
    character(:), allocatable :: valued_macros(:)


    integer :: i

    if (.not.allocated(macros_list)) then
        macros = ""
        return
    end if

    !> Set macro defintion symbol on the basis of compiler used
    select case(id)
    case default
        macro_definition_symbol = " -D"
    case (id_intel_classic_windows, id_intel_llvm_windows)
        macro_definition_symbol = " /D"
    end select

    !> Check if macros are not allocated.
    if (.not.allocated(macros)) then
        macros=""
    end if

    do i = 1, size(macros_list)

        !> Split the macro name and value.
        call split(macros_list(i)%s, valued_macros, delimiters="=")

        !> Replace {version} placeholder with the actual version string
        if (size(valued_macros) > 1) then
            !> Check if the value of macro starts with '{' character.
            if (str_begins_with_str(trim(valued_macros(size(valued_macros))), "{")) then

                !> Check if the value of macro ends with '}' character.
                if (str_ends_with(trim(valued_macros(size(valued_macros))), "}")) then

                    !> Check if the string contains "version" as substring.
                    if (index(valued_macros(size(valued_macros)), "version") /= 0) then

                        !> These conditions are placed in order to ensure proper spacing between the macros.
                        if (present(version)) then 
                           
                           macros = macros//macro_definition_symbol//trim(valued_macros(1))//'='//version%s()
                           cycle
                        
                        else
                            
                           call fpm_stop(1,'Internal error: cannot expand {version} macro in '//macros_list(i)%s)
                        
                        endif
                        
                    end if
                end if
            end if
        end if

        macros = macros//macro_definition_symbol//macros_list(i)%s

    end do

end function get_macros

function get_include_flag(self, path) result(flags)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: flags

    select case(self%id)
    case default
        flags = "-I "//path

    case(id_caf, id_gcc, id_f95, id_cray, id_nvhpc, id_pgi, &
        & id_flang_classic, id_flang, id_amdflang, id_f18, &
        & id_intel_classic_nix, id_intel_classic_mac, &
        & id_intel_llvm_nix, id_lahey, id_nag, id_ibmxl, &
        & id_lfortran)
        flags = "-I "//path

    case(id_intel_classic_windows, id_intel_llvm_windows)
        flags = "/I"//path

    end select
end function get_include_flag

function get_module_flag(self, path) result(flags)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: flags

    select case(self%id)
    case default
        flags = "-module "//path

    case(id_caf, id_gcc, id_f95, id_cray, id_lfortran)
        flags = "-J "//path

    case(id_nvhpc, id_pgi, id_flang_classic)
        flags = "-module "//path

    case(id_flang, id_f18, id_amdflang)
        flags = "-module-dir "//path

    case(id_intel_classic_nix, id_intel_classic_mac, &
        & id_intel_llvm_nix)
        flags = "-module "//path

    case(id_intel_classic_windows, id_intel_llvm_windows)
        flags = "/module:"//path

    case(id_lahey)
        flags = "-M "//path

    case(id_nag)
        flags = "-mdir "//path

    case(id_ibmxl)
        flags = "-qmoddir "//path

    end select

end function get_module_flag


function get_shared_flag(self) result(shared_flag)
    class(compiler_t), intent(in) :: self
    character(len=:), allocatable :: shared_flag

    select case (self%id)
    case default
        shared_flag = "-shared"
    case (id_gcc, id_f95, id_flang, id_amdflang, id_flang_classic, id_lfortran)
        shared_flag = "-shared"
    case (id_intel_classic_nix, id_intel_llvm_nix, id_pgi, id_nvhpc)
        shared_flag = "-shared"
    case (id_intel_classic_windows, id_intel_llvm_windows)
        shared_flag = "/DLL"
    case (id_nag)
        shared_flag = "-Wl,-shared"
    case (id_ibmxl)
        shared_flag = "-qmkshrobj"
    case (id_cray, id_lahey)
        shared_flag = ""  ! Needs special handling
    end select

end function get_shared_flag


function get_feature_flag(self, feature) result(flags)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: feature
    character(len=:), allocatable :: flags

    flags = ""
    select case(feature)
    case("no-implicit-typing")
       select case(self%id)
       case(id_caf, id_gcc, id_f95)
           flags = flag_gnu_no_implicit_typing

       case(id_nag)
           flags = flag_nag_no_implicit_typing

       case(id_cray)
           flags = flag_cray_no_implicit_typing

       case(id_flang)
           flags = flag_flang_no_implicit_typing

       case(id_amdflang)
           flags = flag_flang_no_implicit_typing

       end select

    case("implicit-typing")
       select case(self%id)
       case(id_cray)
           flags = flag_cray_implicit_typing

       case(id_lfortran)
           flags = flag_lfortran_implicit_typing

       end select

    case("no-implicit-external")
       select case(self%id)
       case(id_caf, id_gcc, id_f95)
           flags = flag_gnu_no_implicit_external

       end select

    case("implicit-external")
       select case(self%id)
       case(id_lfortran)
           flags = flag_lfortran_implicit_external

       end select

    case("free-form")

       select case(self%id)
       case(id_caf, id_gcc, id_f95)
           flags = flag_gnu_free_form

       case(id_pgi, id_nvhpc, id_flang_classic)
           flags = flag_pgi_free_form

       case(id_nag)
           flags = flag_nag_free_form

       case(id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix, &
             & id_intel_llvm_unknown)
           flags = flag_intel_free_form

       case(id_intel_classic_windows, id_intel_llvm_windows)
           flags = flag_intel_free_form_win

       case(id_cray)
           flags = flag_cray_free_form

       case(id_flang)
           flags = flag_flang_free_form

       case(id_amdflang)
           flags = flag_flang_free_form

       end select

    case("fixed-form")
       select case(self%id)
       case(id_caf, id_gcc, id_f95)
           flags = flag_gnu_fixed_form

       case(id_pgi, id_nvhpc, id_flang_classic)
           flags = flag_pgi_fixed_form

       case(id_nag)
           flags = flag_nag_fixed_form

       case(id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix, &
             & id_intel_llvm_unknown)
           flags = flag_intel_fixed_form

       case(id_intel_classic_windows, id_intel_llvm_windows)
           flags = flag_intel_fixed_form_win

       case(id_cray)
           flags = flag_cray_fixed_form

       case(id_lfortran)
           flags = flag_lfortran_fixed_form

       case(id_flang)
           flags = flag_flang_fixed_form

       case(id_amdflang)
           flags = flag_flang_fixed_form

       end select

    case("default-form")
        continue

    case default
        error stop "Unknown feature '"//feature//"'"
    end select
end function get_feature_flag


!> Get special flags for the main linker
subroutine get_main_flags(self, language, flags)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: language
    character(len=:), allocatable, intent(out) :: flags

    flags = ""
    select case(language)

    case("fortran")
        flags = ""

    case("c")

        ! If the main program is on a C/C++ source, the Intel Fortran compiler requires option
        ! -nofor-main to avoid "duplicate main" errors.
        ! https://stackoverflow.com/questions/36221612/p3dfft-compilation-ifort-compiler-error-multiple-definiton-of-main
        select case(self%id)
           case(id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix)
               flags = '-nofor-main'
           case(id_intel_classic_windows,id_intel_llvm_windows)
               flags = '/nofor-main'
           case (id_pgi,id_nvhpc)
               flags = '-Mnomain'
        end select

    case("c++","cpp","cxx")

        select case(self%id)
           case(id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix)
               flags = '-nofor-main'
           case(id_intel_classic_windows,id_intel_llvm_windows)
               flags = '/nofor-main'
           case (id_pgi,id_nvhpc)
               flags = '-Mnomain'
        end select

    case default
        error stop "Unknown language '"//language//'", try "fortran", "c", "c++"'
    end select

end subroutine get_main_flags

subroutine get_default_c_compiler(f_compiler, c_compiler)
    character(len=*), intent(in) :: f_compiler
    character(len=:), allocatable, intent(out) :: c_compiler
    integer(compiler_enum) :: id

    id = get_compiler_id(f_compiler)

    select case(id)

    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows)
        c_compiler = 'icc'

    case(id_intel_llvm_nix,id_intel_llvm_windows)
        c_compiler = 'icx'

    case(id_flang_classic, id_flang, id_f18 )
        c_compiler='clang'

    case(id_ibmxl)
        c_compiler='xlc'

    case(id_lfortran)
        c_compiler = 'cc'

    case(id_gcc)
        c_compiler = 'gcc'

    case(id_amdflang)
        c_compiler = 'amdclang'

    case default
        ! Fall-back to using Fortran compiler
        c_compiler = f_compiler
    end select

end subroutine get_default_c_compiler

!> Get C++ Compiler.
subroutine get_default_cxx_compiler(f_compiler, cxx_compiler)
    character(len=*), intent(in) :: f_compiler
    character(len=:), allocatable, intent(out) :: cxx_compiler
    integer(compiler_enum) :: id

    id = get_compiler_id(f_compiler)

    select case(id)

    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows)
        cxx_compiler = 'icpc'

    case(id_intel_llvm_nix,id_intel_llvm_windows)
        cxx_compiler = 'icpx'

    case(id_flang_classic, id_flang, id_f18)
        cxx_compiler='clang++'

    case(id_amdflang)
        cxx_compiler='amdclang++'

    case(id_ibmxl)
        cxx_compiler='xlc++'

    case(id_lfortran)
        cxx_compiler = 'cc'

    case(id_gcc)
        cxx_compiler = 'g++'

    case default
        ! Fall-back to using Fortran compiler
        cxx_compiler = f_compiler
    end select

end subroutine get_default_cxx_compiler

!> Check if C++ compiler is GNU-based by checking its version output
function is_cxx_gnu_based(self) result(is_gnu)
    class(compiler_t), intent(in) :: self
    logical :: is_gnu
    character(len=:), allocatable :: output_file, version_output
    integer :: stat, io

    is_gnu = .false.
    
    if (.not.allocated(self%cxx)) return
    if (len_trim(self%cxx)<=0) return

    ! Get temporary file for compiler version output
    output_file = get_temp_filename()

    ! Run compiler with --version to get version info
    call run(self%cxx//" --version > "//output_file//" 2>&1", &
             echo=.false., exitstat=stat)

    if (stat == 0) then
        ! Read the version output
        open(file=output_file, newunit=io, iostat=stat)
        if (stat == 0) then
            call getline(io, version_output, stat)
            close(io, iostat=stat)

            ! Check if output contains GNU indicators
            if (allocated(version_output)) then
                is_gnu = index(version_output, 'gcc') > 0 .or. &
                         index(version_output, 'GCC') > 0 .or. &
                         index(version_output, 'GNU') > 0 .or. &
                         index(version_output, 'Free Software Foundation') > 0
            end if
        end if
    end if

    ! Clean up temporary file
    call run("rm -f "//output_file, echo=.false., exitstat=stat)

end function is_cxx_gnu_based


function get_compiler_id(compiler) result(id)
    character(len=*), intent(in) :: compiler
    integer(kind=compiler_enum) :: id

    character(len=:), allocatable :: full_command, full_command_parts(:), command, output
    integer :: stat, io

    ! Check whether we are dealing with an MPI compiler wrapper first
    if (check_compiler(compiler, "mpifort") &
        & .or. check_compiler(compiler, "mpif90") &
        & .or. check_compiler(compiler, "mpif77")) then
        output = get_temp_filename()
        call run(compiler//" -show > "//output//" 2>&1", &
            & echo=.false., exitstat=stat)
        if (stat == 0) then
            open(file=output, newunit=io, iostat=stat)
            if (stat == 0) call getline(io, full_command, stat)
            close(io, iostat=stat)

            ! If we get a command from the wrapper, we will try to identify it
            call split(full_command, full_command_parts, delimiters=' ')
            if(size(full_command_parts) > 0)then
               command = trim(full_command_parts(1))
            endif
            if (allocated(command)) then
                id = match_compiler_type(command)
                if (id /= id_unknown) return
            end if
        end if
    end if

    id = match_compiler_type(compiler)

end function get_compiler_id

function match_compiler_type(compiler) result(id)
    character(len=*), intent(in) :: compiler
    integer(kind=compiler_enum) :: id

    if (check_compiler(compiler, "gfortran")) then
        id = id_gcc
        return
    end if

    if (check_compiler(compiler, "f95")) then
        id = id_f95
        return
    end if

    if (check_compiler(compiler, "caf")) then
        id = id_caf
        return
    end if

    if (check_compiler(compiler, "ifort")) then
        select case (get_os_type())
        case default
            id = id_intel_classic_nix
        case (OS_MACOS)
            id = id_intel_classic_mac
        case (OS_WINDOWS, OS_CYGWIN)
            id = id_intel_classic_windows
        end select
        return
    end if

    if (check_compiler(compiler, "ifx")) then
        select case (get_os_type())
        case default
            id = id_intel_llvm_nix
        case (OS_WINDOWS, OS_CYGWIN)
            id = id_intel_llvm_windows
        end select
        return
    end if

    if (check_compiler(compiler, "nvfortran")) then
        id = id_nvhpc
        return
    end if

    if (check_compiler(compiler, "pgfortran") &
        & .or. check_compiler(compiler, "pgf90") &
        & .or. check_compiler(compiler, "pgf95")) then
        id = id_pgi
        return
    end if

    if (check_compiler(compiler, "nagfor")) then
        id = id_nag
        return
    end if

    if (check_compiler(compiler, "flang-classic")) then
        id = id_flang_classic
        return
    end if

    if (check_compiler(compiler, "flang-new") .or. check_compiler(compiler, "flang")) then
        id = id_flang
        return
    end if

    if (check_compiler(compiler, "amdflang")) then
        id = id_amdflang
        return
    end if

    if (check_compiler(compiler, "f18")) then
        id = id_f18
        return
    end if

    if (check_compiler(compiler, "xlf90")) then
        id = id_ibmxl
        return
    end if

    if (check_compiler(compiler, "crayftn")) then
        id = id_cray
        return
    end if

    if (check_compiler(compiler, "lfc")) then
        id = id_lahey
        return
    end if

    if (check_compiler(compiler, "lfortran")) then
        id = id_lfortran
        return
    end if


    if (check_compiler(compiler, "all")) then
        id = id_all
        return
    end if

    id = id_unknown

end function match_compiler_type

!> Check if compiler name is a valid compiler name
pure elemental subroutine validate_compiler_name(compiler_name, is_valid)

    !> Name of a compiler
    character(len=*), intent(in) :: compiler_name

    !> Boolean value of whether compiler_name is valid or not
    logical, intent(out) :: is_valid
    
    character(:), allocatable :: lname
    
    lname = lower(compiler_name)
    
    select case (lname)
      case("gfortran", "ifort", "ifx", "pgfortran", &
           "nvfortran", "flang", "amdflang", "caf", &
           "f95", "lfortran", "lfc", "nagfor",&
           "crayftn", "xlf90", "ftn95", "all")
        is_valid = .true.
      case default
        is_valid = .false.
    end select
    
end subroutine validate_compiler_name


function check_compiler(compiler, expected) result(match)
    character(len=*), intent(in) :: compiler
    character(len=*), intent(in) :: expected
    logical :: match
    match = compiler == expected
    if (.not. match) then
        match = index(basename(compiler), expected) > 0
    end if
end function check_compiler


pure function is_unknown(self)
    class(compiler_t), intent(in) :: self
    logical :: is_unknown
    is_unknown = self%id == id_unknown
end function is_unknown

pure logical function is_intel(self)
    class(compiler_t), intent(in) :: self
    is_intel = any(self%id == [id_intel_classic_nix,id_intel_classic_mac,id_intel_classic_windows, &
                               id_intel_llvm_nix,id_intel_llvm_windows,id_intel_llvm_unknown])
end function is_intel

pure logical function is_gnu(self)
    class(compiler_t), intent(in) :: self
    is_gnu = any(self%id == [id_f95,id_gcc,id_caf])
end function is_gnu

!>
!> Enumerate libraries, based on compiler and platform
!>
function enumerate_libraries(self, prefix, libs) result(r)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: prefix
    type(string_t), intent(in) :: libs(:)
    character(len=:), allocatable :: r

    character(len=:), allocatable :: joined

    if (size(libs) == 0) then
        r = prefix
        return
    end if

    select case (self%id)

    case (id_intel_classic_windows, id_intel_llvm_windows)
        ! Windows Intel uses `.lib` files directly
        joined = string_cat(libs, ".lib ") // ".lib"
        r = trim(prefix) // " " // trim(joined)

    case (id_nag, id_ibmxl)
        ! NAG and IBMXL need -Wl, wrapper around linker flags
        joined = string_cat(libs, " -Wl,")
        r = trim(prefix) // " -Wl," // trim(joined)

    case default
        ! Generic Unix-style linker flags: use `-lfoo`
        joined = string_cat(libs, " -l")
        r = trim(prefix) // " -l" // trim(joined)

    end select

end function enumerate_libraries

!>
!> Generate library export flags for a shared library build
!>
function get_export_flags(self, target_dir, target_name) result(export_flags)
    !> Instance of the compiler
    class(compiler_t), intent(in) :: self
    !> Path and package name
    character(len=*), intent(in) :: target_dir, target_name
    character(len=:), allocatable :: export_flags

    character(len=:), allocatable :: implib_path, def_path

    ! Only apply on Windows
    if (get_os_type() /= OS_WINDOWS) then
        export_flags = ""
        return
    end if

    select case (self%id)

    case (id_gcc, id_caf, id_f95)
        ! GNU-based: emit both import library and def file
        implib_path = quote(join_path(target_dir, target_name // ".dll.a") , for_cmd=.true.)
        def_path    = quote(join_path(target_dir, target_name // ".def" ) , for_cmd=.true.)

        export_flags = " -Wl,--out-implib," // implib_path // &
                       " -Wl,--output-def," // def_path

    case (id_intel_classic_windows, id_intel_llvm_windows)
        ! Intel/MSVC-style
        implib_path = quote(join_path(target_dir, target_name // ".lib") , for_cmd=.true.)
        def_path    = quote(join_path(target_dir, target_name // ".def") , for_cmd=.true.)
                
        export_flags = " /IMPLIB:" // implib_path // &
                       " /DEF:" // def_path

    case default
        
        export_flags = ""  ! Do nothing elsewhere

    end select

end function get_export_flags

!>
!> Generate `install_name` flag for a shared library build on macOS
!>
function get_install_name_flags(self, target_dir, target_name) result(flags)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: target_dir, target_name
    character(len=:), allocatable :: flags
    character(len=:), allocatable :: library_file

    if (get_os_type() /= OS_MACOS) then
        flags = ""
        return
    end if

    ! Shared library basename (e.g., libfoo.dylib)
    if (str_ends_with(target_name, ".dylib")) then
        library_file = target_name        
    else
        library_file = library_filename(target_name,.true.,.false.,OS_MACOS)
    end if
    
    flags = " -Wl,-install_name,@rpath/" // library_file

end function get_install_name_flags

!>
!> Generate header padding flags for install_name_tool compatibility on macOS
!>
function get_headerpad_flags(self) result(flags)
    class(compiler_t), intent(in) :: self
    character(len=:), allocatable :: flags

    if (get_os_type() /= OS_MACOS) then
        flags = ""
        return
    end if

    ! Reserve enough space in the Mach-O header to safely add two install_name or rpath later
    flags = " -Wl,-headerpad,0x200"

end function get_headerpad_flags

!> Create new compiler instance
subroutine new_compiler(self, fc, cc, cxx, echo, verbose)
    !> New instance of the compiler
    type(compiler_t), intent(out) :: self
    !> Fortran compiler name or path
    character(len=*), intent(in) :: fc
    !> C compiler name or path
    character(len=*), intent(in) :: cc
    !> C++ Compiler name or path
    character(len=*), intent(in) :: cxx
    !> Echo compiler command
    logical, intent(in) :: echo
    !> Verbose mode: dump compiler output
    logical, intent(in) :: verbose

    self%id = get_compiler_id(fc)

    self%echo = echo
    self%verbose = verbose
    self%fc = fc
    if (len_trim(cc) > 0) then
      self%cc = cc
    else
      call get_default_c_compiler(self%fc, self%cc)
    end if

    if (len_trim(cxx) > 0) then
      self%cxx = cxx
    else
      call get_default_cxx_compiler(self%fc, self%cxx)
    end if

end subroutine new_compiler


!> Create new archiver instance
subroutine new_archiver(self, ar, echo, verbose)
    !> New instance of the archiver
    type(archiver_t), intent(out) :: self
    !> User provided archiver command
    character(len=*), intent(in) :: ar
    !> Echo compiler command
    logical, intent(in) :: echo
    !> Verbose mode: dump compiler output
    logical, intent(in) :: verbose

    integer :: estat, os_type

    character(len=*), parameter :: arflags = " -rs ", libflags = " /OUT:"

    if (len_trim(ar) > 0) then
      ! Check first for ar-like commands
      if (check_compiler(ar, "ar")) then
        self%ar = ar//arflags
      end if

      ! Check for lib-like commands
      if (check_compiler(ar, "lib")) then
        self%ar = ar//libflags
      end if

      ! Fallback and assume ar-like behaviour
      self%ar = ar//arflags
    else
      os_type = get_os_type()
      if (os_type /= OS_WINDOWS .and. os_type /= OS_UNKNOWN) then
        self%ar = "ar"//arflags
      else
        ! Attempt "ar"
        call execute_command_line("ar --version > "//get_temp_filename()//" 2>&1", &
          & exitstat=estat)

        if (estat == 0) then

            self%ar = "ar"//arflags

        else

            ! Then "gcc-ar"
            call execute_command_line("gcc-ar --version > "//get_temp_filename()//" 2>&1", &
               & exitstat=estat)

            if (estat /= 0) then
              self%ar = "lib"//libflags
            else
              self%ar = "gcc-ar"//arflags
            end if
        endif
      end if
    end if
    self%use_response_file = os_type == OS_WINDOWS
    self%echo = echo
    self%verbose = verbose
end subroutine new_archiver


!> Compile a Fortran object
subroutine compile_fortran(self, input, output, args, log_file, stat, table, dry_run)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Source file input
    character(len=*), intent(in) :: input
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat
    !> Optional compile_commands table
    type(compile_command_table_t), optional, intent(inout) :: table    
    !> Optional mocking
    logical, optional, intent(in) :: dry_run
    
    character(len=:), allocatable :: command 
    type(error_t), allocatable :: error
    logical :: mock
    
    ! Check if we're actually building this file
    mock = .false.
    if (present(dry_run)) mock = dry_run
    
    ! Set command
    command = self%fc // " -c " // input // " " // args // " -o " // output

    ! Execute command
    if (.not.mock) then 
       call run(command, echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
       if (stat/=0) return
    endif
        
    ! Optionally register compile command 
    if (present(table)) then 
        call table%register(command, get_os_type(), error)
        stat = merge(-1,0,allocated(error))
    endif    
        
end subroutine compile_fortran


!> Compile a C object
subroutine compile_c(self, input, output, args, log_file, stat, table, dry_run)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Source file input
    character(len=*), intent(in) :: input
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat
    !> Optional compile_commands table
    type(compile_command_table_t), optional, intent(inout) :: table    
    !> Optional mocking
    logical, optional, intent(in) :: dry_run    
    
    character(len=:), allocatable :: command 
    type(error_t), allocatable :: error
    logical :: mock
    
    ! Check if we're actually building this file
    mock = .false.
    if (present(dry_run)) mock = dry_run    
    
    ! Set command
    command = self%cc // " -c " // input // " " // args // " -o " // output

    ! Execute command
    if (.not.mock) then 
       call run(command, echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
       if (stat/=0) return
    endif
        
    ! Optionally register compile command 
    if (present(table)) then 
        call table%register(command, get_os_type(), error)
        stat = merge(-1,0,allocated(error))
    endif        
    
end subroutine compile_c

!> Compile a CPP object
subroutine compile_cpp(self, input, output, args, log_file, stat, table, dry_run)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Source file input
    character(len=*), intent(in) :: input
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat
    !> Optional compile_commands table
    type(compile_command_table_t), optional, intent(inout) :: table    
    !> Optional mocking
    logical, optional, intent(in) :: dry_run    
    
    character(len=:), allocatable :: command 
    type(error_t), allocatable :: error
    logical :: mock
        
    ! Check if we're actually building this file
    mock = .false.
    if (present(dry_run)) mock = dry_run        
        
    ! Set command
    command = self%cxx // " -c " // input // " " // args // " -o " // output

    ! Execute command
    if (.not.mock) then 
       call run(command, echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
       if (stat/=0) return
    endif
        
    ! Optionally register compile command 
    if (present(table)) then 
        call table%register(command, get_os_type(), error)
        stat = merge(-1,0,allocated(error))
    endif               
        
end subroutine compile_cpp

!> Link an executable
subroutine link_executable(self, output, args, log_file, stat, dry_run)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat
    !> Optional mocking
    logical, optional, intent(in) :: dry_run    
    
    character(len=:), allocatable :: command 
    logical :: mock
        
    ! Check if we're actually linking
    mock = .false.
    if (present(dry_run)) mock = dry_run                
        
    ! Set command
    command = self%fc // " " // args // " -o " // output    
    
    ! Execute command
    if (.not.mock) &
    call run(command, echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
    
end subroutine link_executable

!> Link a shared library
subroutine link_shared(self, output, args, log_file, stat, dry_run)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Output file of shared library object
    character(len=*), intent(in) :: output
    !> Arguments for the compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat
    !> Optional mocking
    logical, optional, intent(in) :: dry_run

    character(len=:), allocatable :: command
    logical :: mock
    character(len=:), allocatable :: shared_flag

    mock = .false.
    if (present(dry_run)) mock = dry_run

    shared_flag = get_shared_flag(self)

    command = self%fc // " " // shared_flag // " " // args // " -o " // output

    if (.not.mock) &
        call run(command, echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)

end subroutine link_shared


!> Create an archive
!> @todo For Windows OS, use the local `delete_file_win32` in stead of `delete_file`.
!> This may be related to a bug in Mingw64-openmp and is expected to be resolved in the future,
!> see issue #707, #708 and #808.
subroutine make_archive(self, output, args, log_file, stat, dry_run)
    !> Instance of the archiver object
    class(archiver_t), intent(in) :: self
    !> Name of the archive to generate
    character(len=*), intent(in) :: output
    !> Object files to include into the archive
    type(string_t), intent(in) :: args(:)
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat
    !> Optional mocking
    logical, optional, intent(in) :: dry_run    
    
    logical :: mock
        
    ! Check if we're actually linking
    mock = .false.
    if (present(dry_run)) mock = dry_run            
    
    if (mock) return

    if (self%use_response_file) then
        call write_response_file(output//".resp" , args)
        call run(self%ar // output // " @" // output//".resp", echo=self%echo, &
            &  verbose=self%verbose, redirect=log_file, exitstat=stat)
        call delete_file_win32(output//".resp")

    else
        call run(self%ar // output // " " // string_cat(args, " "), &
            & echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
    end if

    contains
        subroutine delete_file_win32(file)
            character(len=*), intent(in) :: file
            logical :: exist
            integer :: unit, iostat
            inquire(file=file, exist=exist)
            if (exist) then
                open(file=file, newunit=unit)
                close(unit, status='delete', iostat=iostat)
            end if
        end subroutine delete_file_win32
end subroutine make_archive


!> Response files allow to read command line options from files.
!> Whitespace is used to separate the arguments, we will use newlines
!> as separator to create readable response files which can be inspected
!> in case of errors.
subroutine write_response_file(name, argv)
    character(len=*), intent(in) :: name
    type(string_t), intent(in) :: argv(:)

    integer :: iarg, io

    open(file=name, newunit=io, status='replace')
    do iarg = 1, size(argv)
        write(io, '(a)') unix_path(argv(iarg)%s)
    end do
    close(io)
end subroutine write_response_file


!> String representation of a compiler object
pure function debug_compiler(self) result(repr)
    !> Instance of the compiler object
    type(compiler_t), intent(in) :: self
    !> Representation as string
    character(len=:), allocatable :: repr

    repr = 'fc="'//self%fc//'", cc="'//self%cc//'"'
end function debug_compiler


!> String representation of an archiver object
pure function debug_archiver(self) result(repr)
    !> Instance of the archiver object
    type(archiver_t), intent(in) :: self
    !> Representation as string
    character(len=:), allocatable :: repr

    repr = 'ar="'//self%ar//'"'
end function debug_archiver

!> Check that two archiver_t objects are equal
logical function ar_is_same(this,that)
    class(archiver_t), intent(in) :: this
    class(serializable_t), intent(in) :: that

    ar_is_same = .false.

    select type (other=>that)
       type is (archiver_t)
          if (allocated(this%ar).neqv.allocated(other%ar)) return
          if (allocated(this%ar)) then
            if (.not.(this%ar==other%ar)) return
          end if
          if (.not.(this%use_response_file.eqv.other%use_response_file)) return
          if (.not.(this%echo.eqv.other%echo)) return
          if (.not.(this%verbose.eqv.other%verbose)) return

       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    ar_is_same = .true.

end function ar_is_same

!> Dump dependency to toml table
subroutine dump_to_toml(self, table, error)

    !> Instance of the serializable object
    class(archiver_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    !> Path to archiver
    call set_string(table, "ar", self%ar, error, 'archiver_t')
    if (allocated(error)) return
    call set_value(table, "use-response-file", self%use_response_file, error, 'archiver_t')
    if (allocated(error)) return
    call set_value(table, "echo", self%echo, error, 'archiver_t')
    if (allocated(error)) return
    call set_value(table, "verbose", self%verbose, error, 'archiver_t')
    if (allocated(error)) return

end subroutine dump_to_toml

!> Read dependency from toml table (no checks made at this stage)
subroutine load_from_toml(self, table, error)

    !> Instance of the serializable object
    class(archiver_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call get_value(table, "ar", self%ar)

    call get_value(table, "use-response-file", self%use_response_file, error, 'archiver_t')
    if (allocated(error)) return
    call get_value(table, "echo", self%echo, error, 'archiver_t')
    if (allocated(error)) return
    call get_value(table, "verbose", self%verbose, error, 'archiver_t')
    if (allocated(error)) return

end subroutine load_from_toml

!> Check that two compiler_t objects are equal
logical function compiler_is_same(this,that)
    class(compiler_t), intent(in) :: this
    class(serializable_t), intent(in) :: that

    compiler_is_same = .false.

    select type (other=>that)
       type is (compiler_t)

          if (.not.(this%id==other%id)) return
          if (allocated(this%fc).neqv.allocated(other%fc)) return
          if (allocated(this%fc)) then
            if (.not.(this%fc==other%fc)) return
          end if
          if (allocated(this%cc).neqv.allocated(other%cc)) return
          if (allocated(this%cc)) then
            if (.not.(this%cc==other%cc)) return
          end if
          if (allocated(this%cxx).neqv.allocated(other%cxx)) return
          if (allocated(this%cxx)) then
            if (.not.(this%cxx==other%cxx)) return
          end if
          if (.not.(this%echo.eqv.other%echo)) return
          if (.not.(this%verbose.eqv.other%verbose)) return

       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    compiler_is_same = .true.

end function compiler_is_same

!> Dump dependency to toml table
subroutine compiler_dump(self, table, error)

    !> Instance of the serializable object
    class(compiler_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ierr

    call set_value(table, "id", self%id, error, 'compiler_t')
    if (allocated(error)) return
    call set_string(table, "fc", self%fc, error, 'compiler_t')
    if (allocated(error)) return
    call set_string(table, "cc", self%cc, error, 'compiler_t')
    if (allocated(error)) return
    call set_string(table, "cxx", self%cxx, error, 'compiler_t')
    if (allocated(error)) return
    call set_value(table, "echo", self%echo, error, 'compiler_t')
    if (allocated(error)) return
    call set_value(table, "verbose", self%verbose, error, 'compiler_t')
    if (allocated(error)) return

end subroutine compiler_dump

!> Read dependency from toml table (no checks made at this stage)
subroutine compiler_load(self, table, error)

    !> Instance of the serializable object
    class(compiler_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call get_value(table, "id", self%id, error, 'compiler_t')
    if (allocated(error)) return
    call get_value(table, "fc", self%fc)
    call get_value(table, "cc", self%cc)
    call get_value(table, "cxx", self%cxx)
    call get_value(table, "echo", self%echo, error, 'compiler_t')
    if (allocated(error)) return
    call get_value(table, "verbose", self%verbose, error, 'compiler_t')
    if (allocated(error)) return

end subroutine compiler_load

!> Return a compiler name string
pure function compiler_name(self) result(name)
   !> Instance of the compiler object
   class(compiler_t), intent(in) :: self
   !> Representation as string
   character(len=:), allocatable :: name
   name = compiler_id_name(self%id)
end function compiler_name

!> Convert compiler enum to name (reverse of match_compiler_type)
pure function compiler_id_name(id) result(name)
   integer(compiler_enum), intent(in) :: id
   character(len=:), allocatable :: name

   select case (id)
       case(id_gcc);                   name = "gfortran"
       case(id_f95);                   name = "f95"
       case(id_caf);                   name = "caf"
       case(id_intel_classic_nix);     name = "ifort"
       case(id_intel_classic_mac);     name = "ifort"
       case(id_intel_classic_windows); name = "ifort"
       case(id_intel_llvm_nix);        name = "ifx"
       case(id_intel_llvm_windows);    name = "ifx"
       case(id_intel_llvm_unknown);    name = "ifx"
       case(id_pgi);                   name = "pgfortran"
       case(id_nvhpc);                 name = "nvfortran"
       case(id_nag);                   name = "nagfor"
       case(id_flang_classic);         name = "flang-classic"
       case(id_flang);                 name = "flang"
       case(id_amdflang);              name = "amdflang"
       case(id_f18);                   name = "f18"
       case(id_ibmxl);                 name = "xlf90"
       case(id_cray);                  name = "crayftn"
       case(id_lahey);                 name = "lfc"
       case(id_lfortran);              name = "lfortran"
       case (id_all);                  name = "all"
       case default;                   name = "invalid/unknown"
   end select
end function compiler_id_name

!> Run a single-source Fortran program using the current compiler
!> Compile a Fortran object
logical function check_fortran_source_runs(self, input, compile_flags, link_flags) result(success)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Program Source
    character(len=*), intent(in) :: input
    !> Optional build and link flags
    character(len=*), optional, intent(in) :: compile_flags, link_flags

    integer :: stat,unit
    character(:), allocatable :: source,object,logf,exe,flags,ldflags

    success = .false.

    !> Create temporary source file
    exe    = get_temp_filename()
    source = exe//'.f90'
    object = exe//'.o'
    logf   = exe//'.log'
    open(newunit=unit, file=source, action='readwrite', iostat=stat)
    if (stat/=0) return

    !> Write contents
    write(unit,*) input
    close(unit)

    !> Get flags
    flags    = self%get_default_flags(release=.false.)
    ldflags  = self%get_default_flags(release=.false.)

    if (present(compile_flags)) flags = flags//" "//compile_flags
    if (present(link_flags)) ldflags = ldflags//" "//link_flags

    !> Intel: Needs -warn last for error on unknown command line arguments to work
    if (self%id == id_intel_llvm_nix) then
        flags = flags//" "//flag_intel_warn
        ldflags = ldflags//" "//flag_intel_warn
    elseif (self%id == id_intel_llvm_windows) then
        flags = flags//" "//flag_intel_warn_win
        ldflags = ldflags//" "//flag_intel_warn_win
    end if

    !> Compile and link program
    call self%compile_fortran(source, object, flags, logf, stat)
    if (stat==0) &
    call self%link(exe, ldflags//" "//object, logf, stat)

    !> Run and retrieve exit code
    if (stat==0) &
    call run(exe,echo=.false., exitstat=stat, verbose=.false., redirect=logf)

    !> Successful exit on 0 exit code
    success = stat==0

    !> Delete files
    open(newunit=unit, file=source, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=object, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=logf, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=exe, action='readwrite', iostat=stat)
    close(unit,status='delete')

end function check_fortran_source_runs

!> Check if the given C source code compiles, links, and runs successfully
logical function check_c_source_runs(self, input, compile_flags, link_flags) result(success)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> C program source
    character(len=*), intent(in) :: input
    !> Optional build and link flags
    character(len=*), optional, intent(in) :: compile_flags, link_flags
    integer :: stat,unit
    character(:), allocatable :: source,object,logf,exe,flags,ldflags
    
    success = .false.
    
    !> Create temporary source file
    exe    = get_temp_filename()
    source = exe//'.c'
    object = exe//'.o'
    logf   = exe//'.log'
    
    open(newunit=unit, file=source, action='readwrite', iostat=stat)
    if (stat/=0) return
    
    !> Write contents
    write(unit,'(a)') input
    close(unit)
    
    !> Get flags
    flags    = ""
    ldflags  = ""
    if (present(compile_flags)) flags = flags//" "//compile_flags
    if (present(link_flags)) ldflags = ldflags//" "//link_flags
    
    !> Compile
    call self%compile_c(source,object,flags,logf,stat,dry_run=.false.)
    if (stat/=0) return
    
    !> Link using C compiler for pure C programs
    call run(self%cc//" "//ldflags//" "//object//" -o "//exe, &
              echo=self%echo, verbose=self%verbose, redirect=logf, exitstat=stat)
    if (stat/=0) return
    
    !> Run
    call run(exe//" > "//logf//" 2>&1",echo=.false.,exitstat=stat)
    success = (stat == 0)
    
    !> Delete temporary files
    open(newunit=unit, file=source, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=object, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=logf, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=exe, action='readwrite', iostat=stat)
    close(unit,status='delete')
    
end function check_c_source_runs

!> Check if the given C++ source code compiles, links, and runs successfully
logical function check_cxx_source_runs(self, input, compile_flags, link_flags) result(success)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> C++ program source
    character(len=*), intent(in) :: input
    !> Optional build and link flags
    character(len=*), optional, intent(in) :: compile_flags, link_flags
    integer :: stat,unit
    character(:), allocatable :: source,object,logf,exe,flags,ldflags
    
    success = .false.
    
    !> Create temporary source file
    exe    = get_temp_filename()
    source = exe//'.cpp'
    object = exe//'.o'
    logf   = exe//'.log'
    
    open(newunit=unit, file=source, action='readwrite', iostat=stat)
    if (stat/=0) return
    
    !> Write contents
    write(unit,'(a)') input
    close(unit)
    
    !> Get flags
    flags    = ""
    ldflags  = ""
    if (present(compile_flags)) flags = flags//" "//compile_flags
    if (present(link_flags)) ldflags = ldflags//" "//link_flags
    
    !> Compile
    call self%compile_cpp(source,object,flags,logf,stat,dry_run=.false.)
    if (stat/=0) return
    
    !> Link using C++ compiler for pure C++ programs
    call run(self%cxx//" "//ldflags//" "//object//" -o "//exe, &
              echo=self%echo, verbose=self%verbose, redirect=logf, exitstat=stat)
    if (stat/=0) return
    
    !> Run
    call run(exe//" > "//logf//" 2>&1",echo=.false.,exitstat=stat)
    success = (stat == 0)
    
    !> Delete temporary files
    open(newunit=unit, file=source, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=object, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=logf, action='readwrite', iostat=stat)
    close(unit,status='delete')
    open(newunit=unit, file=exe, action='readwrite', iostat=stat)
    close(unit,status='delete')
    
end function check_cxx_source_runs

!> Check if the given C compile and/or link flags are accepted by the C compiler
logical function check_c_flags_supported(self, compile_flags, link_flags)
    class(compiler_t), intent(in) :: self
    character(len=*), optional, intent(in) :: compile_flags, link_flags
    
    ! Minimal C program that always compiles
    character(len=*), parameter :: hello_world_c = &
        "#include <stdio.h>" // new_line('a') // &
        "int main() { printf(""Hello, World!""); return 0; }"
    
    check_c_flags_supported = self%check_c_source_runs(hello_world_c, compile_flags, link_flags)
end function check_c_flags_supported

!> Check if the given C++ compile and/or link flags are accepted by the C++ compiler
logical function check_cxx_flags_supported(self, compile_flags, link_flags)
    class(compiler_t), intent(in) :: self
    character(len=*), optional, intent(in) :: compile_flags, link_flags
    
    ! Minimal C++ program that always compiles
    character(len=*), parameter :: hello_world_cxx = &
        "#include <cstdio>" // new_line('a') // &
        "int main() { printf(""Hello, World!""); return 0; }"
    
    check_cxx_flags_supported = self%check_cxx_source_runs(hello_world_cxx, compile_flags, link_flags)
end function check_cxx_flags_supported

!> Check if the given compile and/or link flags are accepted by the compiler
logical function check_flags_supported(self, compile_flags, link_flags)
    class(compiler_t), intent(in) :: self
    character(len=*), optional, intent(in) :: compile_flags, link_flags

    ! Minimal program that always compiles
    character(len=*), parameter :: hello_world = "print *, 'Hello, World!'; end"

    check_flags_supported = self%check_fortran_source_runs(hello_world, compile_flags, link_flags)

end function check_flags_supported

!> Check if the current compiler supports 128-bit real precision
logical function with_qp(self)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    with_qp = self%check_fortran_source_runs &
              ('if (selected_real_kind(33) == -1) stop 1; end')
end function with_qp

!> Check if the current compiler supports 80-bit "extended" real precision
logical function with_xdp(self)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    with_xdp = self%check_fortran_source_runs &
               ('if (any(selected_real_kind(18) == [-1, selected_real_kind(33)])) stop 1; end')
end function with_xdp

!> Append new flags to existing flags, removing duplicates and empty flags (string version)
subroutine append_clean_flags(flags, new_flags)
    character(:), intent(inout), allocatable :: flags
    character(*), intent(in) :: new_flags

    type(string_t), allocatable :: flags_array(:), new_flags_array(:)
    integer :: i

    call tokenize_flags(flags, flags_array)
    call tokenize_flags(new_flags, new_flags_array)

    call append_clean_flags_array(flags_array, new_flags_array)

    do i = 1, size(flags_array)
        flags = flags // " " // flags_array(i)%s
    end do
end subroutine append_clean_flags

!> Append new flags to existing flags, removing duplicates and empty flags (array version)
subroutine append_clean_flags_array(flags_array, new_flags_array)
    type(string_t), allocatable, intent(inout) :: flags_array(:)
    type(string_t), intent(in) :: new_flags_array(:)

    integer :: i

    do i = 1, size(new_flags_array)
        if (string_array_contains(new_flags_array(i)%s, flags_array)) cycle
        ! Filter out empty flags and arguments
        if (new_flags_array(i)%s == "") cycle
        if (trim(new_flags_array(i)%s) == "-l") cycle
        if (trim(new_flags_array(i)%s) == "-L") cycle
        if (trim(new_flags_array(i)%s) == "-I") cycle
        if (trim(new_flags_array(i)%s) == "-J") cycle
        if (trim(new_flags_array(i)%s) == "-M") cycle
        call add_strings(flags_array, new_flags_array(i))
    end do
end subroutine append_clean_flags_array

!> Tokenize a string into an array of compiler flags
subroutine tokenize_flags(flags, flags_array)
    character(*), intent(in) :: flags
    type(string_t), allocatable, intent(out) :: flags_array(:)
    character(len=:), allocatable :: flags_char_array(:)

    integer :: i
    logical :: success

    flags_char_array = sh_split(flags, join_spaced=.true., keep_quotes=.true., success=success)
    if (.not. success) then
        allocate(flags_array(0))
        return
    end if
    allocate(flags_array(size(flags_char_array)))
    do i = 1, size(flags_char_array)
        flags_array(i)%s = trim(adjustl(flags_char_array(i)))
    end do
end subroutine tokenize_flags

end module fpm_compiler
