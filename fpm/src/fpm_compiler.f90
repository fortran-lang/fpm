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
! LLVM flang        flang      clang   -module         -I            -mp        X
! LFortran          lfortran   ---     ?               ?             ?          X
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
use fpm_model, only: fpm_model_t
use fpm_filesystem, only: join_path, basename
implicit none
public :: is_unknown_compiler
public :: get_module_flags
public :: get_default_compile_flags
public :: get_debug_compile_flags
public :: get_release_compile_flags

enum, bind(C)
    enumerator :: &
        id_unknown, &
        id_gcc, &
        id_f95, &
        id_caf, &
        id_intel_classic, &
        id_intel_llvm, &
        id_pgi, &
        id_nvhpc, &
        id_nag, &
        id_flang, &
        id_ibmxl, &
        id_cray, &
        id_lahey, &
        id_lfortran
end enum
integer, parameter :: compiler_enum = kind(id_unknown)

contains

subroutine get_default_compile_flags(compiler, release, flags)
    character(len=*), intent(in) :: compiler
    logical, intent(in) :: release
    character(len=:), allocatable, intent(out) :: flags
    integer :: id

    id = get_compiler_id(compiler)
    if (release) then
        call get_release_compile_flags(id, flags)
    else
        call get_debug_compile_flags(id, flags)
    end if

end subroutine get_default_compile_flags

subroutine get_release_compile_flags(id, flags)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(out) :: flags

    select case(id)
    case default
        flags = ""

    case(id_caf)
        flags='&
            & -O3&
            & -Wimplicit-interface&
            & -fPIC&
            & -fmax-errors=1&
            & -funroll-loops&
            &'
    case(id_gcc)
        flags='&
            & -O3&
            & -Wimplicit-interface&
            & -fPIC&
            & -fmax-errors=1&
            & -funroll-loops&
            & -fcoarray=single&
            &'
    case(id_f95)
        flags='&
            & -O3&
            & -Wimplicit-interface&
            & -fPIC&
            & -fmax-errors=1&
            & -ffast-math&
            & -funroll-loops&
            &'
    case(id_nvhpc)
        flags = '&
            & -Mbackslash&
            &'
    case(id_intel_classic)
        flags = '&
            & -fp-model precise&
            & -pc 64&
            & -align all&
            & -error-limit 1&
            & -reentrancy threaded&
            & -nogen-interfaces&
            & -assume byterecl&
            &'
    case(id_nag)
        flags = ' &
            & -O4&
            & -coarray=single&
            & -PIC&
            &'
    end select
end subroutine get_release_compile_flags

subroutine get_debug_compile_flags(id, flags)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(out) :: flags

    select case(id)
    case default
        flags = ""

    case(id_caf)
        flags = '&
            & -Wall&
            & -Wextra&
            & -Wimplicit-interface&
            & -fPIC -fmax-errors=1&
            & -g&
            & -fcheck=bounds&
            & -fcheck=array-temps&
            & -fbacktrace&
            &'

    case(id_gcc)
        flags = '&
            & -Wall&
            & -Wextra&
            & -Wimplicit-interface&
            & -fPIC -fmax-errors=1&
            & -g&
            & -fcheck=bounds&
            & -fcheck=array-temps&
            & -fbacktrace&
            & -fcoarray=single&
            &'

    case(id_f95)
        flags = '&
            & -Wall&
            & -Wextra&
            & -Wimplicit-interface&
            & -fPIC -fmax-errors=1&
            & -g&
            & -fcheck=bounds&
            & -fcheck=array-temps&
            & -Wno-maybe-uninitialized -Wno-uninitialized&
            & -fbacktrace&
            &'

    case(id_nvhpc)
        flags = '&
            & -Minform=inform&
            & -Mbackslash&
            & -g&
            & -Mbounds&
            & -Mchkptr&
            & -Mchkstk&
            & -traceback&
            &'

    case(id_intel_classic)
        flags = '&
            & -warn all&
            & -check:all:noarg_temp_created&
            & -error-limit 1&
            & -O0&
            & -g&
            & -assume byterecl&
            & -traceback&
            &'

    case(id_nag)
        flags = '&
            & -g&
            & -C=all&
            & -O0&
            & -gline&
            & -coarray=single&
            & -PIC&
            &'
    end select
end subroutine get_debug_compile_flags

subroutine get_module_flags(compiler, modpath, flags)
    character(len=*), intent(in) :: compiler
    character(len=*), intent(in) :: modpath
    character(len=:), allocatable, intent(out) :: flags
    integer(compiler_enum) :: id

    id = get_compiler_id(compiler)

    select case(id)
    case default
        flags=' -module '//modpath//' -I '//modpath

    case(id_caf, id_gcc, id_f95, id_cray)
        flags=' -J '//modpath//' -I '//modpath

    case(id_intel_classic, id_intel_llvm, id_nvhpc, id_pgi, id_flang)
        flags=' -module '//modpath//' -I '//modpath

    case(id_lahey)
        flags=' -M '//modpath//' -I '//modpath

    case(id_nag)
        flags=' -mdir '//modpath//' -I '//modpath !

    case(id_ibmxl)
        flags=' -qmoddir '//modpath//' -I '//modpath

    end select

end subroutine get_module_flags

function get_compiler_id(compiler) result(id)
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
        id = id_intel_classic
        return
    end if

    if (check_compiler(compiler, "ifx")) then
        id = id_intel_llvm
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

    if (check_compiler(compiler, "flang")) then
        id = id_flang
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

    if (check_compiler(compiler, "lfort")) then
        id = id_lfortran
        return
    end if

    id = id_unknown

end function get_compiler_id

function check_compiler(compiler, expected) result(match)
    character(len=*), intent(in) :: compiler
    character(len=*), intent(in) :: expected
    logical :: match
    match = compiler == expected
    if (.not. match) then
        match = index(basename(compiler), expected) > 0
    end if
end function check_compiler

function is_unknown_compiler(compiler) result(is_unknown)
    character(len=*), intent(in) :: compiler
    logical :: is_unknown
    is_unknown = get_compiler_id(compiler) == id_unknown
end function is_unknown_compiler

end module fpm_compiler
