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
use fpm_environment, only: &
        run, &
        get_os_type, &
        OS_LINUX, &
        OS_MACOS, &
        OS_WINDOWS, &
        OS_CYGWIN, &
        OS_SOLARIS, &
        OS_FREEBSD, &
        OS_OPENBSD, &
        OS_UNKNOWN
use fpm_filesystem, only: join_path, basename, get_temp_filename, delete_file, unix_path
use fpm_strings, only: string_cat, string_t
implicit none
public :: is_unknown_compiler
public :: get_module_flags
public :: get_default_compile_flags
public :: get_debug_compile_flags
public :: get_release_compile_flags

public :: compiler_t, archiver_t
public :: debug

enum, bind(C)
    enumerator :: &
        id_unknown, &
        id_gcc, &
        id_f95, &
        id_caf, &
        id_intel_classic_nix, &
        id_intel_classic_mac, &
        id_intel_classic_windows, &
        id_intel_classic_unknown, &
        id_intel_llvm_nix, &
        id_intel_llvm_windows, &
        id_intel_llvm_unknown, &
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


!> Definition of compiler object
type :: compiler_t
    !> Path to the Fortran compiler
    character(len=:), allocatable :: fc
    !> Path to the C compiler
    character(len=:), allocatable :: cc
    !> Print all commands
    logical :: echo = .true.
contains
    !> Compile a Fortran object
    procedure :: compile_fortran
    !> Compile a C object
    procedure :: compile_c
    !> Link executable
    procedure :: link
end type compiler_t


!> Definition of archiver object
type :: archiver_t
    !> Path to archiver
    character(len=:), allocatable :: ar
    !> Use response files to pass arguments
    logical :: use_response_file = .false.
    !> Print all command
    logical :: echo = .true.
contains
    !> Create static archive
    procedure :: make_archive
end type archiver_t


!> Constructor for archiver
interface archiver_t
    module procedure :: new_archiver
end interface archiver_t


!> Create debug printout
interface debug
    module procedure :: debug_compiler
    module procedure :: debug_archiver
end interface debug


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
    case(id_intel_classic_nix, id_intel_classic_unknown)
        flags = '&
            & -fp-model precise&
            & -pc64&
            & -align all&
            & -error-limit 1&
            & -reentrancy threaded&
            & -nogen-interfaces&
            & -assume byterecl&
            &'
    case(id_intel_classic_mac)
        flags = '&
            & -fp-model precise&
            & -pc64&
            & -align all&
            & -error-limit 1&
            & -reentrancy threaded&
            & -nogen-interfaces&
            & -assume byterecl&
            &'
    case(id_intel_classic_windows)
        flags = '&
            & /fp:precise&
            & /align:all&
            & /error-limit:1&
            & /reentrancy:threaded&
            & /nogen-interfaces&
            & /assume:byterecl&
            &'
    case(id_intel_llvm_nix, id_intel_llvm_unknown)
        flags = '&
            & -fp-model=precise&
            & -pc64&
            & -align all&
            & -error-limit 1&
            & -reentrancy threaded&
            & -nogen-interfaces&
            & -assume byterecl&
            &'
    case(id_intel_llvm_windows)
        flags = '&
            & /fp:precise&
            & /align:all&
            & /error-limit:1&
            & /reentrancy:threaded&
            & /nogen-interfaces&
            & /assume:byterecl&
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
    case(id_intel_classic_nix, id_intel_classic_unknown)
        flags = '&
            & -warn all&
            & -check all&
            & -error-limit 1&
            & -O0&
            & -g&
            & -assume byterecl&
            & -traceback&
            &'
    case(id_intel_classic_mac)
        flags = '&
            & -warn all&
            & -check all&
            & -error-limit 1&
            & -O0&
            & -g&
            & -assume byterecl&
            & -traceback&
            &'
    case(id_intel_classic_windows)
        flags = '&
            & /warn:all&
            & /check:all&
            & /error-limit:1&
            & /Od&
            & /Z7&
            & /assume:byterecl&
            & /traceback&
            &'
    case(id_intel_llvm_nix, id_intel_llvm_unknown)
        flags = '&
            & -warn all&
            & -check all&
            & -error-limit 1&
            & -O0&
            & -g&
            & -assume byterecl&
            & -traceback&
            &'
    case(id_intel_llvm_windows)
        flags = '&
            & /warn:all&
            & /check:all&
            & /error-limit:1&
            & /Od&
            & /Z7&
            & /assume:byterecl&
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

    case(id_nvhpc, id_pgi, id_flang)
        flags=' -module '//modpath//' -I '//modpath

    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_unknown, id_intel_llvm_nix, id_intel_llvm_unknown)
        flags=' -module '//modpath//' -I'//modpath

    case(id_intel_classic_windows, id_intel_llvm_windows)
        flags=' /module:'//modpath//' /I'//modpath

    case(id_lahey)
        flags=' -M '//modpath//' -I '//modpath

    case(id_nag)
        flags=' -mdir '//modpath//' -I '//modpath !

    case(id_ibmxl)
        flags=' -qmoddir '//modpath//' -I '//modpath

    end select

end subroutine get_module_flags

subroutine get_default_c_compiler(f_compiler, c_compiler)
    character(len=*), intent(in) :: f_compiler
    character(len=:), allocatable, intent(out) :: c_compiler
    integer(compiler_enum) :: id

    id = get_compiler_id(f_compiler)

    select case(id)

    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows, id_intel_classic_unknown)
        c_compiler = 'icc'

    case(id_intel_llvm_nix,id_intel_llvm_windows, id_intel_llvm_unknown)
        c_compiler = 'icx'

    case(id_flang)
        c_compiler='clang'

    case(id_ibmxl)
        c_compiler='xlc'

    case default
        ! Fall-back to using Fortran compiler
        c_compiler = f_compiler
    end select

end subroutine get_default_c_compiler

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
        select case (get_os_type())
        case (OS_LINUX, OS_SOLARIS, OS_FREEBSD)
            id = id_intel_classic_nix
        case (OS_MACOS)
            id = id_intel_classic_mac
        case (OS_WINDOWS, OS_CYGWIN)
            id = id_intel_classic_windows
        case default
            id = id_intel_classic_unknown
        end select
        return
    end if

    if (check_compiler(compiler, "ifx")) then
        select case (get_os_type())
        case (OS_LINUX, OS_SOLARIS, OS_FREEBSD)
            id = id_intel_llvm_nix
        case (OS_WINDOWS, OS_CYGWIN)
            id = id_intel_llvm_windows
        case default
            id = id_intel_llvm_unknown
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


!> Create new archiver
function new_archiver() result(self)
    !> New instance of the archiver
    type(archiver_t) :: self
    integer :: estat, os_type

    os_type = get_os_type()
    if (os_type /= OS_WINDOWS .and. os_type /= OS_UNKNOWN) then
        self%ar = "ar -rs "
    else
        call execute_command_line("ar --version > "//get_temp_filename()//" 2>&1", &
            & exitstat=estat)
        if (estat /= 0) then
            self%ar = "lib /OUT:"
        else
            self%ar = "ar -rs "
        end if
    end if
    self%use_response_file = os_type == OS_WINDOWS
    self%echo = .true.
end function new_archiver


!> Compile a Fortran object
subroutine compile_fortran(self, input, output, args, stat)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Source file input
    character(len=*), intent(in) :: input
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Status flag
    integer, intent(out) :: stat

    call run(self%fc // " -c " // input // " " // args // " -o " // output, &
        & echo=self%echo, exitstat=stat)
end subroutine compile_fortran


!> Compile a C object
subroutine compile_c(self, input, output, args, stat)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Source file input
    character(len=*), intent(in) :: input
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Status flag
    integer, intent(out) :: stat

    call run(self%cc // " -c " // input // " " // args // " -o " // output, &
        & echo=self%echo, exitstat=stat)
end subroutine compile_c


!> Link an executable
subroutine link(self, output, args, stat)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Status flag
    integer, intent(out) :: stat

    call run(self%fc // " " // args // " -o " // output, echo=self%echo, exitstat=stat)
end subroutine link


!> Create an archive
subroutine make_archive(self, output, args, stat)
    !> Instance of the archiver object
    class(archiver_t), intent(in) :: self
    !> Name of the archive to generate
    character(len=*), intent(in) :: output
    !> Object files to include into the archive
    type(string_t), intent(in) :: args(:)
    !> Status flag
    integer, intent(out) :: stat

    if (self%use_response_file) then
        call write_response_file(output//".resp" , args)
        call run(self%ar // output // " @" // output//".resp", echo=self%echo, exitstat=stat)
        call delete_file(output//".resp")
    else
        call run(self%ar // output // " " // string_cat(args, " "), &
            & echo=self%echo, exitstat=stat)
    end if
end subroutine make_archive


!> Response files allow to read command line options from files.
!> Whitespace is used to separate the arguments, we will use newlines
!> as separator to create readable response files which can be inspected
!> in case of errors.
subroutine write_response_file(name, argv)
    character(len=*), intent(in) :: name
    type(string_t), intent(in) :: argv(:)

    integer :: iarg, io

    open(file=name, newunit=io)
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


end module fpm_compiler
