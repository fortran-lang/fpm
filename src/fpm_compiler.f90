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
use fpm_environment, only: &
        run, &
        get_env, &
        get_os_type, &
        OS_LINUX, &
        OS_MACOS, &
        OS_WINDOWS, &
        OS_CYGWIN, &
        OS_SOLARIS, &
        OS_FREEBSD, &
        OS_OPENBSD, &
        OS_UNKNOWN
use fpm_filesystem, only: join_path, basename, get_temp_filename, delete_file, unix_path, &
    & getline
use fpm_strings, only: string_cat, string_t
implicit none
public :: compiler_t, new_compiler, archiver_t, new_archiver
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
    !> Identifier of the compiler
    integer(compiler_enum) :: id = id_unknown
    !> Path to the Fortran compiler
    character(len=:), allocatable :: fc
    !> Path to the C compiler
    character(len=:), allocatable :: cc
    !> Print all commands
    logical :: echo = .true.
contains
    !> Get default compiler flags
    procedure :: get_default_flags
    !> Get flag for module output directories
    procedure :: get_module_flag
    !> Get flag for include directories
    procedure :: get_include_flag
    !> Compile a Fortran object
    procedure :: compile_fortran
    !> Compile a C object
    procedure :: compile_c
    !> Link executable
    procedure :: link
    !> Check whether compiler is recognized
    procedure :: is_unknown
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
    flag_gnu_warn = " -Wall -Wextra -Wimplicit-interface", &
    flag_gnu_check = " -fcheck=bounds -fcheck=array-temps", &
    flag_gnu_limit = " -fmax-errors=1", &
    flag_gnu_external = " -Wimplicit-interface"

character(*), parameter :: &
    flag_pgi_backslash = " -Mbackslash", &
    flag_pgi_traceback = " -traceback", &
    flag_pgi_debug = " -g", &
    flag_pgi_check = " -Mbounds -Mchkptr -Mchkstk", &
    flag_pgi_warn = " -Minform=inform"

character(*), parameter :: &
    flag_intel_backtrace = " -traceback", &
    flag_intel_warn = " -warn all", &
    flag_intel_check = " -check all", &
    flag_intel_debug = " -O0 -g", &
    flag_intel_fp = " -fp-model precise -pc64", &
    flag_intel_align = " -align all", &
    flag_intel_limit = " -error-limit 1", &
    flag_intel_pthread = " -reentrancy threaded", &
    flag_intel_nogen = " -nogen-interfaces", &
    flag_intel_byterecl = " -assume byterecl"

character(*), parameter :: &
    flag_intel_backtrace_win = " /traceback", &
    flag_intel_warn_win = " /warn:all", &
    flag_intel_check_win = " /check:all", &
    flag_intel_debug_win = " /Od /Z7", &
    flag_intel_fp_win = " /fp-model:precise", &
    flag_intel_align_win = " /align:all", &
    flag_intel_limit_win = " /error-limit:1", &
    flag_intel_pthread_win = " /reentrancy:threaded", &
    flag_intel_nogen_win = " /nogen-interfaces", &
    flag_intel_byterecl_win = " /assume:byterecl"

character(*), parameter :: &
    flag_nag_coarray = " -coarray=single", &
    flag_nag_pic = " -PIC", &
    flag_nag_check = " -C=all", &
    flag_nag_debug = " -g -O0", &
    flag_nag_opt = " -O4", &
    flag_nag_backtrace = " -gline"

contains


function get_default_flags(self, release) result(flags)
    class(compiler_t), intent(in) :: self
    logical, intent(in) :: release
    character(len=:), allocatable :: flags

    if (release) then
        call get_release_compile_flags(self%id, flags)
    else
        call get_debug_compile_flags(self%id, flags)
    end if

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

    case(id_intel_classic_nix)
        flags = &
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_classic_mac)
        flags = &
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_classic_windows)
        flags = &
            & flag_intel_fp_win//&
            flag_intel_align_win//&
            flag_intel_limit_win//&
            flag_intel_pthread_win//&
            flag_intel_nogen_win//&
            flag_intel_byterecl_win

    case(id_intel_llvm_nix)
        flags = &
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_llvm_windows)
        flags = &
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
        flags = ""
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
            flag_intel_warn//&
            flag_intel_check//&
            flag_intel_limit//&
            flag_intel_debug//&
            flag_intel_byterecl//&
            flag_intel_backtrace
    case(id_intel_llvm_windows)
        flags = &
            flag_intel_warn_win//&
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
    end select
end subroutine get_debug_compile_flags

function get_include_flag(self, path) result(flags)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: flags

    select case(self%id)
    case default
        flags = "-I "//path

    case(id_caf, id_gcc, id_f95, id_cray, id_nvhpc, id_pgi, id_flang, &
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

    case(id_nvhpc, id_pgi, id_flang)
        flags = "-module "//path

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
    flags = flags//" "//self%get_include_flag(path)

end function get_module_flag


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

    case(id_flang)
        c_compiler='clang'

    case(id_ibmxl)
        c_compiler='xlc'

    case(id_lfortran)
        c_compiler = 'cc'

    case(id_gcc)
        c_compiler = 'gcc'

    case default
        ! Fall-back to using Fortran compiler
        c_compiler = f_compiler
    end select

end subroutine get_default_c_compiler


function get_compiler_id(compiler) result(id)
    character(len=*), intent(in) :: compiler
    integer(kind=compiler_enum) :: id

    character(len=:), allocatable :: command, output
    integer :: stat, io

    ! Check whether we are dealing with an MPI compiler wrapper first
    if (check_compiler(compiler, "mpifort") &
        & .or. check_compiler(compiler, "mpif90") &
        & .or. check_compiler(compiler, "mpif77")) then
        output = get_temp_filename()
        call run(compiler//" -showme:command > "//output//" 2>&1", &
            & echo=.false., exitstat=stat)
        if (stat == 0) then
            open(file=output, newunit=io, iostat=stat)
            if (stat == 0) call getline(io, command, stat)
            close(io, iostat=stat)

            ! If we get a command from the wrapper, we will try to identify it
            if (allocated(command)) then
                id = get_id(command)
                if (id /= id_unknown) return
            end if
        end if
    end if

    id = get_id(compiler)

end function get_compiler_id

function get_id(compiler) result(id)
    character(len=*), intent(in) :: compiler
    integer(kind=compiler_enum) :: id

    integer :: stat

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

    if (check_compiler(compiler, "lfortran")) then
        id = id_lfortran
        return
    end if

    id = id_unknown

end function get_id

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


!> Create new compiler instance
subroutine new_compiler(self, fc)
    !> Fortran compiler name or path
    character(len=*), intent(in) :: fc
    !> New instance of the compiler
    type(compiler_t), intent(out) :: self

    character(len=*), parameter :: cc_env = "FPM_C_COMPILER"

    self%id = get_compiler_id(fc)

    self%fc = fc
    call get_default_c_compiler(self%fc, self%cc)
    self%cc = get_env(cc_env, self%cc)
end subroutine new_compiler


!> Create new archiver instance
subroutine new_archiver(self)
    !> New instance of the archiver
    type(archiver_t), intent(out) :: self
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
end subroutine new_archiver


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
