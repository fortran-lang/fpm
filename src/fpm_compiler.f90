!># Define compiler command options
!!
!! This module defines compiler options to use for the debug and release builds.
module fpm_compiler
use fpm_model, only: fpm_model_t
use fpm_filesystem, only: join_path
public  add_compile_flag_defaults

contains
!> Choose compile flags based on cli settings & manifest inputs
subroutine add_compile_flag_defaults(build_name,compiler,model)
character(len=*),intent(in) :: build_name   !! select build from {release,debug}
character(len=*),intent(in) :: compiler     !! compiler name
type(fpm_model_t), intent(inout) :: model   !! model to add compiler options to

! could just be a function to return a string instead of passing model
! but likely to change other components like matching C compiler

character(len=:),allocatable :: fflags    ! optional flags that might be overridden by user
character(len=:),allocatable :: modpath 
character(len=:),allocatable :: mandatory ! flags required for fpm to function properly;
                                          ! ie. add module path and module include directory as appropriate

! special reserved names "debug" and "release" are for supported compilers with no user-specified compile or load flags

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
character(len=*),parameter :: names(*)=[ character(len=10) :: &
& 'caf', &
& 'gfortran', &
& 'f95', &
& 'nvfortran', &
& 'ifort', &
& 'ifx', &
& 'pgfortran', &
& 'pgf90', &
& 'pgf95', &
& 'flang', &
& 'lfc', &
& 'nagfor', &
& 'crayftn', &
& 'xlf90', &
& 'unknown']
integer :: i

    modpath=join_path(model%output_directory,model%package_name)
    fflags=''
    mandatory=''

    select case(build_name//'_'//compiler)

    case('release_caf')
       fflags='&
       & -O3&
       & -Wimplicit-interface&
       & -fPIC&
       & -fmax-errors=1&
       & -funroll-loops&
       &'
       mandatory=' -J '//modpath//' -I '//modpath 
    case('debug_caf')
       fflags = '&
       & -Wall&
       & -Wextra&
       & -Wimplicit-interface&
       & -fPIC -fmax-errors=1&
       & -g&
       & -fbounds-check&
       & -fcheck-array-temporaries&
       & -fbacktrace&
       &'
       mandatory=' -J '//modpath//' -I '//modpath 
    case('release_gfortran')
       fflags='&
       & -O3&
       & -Wimplicit-interface&
       & -fPIC&
       & -fmax-errors=1&
       & -funroll-loops&
       & -fcoarray=single&
       &'
       mandatory=' -J '//modpath//' -I '//modpath 
    case('debug_gfortran')
       fflags = '&
       & -Wall&
       & -Wextra&
       & -Wimplicit-interface&
       & -fPIC -fmax-errors=1&
       & -g&
       & -fbounds-check&
       & -fcheck-array-temporaries&
       & -fbacktrace&
       & -fcoarray=single&
       &'
       mandatory=' -J '//modpath//' -I '//modpath 

    case('release_f95')
       fflags='&
       & -O3&
       & -Wimplicit-interface&
       & -fPIC&
       & -fmax-errors=1&
       & -ffast-math&
       & -funroll-loops&
       &'
       mandatory=' -J '//modpath//' -I '//modpath 
    case('debug_f95')
       fflags = '&
       & -Wall&
       & -Wextra&
       & -Wimplicit-interface&
       & -fPIC -fmax-errors=1&
       & -g&
       & -fbounds-check&
       & -fcheck-array-temporaries&
       & -Wno-maybe-uninitialized -Wno-uninitialized&
       & -fbacktrace&
       &'
       mandatory=' -J '//modpath//' -I '//modpath 

    case('release_nvfortran')
       fflags = '&
       & -Mbackslash&
       &'
       mandatory=' -module '//modpath//' -I '//modpath 
    case('debug_nvfortran')
       fflags = '&
       & -Minform=inform&
       & -Mbackslash&
       & -g&
       & -Mbounds&
       & -Mchkptr&
       & -Mchkstk&
       & -traceback&
       &'
       mandatory=' -module '//modpath//' -I '//modpath 

    case('release_ifort')
       fflags = '&
       & -fp-model precise&
       & -pc 64&
       & -align all&
       & -error-limit 1&
       & -reentrancy threaded&
       & -nogen-interfaces&
       & -assume byterecl&
       &'
       mandatory=' -module '//modpath//' -I '//modpath 
    case('debug_ifort')
       fflags = '&
       & -warn all&
       & -check:all:noarg_temp_created&
       & -error-limit 1&
       & -O0&
       & -g&
       & -assume byterecl&
       & -traceback&
       &'
       mandatory=' -module '//modpath//' -I '//modpath 
    case('release_ifx')
       fflags = ' '
       mandatory=' -module '//modpath//' -I '//modpath 
    case('debug_ifx')
       fflags = ' '
       mandatory=' -module '//modpath//' -I '//modpath 

    case('release_pgfortran','release_pgf90','release_pgf95')  ! Portland Group F90/F95 compilers
       fflags = ' '
       mandatory=' -module '//modpath//' -I '//modpath 
    case('debug_pgfortran','debug_pgf90','debug_pgf95')  ! Portland Group F90/F95 compilers
       fflags = ' '
       mandatory=' -module '//modpath//' -I '//modpath 

    case('release_flang')
       fflags = ' '
       mandatory=' -module '//modpath//' -I '//modpath 
    case('debug_flang')
       fflags = ' '
       mandatory=' -module '//modpath//' -I '//modpath 

    case('release_lfc')
       fflags = ' '
       mandatory=' -M '//modpath//' -I '//modpath 
    case('debug_lfc')
       fflags = ' '
       mandatory=' -M '//modpath//' -I '//modpath 

    case('release_nagfor')
       fflags = ' &
       & -O4&
       & -coarray=single&
       & -PIC&
       &'
       mandatory=' -mdir '//modpath//' -I '//modpath !
    case('debug_nagfor')
       fflags = '&
       & -g&
       & -C=all&
       & -O0&
       & -gline&
       & -coarray=single&
       & -PIC&
       &'
       mandatory=' -mdir '//modpath//' -I '//modpath !
    case('release_crayftn')
       fflags = ' '
       mandatory=' -J '//modpath//' -I '//modpath 
    case('debug_crayftn')
       fflags = ' '
       mandatory=' -J '//modpath//' -I '//modpath 

    case('release_xlf90')
       fflags = ' '
       mandatory=' -qmoddir '//modpath//' -I '//modpath 
    case('debug_xlf90')
       fflags = ' '
       mandatory=' -qmoddir '//modpath//' -I '//modpath 

    case default
       fflags = ' '
       mandatory=' -module '//modpath//' -I '//modpath 
       write(*,'(*(a))')'<WARNING> unknown compiler (',compiler,') and build name (',build_name,') combination.'
       write(*,'(a,*(T31,6(a:,", "),/))')'          known compilers are ',(trim(names(i)),i=1,size(names)-1)
    end select

    model%fortran_compile_flags = fflags//' '//mandatory
     
end subroutine add_compile_flag_defaults

end module fpm_compiler
