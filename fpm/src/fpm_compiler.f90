module fpm_compiler
use fpm_model, only: fpm_model_t 
use fpm_filesystem, only: join_path
public  add_compile_flag_defaults

contains
subroutine add_compile_flag_defaults(build_name,compiler,model)
! Choose compile flags based on cli settings & manifest inputs
character(len=*),intent(in) :: build_name, compiler

type(fpm_model_t), intent(inout) :: model
! could just be a function to return a string instead of passing model 
! but likely to change other components like matching C compiler
     
character(len=:),allocatable :: fflags
character(len=:),allocatable :: module_path_switch

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

    select case(build_name//'_'//compiler)

    case('release_gfortran') 
       module_path_switch='-J '
       fflags='&
       & -O3&
       & -Wimplicit-interface&
       & -fPIC&
       & -fmax-errors=1&
       & -ffast-math&
       & -funroll-loops&
       & -fcoarray=single&
       &'
    case('debug_gfortran')
       module_path_switch='-J '
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

    case('release_f95')
       module_path_switch='-J '
       fflags='&
       & -O3&
       & -Wimplicit-interface&
       & -fPIC&
       & -fmax-errors=1&
       & -ffast-math&
       & -funroll-loops&
       &'
    case('debug_f95')
       module_path_switch='-J '
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

    case('release_nvfortran')
       module_path_switch='-module '
       fflags = '&
       & -Mbackslash&
       &'
    case('debug_nvfortran')
       module_path_switch='-module '
       fflags = '&
       & -Minform=inform&
       & -Mbackslash&
       & -g&
       & -Mbounds&
       & -Mchkptr&
       & -Mchkstk&
       & -traceback&
       &'

    case('release_ifort')
       module_path_switch='-module '
       fflags = '&
       & -fp-model precise&
       & -pc 64&
       & -align all&
       & -error-limit 1&
       & -reentrancy threaded&
       & -nogen-interfaces&
       & -assume byterecl&
       & -assume nounderscore&
       &'
    case('debug_ifort')
       module_path_switch='-module '
       fflags = '&
       & -warn all&
       & -check all&
       & -error-limit 1&
       & -O0&
       & -g&
       & -assume byterecl&
       & -traceback&
       &'
    case('release_ifx')
       module_path_switch='-module '
       fflags = ' '
    case('debug_ifx')
       module_path_switch='-module '
       fflags = ' '

    case('release_pgfortran','release_pgf90','release_pgf95')  ! Portland Group F90/F95 compilers
       module_path_switch='-module '
       fflags = ' '
    case('debug_pgfortran','debug_pgf90','debug_pgf95')  ! Portland Group F90/F95 compilers
       module_path_switch='-module '
       fflags = ' '

    case('release_flang')
       module_path_switch='-module '
       fflags = ' '
    case('debug_flang')
       module_path_switch='-module '
       fflags = ' '

    case('release_lfc')
       module_path_switch='-M '
       fflags = ' '
    case('debug_lfc')
       module_path_switch='-M '
       fflags = ' '

    case('release_nagfor')
       module_path_switch='-mdir '
       fflags = ' '
    case('debug_nagfor')
       module_path_switch='-mdir '
       fflags = ' '

    case('release_crayftn')
       module_path_switch='-J '
       fflags = ' '
    case('debug_crayftn')
       module_path_switch='-J '
       fflags = ' '

    case('release_xlf90')
       module_path_switch='-qmoddir '
       fflags = ' '
    case('debug_xlf90')
       module_path_switch='-qmoddir '
       fflags = ' '

    case default
       module_path_switch='-module '
       fflags = ' '
       write(*,*)'<WARNING> unknown compiler (',compiler,')'
       write(*,*)'          and build name   (',build_name,')'
       write(*,*)'          combination.'
       write(*,*)'          known compilers are gfortran, nvfortran, ifort'
    end select

! NOTE THAT MODULE_PATH_SWITCH IS ASSUMED TO CONTAIN REQUIRED TRAILING SPACE IF NEEDED
! so that values that do not require a space such as -moddir= will work
    model%fortran_compile_flags = fflags//' '//&
    & module_path_switch//join_path(model%output_directory,model%package_name)

end subroutine add_compile_flag_defaults

end module fpm_compiler
