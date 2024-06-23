!># Definition of the command line interface
!>
!> This module uses [M_CLI2](https://github.com/urbanjost/M_CLI2) to define
!> the command line interface.
!> To define a command line interface create a new command settings type
!> from the [[fpm_cmd_settings]] base class or the respective parent command
!> settings.
!>
!> The subcommand is selected by the first non-option argument in the command
!> line. In the subcase block the actual command line is defined and transferred
!> to an instance of the [[fpm_cmd_settings]], the actual type is used by the
!> *fpm* main program to determine which command entry point is chosen.
!>
!> To add a new subcommand add a new case to select construct and specify the
!> wanted command line and the expected default values.
!> Some of the following points also apply if you add a new option or argument
!> to an existing *fpm* subcommand.
!> At this point you should create a help page for the new command in a simple
!> catman-like format as well in the ``set_help`` procedure.
!> Make sure to register new subcommands in the ``fpm-manual`` command by adding
!> them to the manual character array and in the help/manual case as well.
!> You should add the new command to the synopsis section of the ``fpm-list``,
!> ``fpm-help`` and ``fpm --list`` help pages below to make sure the help output
!> is complete and consistent as well.
module fpm_command_line
use fpm_environment,  only : get_os_type, get_env, &
                             OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD, OS_NAME
use M_CLI2,           only : set_args, lget, sget, unnamed, remaining, specified
use M_CLI2,           only : get_subcommand, CLI_RESPONSE_FILE
use fpm_strings,      only : lower, split, to_fortran_name, is_fortran_name, remove_characters_in_set, &
                             string_t, glob
use fpm_filesystem,   only : basename, canon_path, which, run
use fpm_environment,  only : get_command_arguments_quoted
use fpm_error,        only : fpm_stop, error_t
use fpm_os,           only : get_current_directory
use fpm_release,      only : fpm_version, version_t
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit

implicit none

private
public :: fpm_cmd_settings, &
          fpm_build_settings, &
          fpm_install_settings, &
          fpm_export_settings, &
          fpm_new_settings, &
          fpm_run_settings, &
          fpm_test_settings, &
          fpm_update_settings, &
          fpm_clean_settings, &
          fpm_publish_settings, &
          get_command_line_settings, &
          get_fpm_env

type, abstract :: fpm_cmd_settings
    character(len=:), allocatable :: working_dir
    logical                       :: verbose=.true.
end type

integer,parameter :: ibug=4096

type, extends(fpm_cmd_settings)  :: fpm_new_settings
    character(len=:),allocatable :: name
    logical                      :: with_executable=.false.
    logical                      :: with_test=.false.
    logical                      :: with_lib=.true.
    logical                      :: with_example=.false.
    logical                      :: with_full=.false.
    logical                      :: with_bare=.false.
    logical                      :: backfill=.true.
end type

type, extends(fpm_cmd_settings)  :: fpm_build_settings
    logical                      :: list=.false.
    logical                      :: show_model=.false.
    logical                      :: build_tests=.false.
    logical                      :: prune=.true.
    character(len=:),allocatable :: dump
    character(len=:),allocatable :: compiler
    character(len=:),allocatable :: c_compiler
    character(len=:),allocatable :: cxx_compiler
    character(len=:),allocatable :: archiver
    character(len=:),allocatable :: profile
    character(len=:),allocatable :: flag
    character(len=:),allocatable :: cflag
    character(len=:),allocatable :: cxxflag
    character(len=:),allocatable :: ldflag
end type

type, extends(fpm_build_settings)  :: fpm_run_settings
    character(len=ibug),allocatable :: name(:)
    character(len=:),allocatable :: args ! passed to the app
    character(len=:),allocatable :: runner
    character(len=:),allocatable :: runner_args ! passed to the runner
    logical :: example
    contains
       procedure :: runner_command
       procedure :: name_ID
end type

type, extends(fpm_run_settings)  :: fpm_test_settings
end type

type, extends(fpm_build_settings) :: fpm_install_settings
    character(len=:), allocatable :: prefix
    character(len=:), allocatable :: bindir
    character(len=:), allocatable :: libdir
    character(len=:), allocatable :: includedir
    logical :: no_rebuild
end type

!> Settings for interacting and updating with project dependencies
type, extends(fpm_cmd_settings)  :: fpm_update_settings
    character(len=ibug),allocatable :: name(:)
    character(len=:),allocatable    :: dump
    logical                         :: fetch_only
    logical                         :: clean
end type

!> Settings for exporting model data
type, extends(fpm_build_settings) :: fpm_export_settings
    character(len=:),allocatable  :: dump_manifest
    character(len=:),allocatable  :: dump_dependencies
    character(len=:),allocatable  :: dump_model
end type

type, extends(fpm_cmd_settings)   :: fpm_clean_settings
    logical                       :: clean_skip = .false.
    logical                       :: clean_all = .false.
    logical                       :: registry_cache = .false.
end type

type, extends(fpm_build_settings) :: fpm_publish_settings
    logical :: show_package_version = .false.
    logical :: show_upload_data = .false.
    logical :: is_dry_run = .false.
    character(len=:), allocatable :: token
end type

character(len=:),allocatable :: name
character(len=:),allocatable :: os_type
character(len=ibug),allocatable :: names(:)
character(len=:),allocatable :: tnames(:)

character(len=:), allocatable :: version_text(:)
character(len=:), allocatable :: help_new(:), help_fpm(:), help_run(:), &
                 & help_test(:), help_build(:), help_usage(:), help_runner(:), &
                 & help_text(:), help_install(:), help_help(:), help_update(:), &
                 & help_list(:), help_list_dash(:), help_list_nodash(:), &
                 & help_clean(:), help_publish(:)
character(len=20),parameter :: manual(*)=[ character(len=20) ::&
&  ' ',     'fpm',    'new',     'build',  'run',    'clean',  &
&  'test',  'runner', 'install', 'update', 'list',   'help',   'version', 'publish' ]

character(len=:), allocatable :: val_runner, val_compiler, val_flag, val_cflag, val_cxxflag, val_ldflag, &
    val_profile, val_runner_args, val_dump


!   '12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
character(len=80), parameter :: help_text_build_common(*) = [character(len=80) ::      &
    ' --profile PROF    Selects the compilation profile for the build.               ',&
    '                   Currently available profiles are "release" for               ',&
    '                   high optimization and "debug" for full debug options.        ',&
    '                   If --flag is not specified the "debug" flags are the         ',&
    '                   default.                                                     ',&
    ' --no-prune        Disable tree-shaking/pruning of unused module dependencies   '&
    ]
!   '12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
character(len=80), parameter :: help_text_compiler(*) = [character(len=80) :: &
    ' --compiler NAME    Specify a compiler name. The default is "gfortran"          ',&
    '                    unless set by the environment variable FPM_FC.              ',&
    ' --c-compiler NAME  Specify the C compiler name. Automatically determined by    ',&
    '                    default unless set by the environment variable FPM_CC.      ',&
    ' --cxx-compiler NAME  Specify the C++ compiler name. Automatically determined by',&
    '                    default unless set by the environment variable FPM_CXX.     ',&
    ' --archiver NAME    Specify the archiver name. Automatically determined by      ',&
    '                    default unless set by the environment variable FPM_AR.      '&
    ]

!   '12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
character(len=80), parameter :: help_text_flag(*) = [character(len=80) :: &
    ' --flag  FFLAGS    selects compile arguments for the build, the default value is',&
    '                   set by the FPM_FFLAGS environment variable. These are added  ',&
    '                   to the profile options if --profile is specified, else these ',&
    '                   are added to the defaults. To override the defaults, use the ',&
    '                   keyword [fortran] in the manifest. Note object and .mod      ',&
    '                   directory locations are always built in.                     ',&
    ' --c-flag CFLAGS   selects compile arguments specific for C source in the build.',&
    '                   The default value is set by the FPM_CFLAGS environment       ',&
    '                   variable.                                                    ',&
    ' --cxx-flag CFLAGS selects compile arguments specific for C++ source in the     ',&
    '                   build. The default value is set by the FPM_CXXFLAGS          ',&
    '                   environment variable.                                        ',&
    ' --link-flag LDFLAGS  select arguments passed to the linker for the build. The  ',&
    '                   default value is set by the FPM_LDFLAGS environment variable.'&
    ]


character(len=80), parameter :: help_text_environment(*) = [character(len=80) :: &
    'ENVIRONMENT VARIABLES',&
    ' FPM_FC            sets the path to the Fortran compiler used for the build,', &
    '                   will be overwritten by --compiler command line option', &
    '', &
    ' FPM_FFLAGS        sets the arguments for the Fortran compiler', &
    '                   will be overwritten by --flag command line option', &
    '', &
    ' FPM_CC            sets the path to the C compiler used for the build,', &
    '                   will be overwritten by --c-compiler command line option', &
    '', &
    ' FPM_CFLAGS        sets the arguments for the C compiler', &
    '                   will be overwritten by --c-flag command line option', &
    '', &
    ' FPM_CXX           sets the path to the C++ compiler used for the build,', &
    '                   will be overwritten by --cxx-compiler command line option', &
    '', &
    ' FPM_CXXFLAGS      sets the arguments for the C++ compiler', &
    '                   will be overwritten by --cxx-flag command line option', &
    '', &
    ' FPM_AR            sets the path to the archiver used for the build,', &
    '                   will be overwritten by --archiver command line option', &
    '', &
    ' FPM_LDFLAGS       sets additional link arguments for creating executables', &
    '                   will be overwritten by --link-flag command line option' &
    ]

contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

        integer, parameter            :: widest = 256
        character(len=4096)           :: cmdarg
        integer                       :: i
        integer                       :: os
        type(fpm_install_settings), allocatable :: install_settings
        type(fpm_publish_settings), allocatable :: publish_settings
        type(fpm_export_settings) , allocatable :: export_settings
        type(version_t) :: version
        character(len=:), allocatable :: common_args, compiler_args, run_args, working_dir, &
            & c_compiler, cxx_compiler, archiver, version_s, token_s

        character(len=*), parameter :: fc_env = "FC", cc_env = "CC", ar_env = "AR", &
            & fflags_env = "FFLAGS", cflags_env = "CFLAGS", cxxflags_env = "CXXFLAGS", ldflags_env = "LDFLAGS", &
            & fc_default = "gfortran", cc_default = " ", ar_default = " ", flags_default = " ", &
            & cxx_env = "CXX", cxx_default = " "
        type(error_t), allocatable :: error

        call set_help()
        os = get_os_type()
        ! text for --version switch,
        select case (os)
            case (OS_LINUX);   os_type =  "OS Type:     Linux"
            case (OS_MACOS);   os_type =  "OS Type:     macOS"
            case (OS_WINDOWS); os_type =  "OS Type:     Windows"
            case (OS_CYGWIN);  os_type =  "OS Type:     Cygwin"
            case (OS_SOLARIS); os_type =  "OS Type:     Solaris"
            case (OS_FREEBSD); os_type =  "OS Type:     FreeBSD"
            case (OS_OPENBSD); os_type =  "OS Type:     OpenBSD"
            case (OS_UNKNOWN); os_type =  "OS Type:     Unknown"
            case default     ; os_type =  "OS Type:     UNKNOWN"
        end select

        ! Get current release version
        version = fpm_version()
        version_s = version%s()

        version_text = [character(len=80) :: &
         &  'Version:     '//trim(version_s)//', alpha',               &
         &  'Program:     fpm(1)',                                     &
         &  'Description: A Fortran package manager and build system', &
         &  'Home Page:   https://github.com/fortran-lang/fpm',        &
         &  'License:     MIT',                                        &
         &  os_type]
        ! find the subcommand name by looking for first word on command
        ! not starting with dash
        CLI_RESPONSE_FILE=.true.
        cmdarg = get_subcommand()

        common_args = &
          ' --directory:C " "' // &
          ' --verbose F'

        run_args = &
          ' --target " "' // &
          ' --list F' // &
          ' --runner " "' // &
          ' --runner-args " "'

        compiler_args = &
          ' --profile " "' // &
          ' --no-prune F' // &
          ' --compiler "'//get_fpm_env(fc_env, fc_default)//'"' // &
          ' --c-compiler "'//get_fpm_env(cc_env, cc_default)//'"' // &
          ' --cxx-compiler "'//get_fpm_env(cxx_env, cxx_default)//'"' // &
          ' --archiver "'//get_fpm_env(ar_env, ar_default)//'"' // &
          ' --flag:: "'//get_fpm_env(fflags_env, flags_default)//'"' // &
          ' --c-flag:: "'//get_fpm_env(cflags_env, flags_default)//'"' // &
          ' --cxx-flag:: "'//get_fpm_env(cxxflags_env, flags_default)//'"' // &
          ' --link-flag:: "'//get_fpm_env(ldflags_env, flags_default)//'"'

        ! now set subcommand-specific help text and process commandline
        ! arguments. Then call subcommand routine
        select case(trim(cmdarg))

        case('run')
            call set_args(common_args // compiler_args // run_args //'&
            & --all F &
            & --example F&
            & --',help_run,version_text)

            call check_build_vals()

            if( size(unnamed) > 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif


            if(specified('target') )then
               call split(sget('target'),tnames,delimiters=' ,:')
               names=[character(len=max(len(names),len(tnames))) :: names,tnames]
            endif

            ! convert --all to '*'
            if(lget('all'))then
               names=[character(len=max(len(names),1)) :: names,'*' ]
            endif

            ! convert special string '..' to equivalent (shorter) '*'
            ! to allow for a string that does not require shift-key and quoting
            do i=1,size(names)
               if(names(i)=='..')names(i)='*'
            enddo

            ! If there are additional command-line arguments, remove the additional
            ! double quotes which have been added by M_CLI2
            val_runner_args=sget('runner-args')
            call remove_characters_in_set(val_runner_args,set='"')

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            allocate(fpm_run_settings :: cmd_settings)
            val_runner=sget('runner')
            if(specified('runner') .and. val_runner=='')val_runner='echo'

            cmd_settings=fpm_run_settings(&
            & args=remaining,&
            & profile=val_profile,&
            & prune=.not.lget('no-prune'), &
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & cxx_compiler=cxx_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & cxxflag=val_cxxflag, &
            & ldflag=val_ldflag, &
            & example=lget('example'), &
            & list=lget('list'),&
            & build_tests=.false.,&
            & name=names,&
            & runner=val_runner,&
            & runner_args=val_runner_args, &
            & verbose=lget('verbose') )

        case('build')
            call set_args(common_args // compiler_args //'&
            & --list F &
            & --show-model F &
            & --dump " " &
            & --tests F &
            & --',help_build,version_text)

            call check_build_vals()

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')

            val_dump = sget('dump')
            if (specified('dump') .and. val_dump=='')val_dump='fpm_model.toml'

            allocate( fpm_build_settings :: cmd_settings )
            cmd_settings=fpm_build_settings(  &
            & profile=val_profile,&
            & dump=val_dump,&
            & prune=.not.lget('no-prune'), &
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & cxx_compiler=cxx_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & cxxflag=val_cxxflag, &
            & ldflag=val_ldflag, &
            & list=lget('list'),&
            & show_model=lget('show-model'),&
            & build_tests=lget('tests'),&
            & verbose=lget('verbose') )

        case('new')
            call set_args(common_args // '&
            & --src F &
            & --lib F &
            & --app F &
            & --test F &
            & --example F &
            & --backfill F &
            & --full F &
            & --bare F', &
            & help_new, version_text)
            select case(size(unnamed))
            case(1)
                if(lget('backfill'))then
                   name='.'
                else
                   write(stderr,'(*(7x,g0,/))') &
                   & '<USAGE> fpm new NAME [[--lib|--src] [--app] [--test] [--example]]|[--full|--bare] [--backfill]'
                   call fpm_stop(1,'directory name required')
                endif
            case(2)
                name=trim(unnamed(2))
            case default
                write(stderr,'(7x,g0)') &
                & '<USAGE> fpm new NAME [[--lib|--src] [--app] [--test] [--example]]| [--full|--bare] [--backfill]'
                call fpm_stop(2,'only one directory name allowed')
            end select
            !*! canon_path is not converting ".", etc.
            if(name=='.')then
               call get_current_directory(name, error)
               if (allocated(error)) then
                  write(stderr, '("[Error]", 1x, a)') error%message
                  stop 1
               endif
            endif
            name=canon_path(name)
            if( .not.is_fortran_name(to_fortran_name(basename(name))) )then
                write(stderr,'(g0)') [ character(len=72) :: &
                & '<ERROR> the fpm project name must be made of up to 63 ASCII letters,', &
                & '        numbers, underscores, or hyphens, and start with a letter.']
                call fpm_stop(4,' ')
            endif


            allocate(fpm_new_settings :: cmd_settings)
            if (any( specified([character(len=10) :: 'src','lib','app','test','example','bare'])) &
            & .and.lget('full') )then
                write(stderr,'(*(a))')&
                &'<ERROR> --full and any of [--src|--lib,--app,--test,--example,--bare]', &
                &'        are mutually exclusive.'
                call fpm_stop(5,' ')
            elseif (any( specified([character(len=10) :: 'src','lib','app','test','example','full'])) &
            & .and.lget('bare') )then
                write(stderr,'(*(a))')&
                &'<ERROR> --bare and any of [--src|--lib,--app,--test,--example,--full]', &
                &'        are mutually exclusive.'
                call fpm_stop(3,' ')
            elseif (any( specified([character(len=10) :: 'src','lib','app','test','example']) ) )then
                cmd_settings=fpm_new_settings(&
                 & backfill=lget('backfill'),               &
                 & name=name,                               &
                 & with_executable=lget('app'),             &
                 & with_lib=any([lget('lib'),lget('src')]), &
                 & with_test=lget('test'),                  &
                 & with_example=lget('example'),            &
                 & verbose=lget('verbose') )
            else  ! default if no specific directories are requested
                cmd_settings=fpm_new_settings(&
                 & backfill=lget('backfill') ,           &
                 & name=name,                            &
                 & with_executable=.true.,               &
                 & with_lib=.true.,                      &
                 & with_test=.true.,                     &
                 & with_example=lget('full'),            &
                 & with_full=lget('full'),               &
                 & with_bare=lget('bare'),               &
                 & verbose=lget('verbose') )
            endif

        case('help','manual')
            call set_args(common_args, help_help,version_text)
            if(size(unnamed)<2)then
                if(unnamed(1)=='help')then
                   unnamed=['   ', 'fpm']
                else
                   unnamed=manual
                endif
            elseif(unnamed(2)=='manual')then
                unnamed=manual
            endif
            allocate(character(len=widest) :: help_text(0))
            do i=2,size(unnamed)
                select case(unnamed(i))
                case('       ' )
                case('fpm    ' )
                   help_text=[character(len=widest) :: help_text, help_fpm]
                case('new    ' )
                   help_text=[character(len=widest) :: help_text, help_new]
                case('build  ' )
                   help_text=[character(len=widest) :: help_text, help_build]
                case('install' )
                   help_text=[character(len=widest) :: help_text, help_install]
                case('run    ' )
                   help_text=[character(len=widest) :: help_text, help_run]
                case('test   ' )
                   help_text=[character(len=widest) :: help_text, help_test]
                case('runner' )
                   help_text=[character(len=widest) :: help_text, help_runner]
                case('list   ' )
                   help_text=[character(len=widest) :: help_text, help_list]
                case('update ' )
                   help_text=[character(len=widest) :: help_text, help_update]
                case('help   ' )
                   help_text=[character(len=widest) :: help_text, help_help]
                case('version' )
                   help_text=[character(len=widest) :: help_text, version_text]
                case('clean' )
                   help_text=[character(len=widest) :: help_text, help_clean]
                case('publish')
                   help_text=[character(len=widest) :: help_text, help_publish]
                case default
                   help_text=[character(len=widest) :: help_text, &
                   & '<ERROR> unknown help topic "'//trim(unnamed(i))//'"']
                   !!& '<ERROR> unknown help topic "'//trim(unnamed(i)).'not found in:',manual]
                end select
            enddo
            call printhelp(help_text)

        case('install')
            call set_args(common_args // compiler_args // '&
                & --no-rebuild F --prefix " " &
                & --list F &
                & --libdir "lib" --bindir "bin" --includedir "include"', &
                help_install, version_text)

            call check_build_vals()

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            allocate(install_settings, source=fpm_install_settings(&
                list=lget('list'), &
                profile=val_profile,&
                prune=.not.lget('no-prune'), &
                compiler=val_compiler, &
                c_compiler=c_compiler, &
                cxx_compiler=cxx_compiler, &
                archiver=archiver, &
                flag=val_flag, &
                cflag=val_cflag, &
                cxxflag=val_cxxflag, &
                ldflag=val_ldflag, &
                no_rebuild=lget('no-rebuild'), &
                verbose=lget('verbose')))
            call get_char_arg(install_settings%prefix, 'prefix')
            call get_char_arg(install_settings%libdir, 'libdir')
            call get_char_arg(install_settings%bindir, 'bindir')
            call get_char_arg(install_settings%includedir, 'includedir')
            call move_alloc(install_settings, cmd_settings)

        case('list')
            call set_args(common_args // '&
            & --list F&
            &', help_list, version_text)
            if(lget('list'))then
                help_text = [character(widest) :: help_list_nodash, help_list_dash]
            else
                help_text = help_list_nodash
            endif
            call printhelp(help_text)

        case('test')
            call set_args(common_args // compiler_args // run_args // ' --', &
              help_test,version_text)

            call check_build_vals()

            if( size(unnamed) > 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif

            if(specified('target') )then
               call split(sget('target'),tnames,delimiters=' ,:')
               names=[character(len=max(len(names),len(tnames))) :: names,tnames]
            endif

            ! convert special string '..' to equivalent (shorter) '*'
            ! to allow for a string that does not require shift-key and quoting
            do i=1,size(names)
               if(names(i)=='..')names(i)='*'
            enddo

            ! If there are additional command-line arguments, remove the additional
            ! double quotes which have been added by M_CLI2
            val_runner_args=sget('runner-args')
            call remove_characters_in_set(val_runner_args,set='"')

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            allocate(fpm_test_settings :: cmd_settings)
            val_runner=sget('runner')
            if(specified('runner') .and. val_runner=='')val_runner='echo'

            cmd_settings=fpm_test_settings(&
            & args=remaining, &
            & profile=val_profile, &
            & prune=.not.lget('no-prune'), &
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & cxx_compiler=cxx_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & cxxflag=val_cxxflag, &
            & ldflag=val_ldflag, &
            & example=.false., &
            & list=lget('list'), &
            & build_tests=.true., &
            & name=names, &
            & runner=val_runner, &
            & runner_args=val_runner_args, &
            & verbose=lget('verbose'))

        case('update')
            call set_args(common_args // ' --fetch-only F --clean F --dump " " ', &
                help_update, version_text)

            if( size(unnamed) > 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif

            val_dump = sget('dump')
            if (specified('dump') .and. val_dump=='')val_dump='fpm_dependencies.toml'

            allocate(fpm_update_settings :: cmd_settings)
            cmd_settings=fpm_update_settings(name=names, dump=val_dump, &
                fetch_only=lget('fetch-only'), verbose=lget('verbose'), &
                clean=lget('clean'))

        case('export')

            call set_args(common_args // compiler_args // '&
                & --manifest "filename"  &
                & --model "filename" &
                & --dependencies "filename" ', &
                help_build, version_text)

            call check_build_vals()

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            allocate(export_settings, source=fpm_export_settings(&
                profile=val_profile,&
                prune=.not.lget('no-prune'), &
                compiler=val_compiler, &
                c_compiler=c_compiler, &
                cxx_compiler=cxx_compiler, &
                archiver=archiver, &
                flag=val_flag, &
                cflag=val_cflag, &
                show_model=.true., &
                cxxflag=val_cxxflag, &
                ldflag=val_ldflag, &
                verbose=lget('verbose')))
            call get_char_arg(export_settings%dump_model, 'model')
            call get_char_arg(export_settings%dump_manifest, 'manifest')
            call get_char_arg(export_settings%dump_dependencies, 'dependencies')
            call move_alloc(export_settings, cmd_settings)


        case('clean')
            call set_args(common_args // &
            &   ' --registry-cache'   // &
            &   ' --skip'             // &
            &   ' --all',                &
                help_clean, version_text)

            block
                logical :: skip, clean_all

                skip = lget('skip')
                clean_all = lget('all')

                if (all([skip, clean_all])) then
                    call fpm_stop(6, 'Do not specify both --skip and --all options on the clean subcommand.')
                end if

                allocate(fpm_clean_settings :: cmd_settings)
                cmd_settings = fpm_clean_settings( &
                &   registry_cache=lget('registry-cache'), &
                &   clean_skip=skip, &
                &   clean_all=clean_all)
            end block

        case('publish')
            call set_args(common_args // compiler_args //'&
            & --show-package-version F &
            & --show-upload-data F &
            & --dry-run F &
            & --token " " &
            & --list F &
            & --show-model F &
            & --tests F &
            & --', help_publish, version_text)

            call check_build_vals()

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            token_s = sget('token')

            allocate(fpm_publish_settings :: cmd_settings)
            cmd_settings = fpm_publish_settings( &
            & show_package_version = lget('show-package-version'), &
            & show_upload_data = lget('show-upload-data'), &
            & is_dry_run = lget('dry-run'), &
            & profile=val_profile,&
            & prune=.not.lget('no-prune'), &
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & cxx_compiler=cxx_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & cxxflag=val_cxxflag, &
            & ldflag=val_ldflag, &
            & list=lget('list'),&
            & show_model=lget('show-model'),&
            & build_tests=lget('tests'),&
            & verbose=lget('verbose'),&
            & token=token_s)

        case default

            if(cmdarg.ne.''.and.which('fpm-'//cmdarg).ne.'')then
                call run('fpm-'//trim(cmdarg)//' '// get_command_arguments_quoted(),.false.)
                stop
            else
                call set_args('&
                & --list F&
                &', help_fpm, version_text)
                ! Note: will not get here if --version or --usage or --help
                ! is present on commandline
                if(lget('list'))then
                    help_text = help_list_dash
                elseif(len_trim(cmdarg)==0)then
                    write(stdout,'(*(a))')'Fortran Package Manager:'
                    write(stdout,'(*(a))')' '
                    help_text = [character(widest) :: help_list_nodash, help_usage]
                else
                    write(stderr,'(*(a))')'<ERROR> unknown subcommand [', &
                     & trim(cmdarg), ']'
                    help_text = [character(widest) :: help_list_dash, help_usage]
                endif
                call printhelp(help_text)
            endif

        end select

        if (allocated(cmd_settings)) then
            working_dir = sget("directory")
            call move_alloc(working_dir, cmd_settings%working_dir)
        end if

    contains

    subroutine check_build_vals()
        val_compiler=sget('compiler')
        if(val_compiler=='') val_compiler='gfortran'

        val_flag = " " // sget('flag')
        val_cflag = " " // sget('c-flag')
        val_cxxflag = " "// sget('cxx-flag')
        val_ldflag = " " // sget('link-flag')
        val_profile = sget('profile')

    end subroutine check_build_vals

    !> Print help text and stop
    subroutine printhelp(lines)
    character(len=:),intent(in),allocatable :: lines(:)
    integer :: iii,ii
        if(allocated(lines))then
           ii=size(lines)
           if(ii > 0 .and. len(lines)> 0) then
               write(stdout,'(g0)')(trim(lines(iii)), iii=1, ii)
           else
               write(stdout,'(a)')'<WARNING> *printhelp* output requested is empty'
           endif
        endif
        stop
    end subroutine printhelp

    end subroutine get_command_line_settings

    subroutine set_help()
   help_list_nodash=[character(len=80) :: &
   'USAGE: fpm [ SUBCOMMAND [SUBCOMMAND_OPTIONS] ]|[--list|--help|--version]', &
   '       where SUBCOMMAND is commonly new|build|run|test                  ', &
   '                                                                        ', &
   ' subcommand may be one of                                               ', &
   '                                                                        ', &
   '  build     Compile the package placing results in the "build" directory', &
   '  help      Display help                                                ', &
   '  list      Display this list of subcommand descriptions                ', &
   '  new       Create a new Fortran package directory with sample files    ', &
   '  run       Run the local package application programs                  ', &
   '  test      Run the test programs                                       ', &
   '  update    Update and manage project dependencies                      ', &
   '  install   Install project                                             ', &
   '  clean     Delete the build                                            ', &
   '  publish   Publish package to the registry                             ', &
   '                                                                        ', &
   ' Enter "fpm --list" for a brief list of subcommand options. Enter       ', &
   ' "fpm --help" or "fpm SUBCOMMAND --help" for detailed descriptions.     ', &
   ' ']
   help_list_dash = [character(len=80) :: &
   '                                                                                ', &
   ' build [--compiler COMPILER_NAME] [--profile PROF] [--flag FFLAGS] [--list]     ', &
   '       [--tests] [--no-prune] [--dump [FILENAME]]                               ', &
   ' help [NAME(s)]                                                                 ', &
   ' new NAME [[--lib|--src] [--app] [--test] [--example]]|                         ', &
   '          [--full|--bare][--backfill]                                           ', &
   ' update [NAME(s)] [--fetch-only] [--clean] [--verbose] [--dump [FILENAME]]      ', &
   ' list [--list]                                                                  ', &
   ' run  [[--target] NAME(s) [--example] [--profile PROF] [--flag FFLAGS] [--all]  ', &
   '      [--runner "CMD"] [--compiler COMPILER_NAME] [--list] [-- ARGS]            ', &
   ' test [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--runner "CMD"]    ', &
   '      [--list] [--compiler COMPILER_NAME] [-- ARGS]                             ', &
   ' install [--profile PROF] [--flag FFLAGS] [--no-rebuild] [--prefix PATH]        ', &
   '         [options]                                                              ', &
   ' clean [--skip] [--all] [--registry-cache]                                      ', &
   ' publish [--token TOKEN] [--show-package-version] [--show-upload-data]          ', &
   '         [--dry-run] [--verbose]                                                ', &
   ' ']
    help_usage=[character(len=80) :: &
    '' ]
    help_runner=[character(len=80) :: &
   'NAME                                                                            ', &
   '   --runner(1) - a shared option for specifying an application to launch        ', &
   '                 executables.                                                   ', &
   '                                                                                ', &
   'SYNOPSIS                                                                        ', &
   '   fpm run|test --runner CMD ... --runner-args ARGS -- SUFFIX_OPTIONS           ', &
   '                                                                                ', &
   'DESCRIPTION                                                                     ', &
   '   The --runner option allows specifying a program to launch                    ', &
   '   executables selected via the fpm(1) subcommands "run" and "test". This       ', &
   '   gives easy recourse to utilities such as debuggers and other tools           ', &
   '   that wrap other executables.                                                 ', &
   '                                                                                ', &
   '   These external commands are not part of fpm(1) itself as they vary           ', &
   '   from platform to platform or require independent installation.               ', &
   '                                                                                ', &
   'OPTION                                                                          ', &
   ' --runner ''CMD''  quoted command used to launch the fpm(1) executables.          ', &
   '               Available for both the "run" and "test" subcommands.             ', &
   '               If the keyword is specified without a value the default command  ', &
   '               is "echo".                                                       ', &
   ' --runner-args "args"    an additional option to pass command-line arguments    ', &
   '               to the runner command, instead of to the fpm app.                ', &
   ' -- SUFFIX_OPTIONS  additional options to suffix the command CMD and executable ', &
   '                    file names with. These options are passed as command-line   ', &
   '                    arguments to the app.                                       ', &
   'EXAMPLES                                                                        ', &
   '   Use cases for ''fpm run|test --runner "CMD"'' include employing                ', &
   '   the following common GNU/Linux and Unix commands:                            ', &
   '                                                                                ', &
   ' INTERROGATE                                                                    ', &
   '    + nm - list symbols from object files                                       ', &
   '    + size - list section sizes and total size.                                 ', &
   '    + ldd - print shared object dependencies                                    ', &
   '    + ls - list directory contents                                              ', &
   '    + stat - display file or file system status                                 ', &
   '    + file - determine file type                                                ', &
   ' PERFORMANCE AND DEBUGGING                                                      ', &
   '    + gdb - The GNU Debugger                                                    ', &
   '    + valgrind - a suite of tools for debugging and profiling                   ', &
   '    + time - time a simple command or give resource usage                       ', &
   '    + timeout - run a command with a time limit                                 ', &
   ' COPY                                                                           ', &
   '    + install - copy files and set attributes                                   ', &
   '    + tar - an archiving utility                                                ', &
   ' ALTER                                                                          ', &
   '    + rm - remove files or directories                                          ', &
   '    + chmod - change permissions of a file                                      ', &
   '    + strip - remove unnecessary information from strippable files              ', &
   '                                                                                ', &
   ' For example                                                                    ', &
   '                                                                                ', &
   '  fpm test --runner gdb                                                         ', &
   '  fpm run --runner "tar cvfz $HOME/bundle.tgz"                                  ', &
   '  fpm run --runner "mpiexec" --runner-args "-np 12"                             ', &
   '  fpm run --runner ldd                                                          ', &
   '  fpm run --runner strip                                                        ', &
   '  fpm run --runner ''cp -t /usr/local/bin''                                       ', &
   '                                                                                ', &
   '  # options after executable name can be specified after the -- option          ', &
   '  fpm --runner cp run -- /usr/local/bin/                                        ', &
   '  # generates commands of the form "cp $FILENAME /usr/local/bin/"               ', &
   '                                                                                ', &
   '  # bash(1) alias example:                                                      ', &
   '  alias fpm-install=\                                                           ', &
   '  "fpm run --profile release --runner ''install -vbp -m 0711 -t ~/.local/bin''" ', &
   '  fpm-install                                                           ', &
    '' ]
    help_fpm=[character(len=80) :: &
    'NAME                                                                   ', &
    '   fpm(1) - A Fortran package manager and build system                 ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    '   fpm SUBCOMMAND [SUBCOMMAND_OPTIONS]                                 ', &
    '                                                                       ', &
    '   fpm --help|--version|--list                                         ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    '   fpm(1) is a package manager that helps you create Fortran projects  ', &
    '   from source -- it automatically determines dependencies!            ', &
    '                                                                       ', &
    '   Most significantly fpm(1) lets you draw upon other fpm(1) packages  ', &
    '   in distributed git(1) repositories as if the packages were a basic  ', &
    '   part of your default programming environment, as well as letting    ', &
    '   you share your projects with others in a similar manner.            ', &
    '                                                                       ', &
    '   All output goes into the directory "build/" which can generally be  ', &
    '   removed and rebuilt if required. Note that if external packages are ', &
    '   being used you need network connectivity to rebuild from scratch.   ', &
    '                                                                       ', &
    'SUBCOMMANDS                                                            ', &
    '  Valid fpm(1) subcommands are:                                        ', &
    '                                                                       ', &
    '  + build    Compile the packages into the "build/" directory.         ', &
    '  + new      Create a new Fortran package directory with sample files. ', &
    '  + update   Update the project dependencies.                          ', &
    '  + run      Run the local package binaries. Defaults to all binaries  ', &
    '             for that release.                                         ', &
    '  + test     Run the tests.                                            ', &
    '  + help     Alternate to the --help switch for displaying help text.  ', &
    '  + list     Display brief descriptions of all subcommands.            ', &
    '  + install  Install project.                                          ', &
    '  + clean    Delete directories in the "build/" directory, except      ', &
    '             dependencies. Prompts for confirmation to delete.         ', &
    '  + publish  Publish package to the registry.                          ', &
    '                                                                       ', &
    '  Their syntax is                                                      ', &
    '                                                                                ', &
    '    build [--profile PROF] [--flag FFLAGS] [--list] [--compiler COMPILER_NAME]  ', &
    '          [--tests] [--no-prune] [--dump [FILENAME]]                            ', &
    '    new NAME [[--lib|--src] [--app] [--test] [--example]]|                      ', &
    '             [--full|--bare][--backfill]                                        ', &
    '    update [NAME(s)] [--fetch-only] [--clean] [--dump [FILENAME]]               ', &
    '    run [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--list] [--all]  ', &
    '        [--example] [--runner "CMD"] [--compiler COMPILER_NAME]                 ', &
    '        [--no-prune] [-- ARGS]                                                  ', &
    '    test [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--list]         ', &
    '         [--runner "CMD"] [--compiler COMPILER_NAME] [--no-prune] [-- ARGS]     ', &
    '    help [NAME(s)]                                                              ', &
    '    list [--list]                                                               ', &
    '    install [--profile PROF] [--flag FFLAGS] [--no-rebuild] [--prefix PATH]     ', &
    '            [options]                                                           ', &
    '    clean [--skip] [--all] [--registry-cache]                                   ', &
    '    publish [--token TOKEN] [--show-package-version] [--show-upload-data]       ', &
    '            [--dry-run] [--verbose]                                             ', &
    '                                                                                ', &
    'SUBCOMMAND OPTIONS                                                              ', &
    ' -C, --directory PATH', &
    '             Change working directory to PATH before running any command', &
    help_text_build_common, &
    help_text_compiler, &
    help_text_flag, &
    '  --list     List candidates instead of building or running them. On   ', &
    '             the fpm(1) command this shows a brief list of subcommands.', &
    '  --runner CMD  Provides a command to prefix program execution paths.  ', &
    '  -- ARGS    Arguments to pass to executables.                         ', &
    '  --skip     Delete directories in the build/ directory without        ', &
    '             prompting, but skip dependencies. Cannot be used together ', &
    '             with --all.                                               ', &
    '  --all      Delete directories in the build/ directory without        ', &
    '             prompting, including dependencies. Cannot be used together', &
    '             with --skip.                                              ', &
    '  --registry-cache  Delete registry cache.                             ', &
    '                                                                       ', &
    'VALID FOR ALL SUBCOMMANDS                                              ', &
    '  --help     Show help text and exit                                   ', &
    '  --verbose  Display additional information when available             ', &
    '  --version  Show version information and exit.                        ', &
    '                                                                       ', &
    '@file                                                                  ', &
    '   You may replace the default options for the fpm(1) command from a   ', &
    '   file if your first options begin with @file. Initial options will   ', &
    '   then be read from the "response file" "file.rsp" in the current     ', &
    '   directory.                                                          ', &
    '                                                                       ', &
    '   If "file" does not exist or cannot be read, then an error occurs and', &
    '   the program stops. Each line of the file is prefixed with "options" ', &
    '   and interpreted as a separate argument. The file itself may not     ', &
    '   contain @file arguments. That is, it is not processed recursively.  ', &
    '                                                                       ', &
    '   For more information on response files see                          ', &
    '                                                                       ', &
    '      https://urbanjost.github.io/M_CLI2/set_args.3m_cli2.html         ', &
    '                                                                       ', &
    '   The basic functionality described here will remain the same, but    ', &
    '   other features described at the above reference may change.         ', &
    '                                                                       ', &
    '   An example file:                                                    ', &
    '                                                                       ', &
    '     # my build options                                                ', &
    '     options build                                                     ', &
    '     options --compiler gfortran                                       ', &
    '     options --flag "-pg -static -pthread -Wunreachable-code -Wunused  ', &
    '      -Wuninitialized -g -O -fbacktrace -fdump-core -fno-underscoring  ', &
    '      -frecord-marker=4 -L/usr/X11R6/lib -L/usr/X11R6/lib64 -lX11"     ', &
    '                                                                       ', &
    '   Note --flag would have to be on one line as response files do not   ', &
    '   (currently) allow for continued lines or multiple specifications of ', &
    '   the same option.                                                    ', &
    '                                                                       ', &
    help_text_environment, &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    '   sample commands:                                                    ', &
    '                                                                       ', &
    '    fpm new mypackage --app --test                                     ', &
    '    fpm build                                                          ', &
    '    fpm test                                                           ', &
    '    fpm run                                                            ', &
    '    fpm run --example                                                  ', &
    '    fpm new --help                                                     ', &
    '    fpm run myprogram --profile release -- -x 10 -y 20 --title "my title"       ', &
    '    fpm install --prefix ~/.local                                               ', &
    '    fpm clean --all                                                             ', &
    '                                                                                ', &
    'SEE ALSO                                                                        ', &
    '                                                                                ', &
    ' + The fpm(1) home page is at https://github.com/fortran-lang/fpm               ', &
    ' + Registered fpm(1) packages are at https://fortran-lang.org/packages          ', &
    ' + The fpm(1) TOML file format is described at                                  ', &
    '   https://fpm.fortran-lang.org/spec/manifest.html                              ', &
    '']
    help_list=[character(len=80) :: &
    'NAME                                                                   ', &
    ' list(1) - list summary of fpm(1) subcommands                          ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm list                                                              ', &
    '                                                                       ', &
    ' fpm list --help|--version                                             ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' Display a short description for each fpm(1) subcommand.               ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' --list     display a list of command options as well. This is the     ', &
    '            same output as generated by "fpm --list".                  ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' display a short list of fpm(1) subcommands                            ', &
    '                                                                       ', &
    '  fpm list                                                             ', &
    '  fpm --list                                                           ', &
    '' ]
    help_run=[character(len=80) :: &
    'NAME                                                                   ', &
    ' run(1) - the fpm(1) subcommand to run project applications            ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm run [[--target] NAME(s) [--profile PROF] [--flag FFLAGS]', &
    '         [--compiler COMPILER_NAME] [--runner "CMD"] [--example]', &
    '         [--list] [--all] [-- ARGS]', &
    '                                                                       ', &
    ' fpm run --help|--version                                              ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' Run the applications in your fpm(1) package. By default applications  ', &
    ' in /app or specified as "executable" in your "fpm.toml" manifest are  ', &
    ' used. Alternatively demonstration programs in example/ or specified in', &
    ' the "example" section in "fpm.toml" can be executed. The applications ', &
    ' are automatically rebuilt before being run if they are out of date.   ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' --target NAME(s)  list of application names to execute. No name is    ', &
    '                   required if only one target exists. If no name is   ', &
    '                   supplied and more than one candidate exists or a    ', &
    '                   name has no match a list is produced and fpm(1)     ', &
    '                   exits.                                              ', &
    '                                                                       ', &
    '                   Basic "globbing" is supported where "?" represents  ', &
    '                   any single character and "*" represents any string. ', &
    '                   Note The glob string normally needs quoted to       ', &
    '                   the special characters from shell expansion.        ', &
    ' --all   Run all examples or applications. An alias for --target ''*''.  ', &
    ' --example  Run example programs instead of applications.              ', &
    help_text_build_common, &
    help_text_compiler, &
    help_text_flag, &
    ' --runner CMD  A command to prefix the program execution paths with.   ', &
    '               see "fpm help runner" for further details.              ', &
    ' --list     list basenames of candidates instead of running them. Note ', &
    '            out-of-date candidates will still be rebuilt before being  ', &
    '            listed.                                                    ', &
    ' -- ARGS    optional arguments to pass to the program(s). The same     ', &
    '            arguments are passed to all program names specified.       ', &
    '                                                                       ', &
    help_text_environment, &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' fpm(1) - run or display project applications:                         ', &
    '                                                                       ', &
    '  fpm run        # run a target when only one exists or list targets   ', &
    '  fpm run --list # list basename of all targets, running nothing.      ', &
    '  fpm run "demo*" --list # list target basenames starting with "demo*".', &
    '  fpm run "psi*" --runner # list target pathnames starting with "psi*".', &
    '  fpm run --all  # run all targets, no matter how many there are.      ', &
    '                                                                       ', &
    '  # run default program built or to be built with the compiler command ', &
    '  # "f90". If more than one app exists a list displays and target names', &
    '  # are required.                                                      ', &
    '  fpm run --compiler f90                                               ', &
    '                                                                       ', &
    '  # run example programs instead of the application programs.          ', &
    '  fpm run --example "*"                                                ', &
    '                                                                       ', &
    '  # run a specific program and pass arguments to the command           ', &
    '  fpm run myprog -- -x 10 -y 20 --title "my title line"                ', &
    '                                                                       ', &
    '  # run production version of two applications                         ', &
    '  fpm run --target prg1,prg2 --profile release                         ', &
    '                                                                       ', &
    '  # install executables in directory (assuming install(1) exists)      ', &
    '  fpm run --runner ''install -b -m 0711 -p -t /usr/local/bin''         ', &
    '' ]
    help_build=[character(len=80) :: &
    'NAME                                                                   ', &
    ' build(1) - the fpm(1) subcommand to build a project                   ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm build [--profile PROF] [--flag FFLAGS] [--compiler COMPILER_NAME] ', &
    '           [--list] [--tests] [--dump [FILENAME]]                      ', &
    '                                                                       ', &
    ' fpm build --help|--version                                            ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' The "fpm build" command                                               ', &
    '    o Fetches any dependencies                                         ', &
    '    o Scans your sources                                               ', &
    '    o Builds them in the proper order                                  ', &
    '                                                                       ', &
    ' The Fortran source files are assumed by default to be in              ', &
    '    o src/     for modules and procedure source                        ', &
    '    o app/     main program(s) for applications                        ', &
    '    o test/    main program(s) and support files for project tests     ', &
    '    o example/ main program(s) for example programs                    ', &
    ' Changed or new files found are rebuilt. The results are placed in     ', &
    ' the build/ directory.                                                 ', &
    '                                                                       ', &
    ' Non-default pathnames and remote dependencies are used if             ', &
    ' specified in the "fpm.toml" file.                                     ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    help_text_build_common,&
    help_text_compiler, &
    help_text_flag, &
    ' --list        list candidates instead of building or running them     ', &
    ' --tests       build all tests (otherwise only if needed)              ', &
    ' --show-model  show the model and exit (do not build)                  ', &
    ' --dump [FILENAME] save model representation to file. use JSON format  ', &
    '                   if file name is *.json; use TOML format otherwise   ', &
    '                   (default file name: model.toml)                     ', &
    ' --help        print this help and exit                                ', &
    ' --version     print program version information and exit              ', &
    '                                                                       ', &
    help_text_environment, &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' Sample commands:                                                      ', &
    '                                                                       ', &
    '  fpm build                   # build with debug options               ', &
    '  fpm build --profile release # build with high optimization           ', &
    '' ]

    help_help=[character(len=80) :: &
    'NAME                                                                   ', &
    '   help(1) - the fpm(1) subcommand to display help                     ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    '   fpm help [fpm] [new] [build] [run] [test] [help] [version] [manual] ', &
    '   [runner]                                                            ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    '   The "fpm help" command is an alternative to the --help parameter    ', &
    '   on the fpm(1) command and its subcommands.                          ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    '   NAME(s)    A list of topic names to display. All the subcommands    ', &
    '              have their own page (new, build, run, test, ...).        ', &
    '                                                                       ', &
    '              The special name "manual" displays all the fpm(1)        ', &
    '              built-in documentation.                                  ', &
    '                                                                       ', &
    '              The default is to display help for the fpm(1) command    ', &
    '              itself.                                                  ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    '   Sample usage:                                                       ', &
    '                                                                       ', &
    '     fpm help           # general fpm(1) command help                  ', &
    '     fpm help version   # show program version                         ', &
    '     fpm help new       # display help for "new" subcommand            ', &
    '     fpm help manual    # All fpm(1) built-in documentation            ', &
    '                                                                       ', &
    '' ]
    help_new=[character(len=80) ::                                             &
    'NAME                                                                   ', &
    ' new(1) - the fpm(1) subcommand to initialize a new project            ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm new NAME [[--lib|--src] [--app] [--test] [--example]]|            ', &
    '              [--full|--bare][--backfill]                              ', &
    ' fpm new --help|--version                                              ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' "fpm new" creates and populates a new programming project directory.  ', &
    '                                                                       ', &
    ' It                                                                    ', &
    '   o creates a directory with the specified name                       ', &
    '   o runs the command "git init" in that directory                     ', &
    '   o populates the directory with the default project directories      ', &
    '   o adds sample Fortran source files                                  ', &
    '                                                                       ', &
    ' The default file structure (that will be automatically scanned) is    ', &
    '                                                                       ', &
    '     NAME/                                                             ', &
    '       fpm.toml                                                        ', &
    '       src/                                                            ', &
    '           NAME.f90                                                    ', &
    '       app/                                                            ', &
    '           main.f90                                                    ', &
    '       test/                                                           ', &
    '           check.f90                                                   ', &
    '       example/                                                        ', &
    '           demo.f90                                                    ', &
    '                                                                       ', &
    ' Using this file structure is highly encouraged, particularly for      ', &
    ' small packages primarily intended to be used as dependencies.         ', &
    '                                                                       ', &
    ' If you find this restrictive and need to customize the package        ', &
    ' structure you will find using the --full switch creates a             ', &
    ' heavily annotated manifest file with references to documentation      ', &
    ' to aid in constructing complex package structures.                    ', &
    '                                                                       ', &
    ' Remember to update the information in the sample "fpm.toml"           ', &
    ' file with your name and e-mail address.                               ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' NAME   the name of the project directory to create. The name          ', &
    '        must be made of up to 63 ASCII letters, digits, underscores,   ', &
    '        or hyphens, and start with a letter.                           ', &
    '                                                                       ', &
    ' The default is to create the src/, app/, and test/ directories.       ', &
    ' If any of the following options are specified then only the           ', &
    ' selected subdirectories are generated:                                ', &
    '                                                                       ', &
    ' --lib,--src  create directory src/ and a placeholder module           ', &
    '              named "NAME.f90" for use with subcommand "build".        ', &
    ' --app        create directory app/ and a placeholder main             ', &
    '              program for use with subcommand "run".                   ', &
    ' --test       create directory test/ and a placeholder program         ', &
    '              for use with the subcommand "test". Note that sans       ', &
    '              "--lib" it really does not have anything to test.        ', &
    ' --example    create directory example/ and a placeholder program      ', &
    '              for use with the subcommand "run --example".             ', &
    '              It is only created by default if "--full is" specified.  ', &
    '                                                                       ', &
    ' So the default is equivalent to                                        ',&
    '                                                                       ', &
    '    fpm NAME --lib --app --test                                        ', &
    '                                                                       ', &
    ' --backfill   By default the directory must not exist. If this         ', &
    '              option is present the directory may pre-exist and        ', &
    '              only subdirectories and files that do not                ', &
    '              already exist will be created. For example, if you       ', &
    '              previously entered "fpm new myname --lib" entering       ', &
    '              "fpm new myname -full --backfill" will create any missing', &
    '              app/, example/, and test/ directories and programs.      ', &
    '                                                                       ', &
    ' --full       By default a minimal manifest file ("fpm.toml") is       ', &
    '              created that depends on auto-discovery. With this        ', &
    '              option a much more extensive manifest sample is written  ', &
    '              and the example/ directory is created and populated.     ', &
    '              It is designed to facilitate creating projects that      ', &
    '              depend extensively on non-default build options.         ', &
    '                                                                       ', &
    ' --bare       A minimal manifest file ("fpm.toml") is created and      ', &
    '              "README.md" file is created but no directories or        ', &
    '              sample Fortran are generated.                            ', &
    '                                                                       ', &
    ' --help       print this help and exit                                 ', &
    ' --version    print program version information and exit               ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' Sample use                                                            ', &
    '                                                                       ', &
    '   fpm new myproject  # create new project directory and seed it       ', &
    '   cd myproject       # Enter the new directory                        ', &
    '   # and run commands such as                                          ', &
    '   fpm build                                                           ', &
    '   fpm run            # run lone example application program           ', &
    '   fpm test           # run example test program(s)                    ', &
    '   fpm run --example  # run lone example program                       ', &
    '                                                                       ', &
    '   fpm new A --full # create example/ and an annotated fpm.toml as well', &
    '   fpm new A --bare # create no directories                            ', &
    '   create any missing files in current directory                       ', &
    '   fpm new --full --backfill                                           ', &
    '' ]
    help_test=[character(len=80) :: &
    'NAME                                                                   ', &
    ' test(1) - the fpm(1) subcommand to run project tests                  ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm test [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS]', &
    '          [--compiler COMPILER_NAME ] [--runner "CMD"] [--list][-- ARGS]', &
    '                                                                       ', &
    ' fpm test --help|--version                                             ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' Run applications you have built to test your project.                 ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' --target NAME(s)  optional list of specific test names to execute.    ', &
    '                   The default is to run all the tests in test/        ', &
    '                   or the tests listed in the "fpm.toml" file.         ', &
    '                                                                       ', &
    '                   Basic "globbing" is supported where "?" represents  ', &
    '                   any single character and "*" represents any string. ', &
    '                   Note The glob string normally needs quoted to       ', &
    '                   protect the special characters from shell expansion.', &
    help_text_build_common,&
    help_text_compiler, &
    help_text_flag, &
    ' --runner CMD  A command to prefix the program execution paths with.   ', &
    '               see "fpm help runner" for further details.              ', &
    ' --list     list candidate basenames instead of running them. Note they', &
    ' --list     will still be built if not currently up to date.           ', &
    ' -- ARGS    optional arguments to pass to the test program(s).         ', &
    '            The same arguments are passed to all test names            ', &
    '            specified.                                                 ', &
    '                                                                       ', &
    help_text_environment, &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    'run tests                                                              ', &
    '                                                                       ', &
    ' # run default tests in /test or as specified in "fpm.toml"            ', &
    ' fpm test                                                              ', &
    '                                                                       ', &
    ' # run using compiler command "f90"                                    ', &
    ' fpm test --compiler f90                                               ', &
    '                                                                       ', &
    ' # run a specific test and pass arguments to the command               ', &
    ' fpm test mytest -- -x 10 -y 20 --title "my title line"                ', &
    '                                                                       ', &
    ' fpm test tst1 tst2 --profile PROF  # run production version of two tests', &
    '' ]
    help_update=[character(len=80) :: &
    'NAME', &
    ' update(1) - manage project dependencies', &
    '', &
    'SYNOPSIS', &
    ' fpm update [--fetch-only] [--clean] [--verbose] [--dump [FILENAME]] [NAME(s)]', &
    '', &
    'DESCRIPTION', &
    ' Manage and update project dependencies. If no dependency names are', &
    ' provided all the dependencies are updated automatically.', &
    '', &
    'OPTIONS', &
    ' --fetch-only  Only fetch dependencies, do not update existing projects', &
    ' --clean       Do not use previous dependency cache', &
    ' --verbose     Show additional printout', &
    ' --dump [FILENAME] Dump updated dependency tree to file. use JSON format  ', &
    '                   if file name is *.json; use TOML format otherwise      ', &
    '                   (default file name: fpm_dependencies.toml)             ', &
    '', &
    'SEE ALSO', &
    ' The fpm(1) home page at https://github.com/fortran-lang/fpm', &
    '' ]
    help_install=[character(len=80) :: &
    'NAME', &
    ' install(1) - install fpm projects', &
    '', &
    'SYNOPSIS', &
    ' fpm install [--profile PROF] [--flag FFLAGS] [--list] [--no-rebuild]', &
    '             [--prefix DIR] [--bindir DIR] [--libdir DIR] [--includedir DIR]', &
    '             [--verbose]', &
    '', &
    'DESCRIPTION', &
    ' Subcommand to install fpm projects. Running install will export the', &
    ' current project to the selected prefix, this will by default install all', &
    ' executables (tests and examples are excluded) which are part of the projects.', &
    ' Libraries and module files are only installed for projects requiring the', &
    ' installation of those components in the package manifest.', &
    '', &
    'OPTIONS', &
    ' --list            list all installable targets for this project,', &
    '                   but do not install any of them', &
    help_text_build_common,&
    help_text_flag, &
    ' --no-rebuild      do not rebuild project before installation', &
    ' --prefix DIR      path to installation directory (requires write access),', &
    '                   the default prefix on Unix systems is $HOME/.local', &
    '                   and %APPDATA%\local on Windows', &
    ' --bindir DIR      subdirectory to place executables in (default: bin)', &
    ' --libdir DIR      subdirectory to place libraries and archives in', &
    '                   (default: lib)', &
    ' --includedir DIR  subdirectory to place headers and module files in', &
    '                   (default: include)', &
    ' --verbose         print more information', &
    '', &
    help_text_environment, &
    '', &
    'EXAMPLES', &
    ' 1. Install release version of project:', &
    '', &
    '    fpm install --profile release', &
    '', &
    ' 2. Install the project without rebuilding the executables:', &
    '', &
    '    fpm install --no-rebuild', &
    '', &
    ' 3. Install executables to a custom prefix into the exe directory:', &
    '', &
    '    fpm install --prefix $PWD --bindir exe', &
    '' ]
    help_clean=[character(len=80) :: &
    'NAME', &
    ' clean(1) - delete the build', &
    '', &
    'SYNOPSIS', &
    ' fpm clean', &
    '', &
    'DESCRIPTION', &
    ' Prompts the user to confirm deletion of the build. If affirmative,', &
    ' directories in the build/ directory are deleted, except dependencies.', &
    ' Use the --registry-cache option to delete the registry cache.', &
    '', &
    'OPTIONS', &
    ' --skip            Delete the build without prompting but skip dependencies.', &
    ' --all             Delete the build without prompting including dependencies.', &
    ' --registry-cache  Delete registry cache.', &
    '' ]
    help_publish=[character(len=80) :: &
    'NAME', &
    ' publish(1) - publish package to the registry', &
    '', &
    'SYNOPSIS', &
    ' fpm publish [--token TOKEN] [--show-package-version] [--show-upload-data]', &
    '             [--dry-run] [--verbose]                                      ', &
    '', &
    ' fpm publish --help|--version', &
    '', &
    'DESCRIPTION', &
    ' Follow the steps to create a tarball and upload a package to the registry:', &
    '', &
    '  1. Register on the website (https://registry-phi.vercel.app/).', &
    '  2. Create a namespace. Uploaded packages must be assigned to a unique', &
    '     namespace to avoid conflicts among packages with similar names. A', &
    '     namespace can accommodate multiple packages.', &
    '  3. Create a token for that namespace. A token is linked to your username', &
    '     and is used to authenticate you during the upload process. Do not share', &
    '     the token with others.', &
    '  4. Run fpm publish --token TOKEN to upload the package to the registry.', &
    '     But be aware that the upload is permanent. An uploaded package cannot be', &
    '     deleted.', &
    '', &
    ' See documentation for more information regarding package upload and usage:', &
    '', &
    ' Package upload:', &
    ' https://fpm.fortran-lang.org/spec/publish.html', &
    '', &
    ' Package usage:', &
    ' https://fpm.fortran-lang.org/spec/manifest.html#dependencies-from-a-registry', &
    '', &
    'OPTIONS', &
    ' --show-package-version   show package version without publishing', &
    ' --show-upload-data       show upload data without publishing', &
    ' --dry-run                perform dry run without publishing', &
    ' --help                   print this help and exit', &
    ' --version                print program version information and exit', &
    ' --verbose                print more information', &
    '', &
    'EXAMPLES', &
    '', &
    ' fpm publish --show-package-version    # show package version without publishing', &
    ' fpm publish --show-upload-data        # show upload data without publishing', &
    ' fpm publish --token TOKEN --dry-run   # perform dry run without publishing', &
    ' fpm publish --token TOKEN             # upload package to the registry', &
    '' ]
     end subroutine set_help

    subroutine get_char_arg(var, arg)
      character(len=:), allocatable, intent(out) :: var
      character(len=*), intent(in) :: arg
      var = sget(arg)
      if (len_trim(var) == 0) deallocate(var)
    end subroutine get_char_arg


    !> Get an environment variable for fpm, this routine ensures that every variable
    !> used by fpm is prefixed with FPM_.
    function get_fpm_env(env, default) result(val)
      character(len=*), intent(in) :: env
      character(len=*), intent(in) :: default
      character(len=:), allocatable :: val

      character(len=*), parameter :: fpm_prefix = "FPM_"

      val = get_env(fpm_prefix//env, default)
    end function get_fpm_env


    !> Build a full runner command (executable + command-line arguments)
    function runner_command(cmd) result(run_cmd)
        class(fpm_run_settings), intent(in) :: cmd
        character(len=:), allocatable :: run_cmd
        !> Get executable
        if (len_trim(cmd%runner)>0) then
            run_cmd = trim(cmd%runner)
        else
            run_cmd = ''
        end if
        !> Append command-line arguments
        if (len_trim(cmd%runner_args)>0) run_cmd = run_cmd//' '//trim(cmd%runner_args)
    end function runner_command

    !> Check name in list ID. return 0 if not found
    integer function name_ID(cmd,name)
       class(fpm_run_settings), intent(in) :: cmd
       character(*), intent(in) :: name
       
       integer :: j
       
       !> Default: not found
       name_ID = 0
       if (.not.allocated(cmd%name)) return
       
       do j=1,size(cmd%name)
          
          if (glob(trim(name),trim(cmd%name(j)))) then 
              name_ID = j
              return
          end if
          
       end do
    
    end function name_ID


end module fpm_command_line
