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
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
use M_CLI2,           only : set_args, lget, sget, unnamed, remaining, specified
use M_CLI2,           only : get_subcommand, CLI_RESPONSE_FILE
use fpm_strings,      only : lower, split, fnv_1a, to_fortran_name, is_fortran_name
use fpm_filesystem,   only : basename, canon_path, which
use fpm_environment,  only : run, get_command_arguments_quoted
use fpm_error,        only : fpm_stop
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
implicit none

private
public :: fpm_cmd_settings, &
          fpm_build_settings, &
          fpm_install_settings, &
          fpm_new_settings, &
          fpm_run_settings, &
          fpm_test_settings, &
          fpm_update_settings, &
          get_command_line_settings

type, abstract :: fpm_cmd_settings
    character(len=:), allocatable :: working_dir
    logical                      :: verbose=.true.
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
    character(len=:),allocatable :: compiler
    character(len=:),allocatable :: c_compiler
    character(len=:),allocatable :: archiver
    character(len=:),allocatable :: profile
    character(len=:),allocatable :: flag
    character(len=:),allocatable :: cflag
    character(len=:),allocatable :: ldflag
end type

type, extends(fpm_build_settings)  :: fpm_run_settings
    character(len=ibug),allocatable :: name(:)
    character(len=:),allocatable :: args
    character(len=:),allocatable :: runner
    character(len=:),allocatable :: env
    logical :: example
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
    logical :: fetch_only
    logical :: clean
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
                 & help_env(:), help_response(:)
character(len=20),parameter :: manual(*)=[ character(len=20) ::&
& ' ', 'fpm', 'new', 'build', 'run', 'test', 'install', 'update', 'help', 'list', &
& 'runner', 'compiler', 'response', 'version', 'toc' ]

character(len=:), allocatable :: val_runner, val_compiler, val_flag, val_cflag, val_ldflag, &
& val_profile, val_env

logical :: verbose
contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

        character(len=4096)           :: cmdarg
        integer                       :: i
        integer                       :: widest
        type(fpm_install_settings), allocatable :: install_settings
        character(len=:), allocatable :: common_args, compiler_args, run_args, working_dir, &
            & c_compiler, archiver
        character(len=*), parameter :: fc_env = "FC", cc_env = "CC", ar_env = "AR", &
            & fflags_env = "FFLAGS", cflags_env = "CFLAGS", ldflags_env = "LDFLAGS", &
            & fc_default = "gfortran", cc_default = " ", ar_default = " ", flags_default = " "
        character(len=:), allocatable :: new_fc_default
        integer :: idum

        call set_help()
        ! text for --version switch,
        select case (get_os_type())
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
        version_text = [character(len=80) :: &
         &  'Version:     0.5.0, alpha',                               &
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
          ' --runner " "'

        ! without moving all calls to get_fpm_env(3f) into the places where compiler_args(3f)
        ! is used, need to process the --env option and optionally the --compiler option before
        ! the actual subcommands are processed, so need to make fax calls to set_args(3f) to
        ! get the values. A little convoluted but gives the desired behavior.

        ! first get compiler from normal command line or FPM_FC.
        val_env='FPM_'
        new_fc_default=get_fpm_env(fc_env, fc_default)
        ! now see if --env option is present using special call to SET_ARGS(3f)
        call set_args('--env "FPM_"',ierr=idum)   ! parse without errors to get value of --env
        val_env=sget('env')
        if(specified('env'))then
           if(val_env.eq.''.and.len(val_env).eq.1) then
              val_env="FPM_"
              call set_args(' --compiler "'//get_fpm_env(fc_env, fc_default)//'"',ierr=idum)
              new_fc_default=sget('compiler')
              val_env='FPM_'//trim(adjustl(basename(sget('compiler'))))//'_'
           elseif(val_env.ne.'')then
              val_env='FPM_'//trim(adjustl(val_env))//"_"
           endif
           new_fc_default=trim(adjustl(new_fc_default))
           verbose=lget('verbose')
           if(verbose)then
              write(*,'(1x,*(g0))') '<INFO> ENVIRONMENT PREFIX:  ',val_env
           endif
           compiler_args = &
             ' --env "dummy"'// &
             ' --profile " "' // &
             ' --compiler "'//get_fpm_env(fc_env, new_fc_default)//'"' // &
             ' --c-compiler "'//get_fpm_env(cc_env, cc_default)//'"' // &
             ' --archiver "'//get_fpm_env(ar_env, ar_default)//'"' // &
             ' --flag:: "'//get_fpm_env(fflags_env, flags_default)//'"' // &
             ' --c-flag:: "'//get_fpm_env(cflags_env, flags_default)//'"' // &
             ' --link-flag:: "'//get_fpm_env(ldflags_env, flags_default)//'"'
        else  ! if --env is not present skip other compiler-related variables
           verbose=lget('verbose')
           compiler_args = &
             ' --profile  " "' // &
             ' --compiler "'//get_fpm_env(fc_env, fc_default)//'"' // &
             ' --c-compiler " "' // &
             ' --archiver " "' // &
             ' --flag:: " "' // &
             ' --c-flag:: " "' // &
             ' --link-flag:: " "'
        endif

        ! now set subcommand-specific help text and process commandline
        ! arguments. Then call subcommand routine
        select case(trim(cmdarg))

        case('run')
            call set_args(common_args // compiler_args // run_args //'&
            & --all F &
            & --example F&
            & --',help_run,version_text)

            call check_build_vals()

            if( size(unnamed) .gt. 1 )then
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
               if(names(i).eq.'..')names(i)='*'
            enddo

            c_compiler = sget('c-compiler')
            archiver = sget('archiver')
            allocate(fpm_run_settings :: cmd_settings)
            val_runner=sget('runner')

            if(specified('runner') .and. val_runner.eq.'')val_runner='echo'
            cmd_settings=fpm_run_settings(&
            & args=remaining,&
            & profile=val_profile,&
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & ldflag=val_ldflag, &
            & example=lget('example'), &
            & list=lget('list'),&
            & build_tests=.false.,&
            & name=names,&
            & runner=val_runner,&
            & verbose=lget('verbose') )

        case('build')
            call set_args(common_args // compiler_args //'&
            & --list F &
            & --show-model F &
            & --tests F &
            & --',help_build,version_text)

            call check_build_vals()

            c_compiler = sget('c-compiler')
            archiver = sget('archiver')
            allocate( fpm_build_settings :: cmd_settings )
            cmd_settings=fpm_build_settings(  &
            & profile=val_profile,&
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
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
                write(stderr,'(*(7x,g0,/))') &
                & '<USAGE> fpm new NAME [[--lib|--src] [--app] [--test] [--example]]|[--full|--bare] [--backfill]'
                call fpm_stop(1,'directory name required')
            case(2)
                name=trim(unnamed(2))
            case default
                write(stderr,'(7x,g0)') &
                & '<USAGE> fpm new NAME [[--lib|--src] [--app] [--test] [--example]]| [--full|--bare] [--backfill]'
                call fpm_stop(2,'only one directory name allowed')
            end select
            !*! canon_path is not converting ".", etc.
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
            if(size(unnamed).lt.2)then
                if(unnamed(1).eq.'help')then
                   unnamed=['   ', 'fpm']
                else
                   unnamed=pack(manual,manual.ne.'toc')
                endif
            elseif(unnamed(2).eq.'manual')then
                unnamed=pack(manual,manual.ne.'toc')
            endif
            widest=256
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
                case('run    ' )
                   help_text=[character(len=widest) :: help_text, help_run]
                case('test   ' )
                   help_text=[character(len=widest) :: help_text, help_test]
                case('install' )
                   help_text=[character(len=widest) :: help_text, help_install]
                case('update ' )
                   help_text=[character(len=widest) :: help_text, help_update]
                case('help   ' )
                   help_text=[character(len=widest) :: help_text, help_help]
                case('list   ' )
                   help_text=[character(len=widest) :: help_text, help_list]
                case('runner ' )
                   help_text=[character(len=widest) :: help_text, help_runner]
                case('compiler')
                   help_text=[character(len=widest) :: help_text, help_env]
                case('response')
                   help_text=[character(len=widest) :: help_text, help_response]
                case('version' )
                   help_text=[character(len=widest) :: help_text, version_text]
                case('toc' )
                   write(*,'(a)')'Available help topics are:'
                   write(*,'(4(a,1x))')manual
                   help_text=[character(len=widest) :: help_text, ' ']
                case default
                   help_text=[character(len=widest) :: help_text, &
                   & '<ERROR> unknown help topic "'//trim(unnamed(i))//'"', &
                   & '' ]
                   write(*,'(a)')'Available topics are:'
                   write(*,'(4(a,1x))')manual
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
            archiver = sget('archiver')
            allocate(install_settings)
            install_settings = fpm_install_settings(&
                list=lget('list'), &
                profile=val_profile,&
                compiler=val_compiler, &
                c_compiler=c_compiler, &
                archiver=archiver, &
                flag=val_flag, &
                cflag=val_cflag, &
                ldflag=val_ldflag, &
                no_rebuild=lget('no-rebuild'), &
                verbose=lget('verbose'))
            call get_char_arg(install_settings%prefix, 'prefix')
            call get_char_arg(install_settings%libdir, 'libdir')
            call get_char_arg(install_settings%bindir, 'bindir')
            call get_char_arg(install_settings%includedir, 'includedir')
            call move_alloc(install_settings, cmd_settings)

        case('list')
            call set_args(common_args // '&
            & --list F&
            &', help_list, version_text)
            call printhelp(help_list_nodash)
            if(lget('list'))then
               call printhelp(help_list_dash)
            endif
        case('test')
            call set_args(common_args // compiler_args // run_args // ' --', &
              help_test,version_text)

            call check_build_vals()

            if( size(unnamed) .gt. 1 )then
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
               if(names(i).eq.'..')names(i)='*'
            enddo

            c_compiler = sget('c-compiler')
            archiver = sget('archiver')
            allocate(fpm_test_settings :: cmd_settings)
            val_runner=sget('runner')
            if(specified('runner') .and. val_runner.eq.'')val_runner='echo'
            cmd_settings=fpm_test_settings(&
            & args=remaining, &
            & profile=val_profile, &
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & ldflag=val_ldflag, &
            & example=.false., &
            & list=lget('list'), &
            & build_tests=.true., &
            & name=names, &
            & runner=val_runner, &
            & verbose=lget('verbose') )

        case('update')
            call set_args(common_args // ' --fetch-only F --clean F', &
                help_update, version_text)

            if( size(unnamed) .gt. 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif

            allocate(fpm_update_settings :: cmd_settings)
            cmd_settings=fpm_update_settings(name=names, &
                fetch_only=lget('fetch-only'), verbose=lget('verbose'), &
                clean=lget('clean'))

        case default

            if(which('fpm-'//cmdarg).ne.'')then
                call run('fpm-'//trim(cmdarg)//' '// get_command_arguments_quoted(),.false.)
            else
                call set_args('&
                & --list F&
                &', help_fpm, version_text)
                ! Note: will not get here if --version or --usage or --help
                ! is present on commandline
                help_text=help_usage
                if(lget('list'))then
                   help_text=help_list_dash
                elseif(len_trim(cmdarg).eq.0)then
                    write(stdout,'(*(a))')'Fortran Package Manager:'
                    write(stdout,'(*(a))')' '
                    call printhelp(help_list_nodash)
                else
                    write(stderr,'(*(a))')'<ERROR> unknown subcommand [', &
                     & trim(cmdarg), ']'
                    call printhelp(help_list_dash)
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
        character(len=:), allocatable :: flags

        val_compiler=sget('compiler')
        if(val_compiler.eq.'') then
            val_compiler='gfortran'
        endif

        val_flag = " " // sget('flag')
        val_cflag = " " // sget('c-flag')
        val_ldflag = " " // sget('link-flag')
        val_profile = sget('profile')

    end subroutine check_build_vals

    subroutine printhelp(lines)
    character(len=:),intent(in),allocatable :: lines(:)
    integer :: iii,ii
        if(allocated(lines))then
           ii=size(lines)
           if(ii .gt. 0 .and. len(lines).gt. 0) then
               write(stdout,'(g0)')(trim(lines(iii)), iii=1, ii)
           else
               write(stdout,'(a)')'<WARNING> *printhelp* output requested is empty'
           endif
        endif
    end subroutine printhelp

    end subroutine get_command_line_settings

    subroutine set_help()
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_list_nodash=[character(len=80) :: &
'USAGE: fpm [ SUBCOMMAND [SUBCOMMAND_OPTIONS] ]                                  ', &
'                                                                                ', &
'valid subcommands are:                                                          ', &
'                                                                                ', &
'  new       Create a new Fortran package directory with sample files            ', &
'  build     Compile the package placing results in the "build" directory        ', &
'  run       Run the local package application programs                          ', &
'  test      Run the test programs                                               ', &
'  update    Update and manage project dependencies                              ', &
'  install   Install project                                                     ', &
'  help      Display help                                                        ', &
'  list      Display this list of subcommand descriptions                        ', &
'                                                                                ', &
' Enter "fpm --list" for a brief list of subcommand options. Enter               ', &
' "fpm help" or "fpm help SUBCOMMAND" for detailed descriptions.                 ', &
' ']
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_list_dash = [character(len=80) :: &
' new NAME [[--lib|--src] [--app] [--test] [--example]]|                         ', &
'          [--full|--bare] [--backfill] [--directory PATH]                       ', &
' build [--profile PROF] [COMPILER_OPTIONS] [--list] [--tests]                   ', &
'           [--directory PATH] [--show-model]                                    ', &
' run [[--target] NAME(s)|--all] [--example] [--profile PROF] [--runner "CMD"]   ', &
'      [COMPILER_OPTIONS] [--list] [--directory PATH] [-- ARGS]                  ', &
' test [[--target] NAME(s)] [--profile PROF] [--runner "CMD"] [--runner "CMD"]   ', &
'      [COMPILER_OPTIONS] [--directory PATH] [--list] [-- ARGS]                  ', &
' update [NAME(s)] [--fetch-only] [--clean] [--directory PATH]                   ', &
' install [--profile PROF] [COMPILER_OPTIONS] [--no-rebuild] [--prefix PATH]     ', &
'         [--directory PATH]                                                     ', &
'where COMPILER_OPTIONS are                                                      ', &
'                                                                                ', &
'    --compiler FC --c-compiler CC --archiver AR                                 ', &
'    --flag FFLAGS --c-flag CFLAGS --link-flag LDFLAGS                           ', &
'    --env PREFIX                                                                ', &
'                                                                                ', &
'In addition, these options are valid on any subcommand:                         ', &
'    --verbose                                                                   ', &
'    --help                                                                      ', &
' ']
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
    help_usage=[character(len=80) :: &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_fpm=[character(len=80) :: &
'NAME                                                                            ', &
'   fpm(1) - A Fortran package manager and build system                          ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
'   fpm SUBCOMMAND [SUBCOMMAND_OPTIONS]                                          ', &
'                                                                                ', &
'   fpm --help|--version|--list                                                  ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' fpm(1) is a package manager that helps you create Fortran projects             ', &
' from source -- it automatically determines dependencies!                       ', &
'                                                                                ', &
' Additionally fpm(1) lets you draw upon other fpm(1) packages in                ', &
' distributed git(1) repositories or subdirectories as if the packages           ', &
' were a basic part of your default programming environment, as well             ', &
' as letting you share your projects with others in a similar manner.            ', &
'                                                                                ', &
' All output goes into the directory "build/" which can generally be             ', &
' removed and rebuilt if required. Note that if external packages are            ', &
' being used you need network connectivity to rebuild from scratch.              ', &
'                                                                                ', &
'SUBCOMMANDS                                                                     ', &
   help_list_nodash, &
'Their syntax is                                                                 ', &
'                                                                                ', &
   help_list_dash, &
'                                                                                ', &
' To customize compiler options, see "fpm help compiler".                        ', &
'                                                                                ', &
'EXAMPLES                                                                        ', &
'   sample commands:                                                             ', &
'                                                                                ', &
'    fpm new mypackage --app --test                                              ', &
'    fpm build --compiler gfortran                                               ', &
'    fpm run                                                                     ', &
'    fpm test                                                                    ', &
'    fpm run --example # if you had an example/ directory                        ', &
'    fpm new --help                                                              ', &
'    fpm run myprogram --profile release -- -x 10 -y 20 --title "my title"       ', &
'    fpm install --prefix ~/.local                                               ', &
'                                                                                ', &
'SEE ALSO                                                                        ', &
'                                                                                ', &
' + The fpm(1) home page is at https://github.com/fortran-lang/fpm               ', &
' + Registered fpm(1) packages are at https://fortran-lang.org/packages          ', &
' + The fpm(1) TOML file format is described at                                  ', &
'   https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md          ', &
'']
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_new=[character(len=80) :: &
'NAME                                                                            ', &
' new(1) - the fpm(1) subcommand to initialize a new project                     ', &
'SYNOPSIS                                                                        ', &
'  fpm new NAME [[--lib|--src] [--app] [--test] [--example]]|                    ', &
'      [--directory PATH] [--full|--bare][--backfill] [--verbose]                ', &
'                                                                                ', &
'  fpm new --help|--version                                                      ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' "fpm new" creates and populates a new programming project directory.           ', &
' It                                                                             ', &
'   o creates a directory with the specified name                                ', &
'   o runs the command "git init" in that directory                              ', &
'   o populates the directory with the default project directories               ', &
'   o adds sample Fortran source files                                           ', &
'                                                                                ', &
' The default file structure (that will be automatically scanned) is             ', &
'                                                                                ', &
'     NAME/                                                                      ', &
'       fpm.toml                                                                 ', &
'       src/            # source for modules, libraries, and procedures          ', &
'           NAME.f90                                                             ', &
'       app/            # source for main programs                               ', &
'           main.f90                                                             ', &
'       test/           # source for test programs                               ', &
'           check.f90                                                            ', &
'       example/        # source for example programs                            ', &
'           demo.f90                                                             ', &
'       include/        # source for include files                               ', &
'                                                                                ', &
' Using this file structure is highly encouraged, particularly for               ', &
' small packages primarily intended to be used as dependencies.                  ', &
'                                                                                ', &
' If you find this restrictive and need to customize the package                 ', &
' structure you will find using the --full switch creates a                      ', &
' heavily annotated manifest file with references to documentation               ', &
' to aid in constructing complex package structures.                             ', &
'                                                                                ', &
' Remember to update the information in the sample "fpm.toml"                    ', &
' file with your name and e-mail address.                                        ', &
'                                                                                ', &
'OPTIONS                                                                         ', &
' NAME   the name of the project directory to create. The name                   ', &
'        must be made of up to 63 ASCII letters, digits, underscores,            ', &
'        or hyphens, and start with a letter.                                    ', &
'                                                                                ', &
' The default is to create the src/, app/, and test/ directories.                ', &
' If any of the following options are specified then only the                    ', &
' selected subdirectories are generated:                                         ', &
'                                                                                ', &
' --lib,--src  create directory src/ and a placeholder module                    ', &
'              named "NAME.f90" for use with subcommand "build".                 ', &
' --app        create directory app/ and a placeholder main                      ', &
'              program for use with subcommand "run".                            ', &
' --test       create directory test/ and a placeholder program                  ', &
'              for use with the subcommand "test". Note that sans                ', &
'              "--lib" it really does not have anything to test.                 ', &
' --example    create directory example/ and a placeholder program               ', &
'              for use with the subcommand "run --example".                      ', &
'              It is only created by default if "--full" is specified.           ', &
' --verbose    print more information                                            ', &
' --backfill   By default the directory must not exist. If this                  ', &
'              option is present the directory may pre-exist and                 ', &
'              only subdirectories and files that do not                         ', &
'              already exist will be created. For example, if you                ', &
'              previously entered "fpm new myname --lib" entering                ', &
'              "fpm new myname -full --backfill" will create any missing         ', &
'              app/, example/, and test/ directories and programs.               ', &
'                                                                                ', &
' --full       By default a minimal manifest file ("fpm.toml") is                ', &
'              created that depends on auto-discovery. With this                 ', &
'              option a much more extensive manifest sample is written           ', &
'              and the example/ directory is created and populated.              ', &
'              It is designed to facilitate creating projects that               ', &
'              depend extensively on non-default build options.                  ', &
'                                                                                ', &
' --bare       A minimal manifest file ("fpm.toml") is created and               ', &
'              "README.md" file is created but no directories or                 ', &
'              Fortran samples are generated.                                    ', &
' -C, --directory PATH  Change working directory to PATH before running          ', &
'                       command.                                                 ', &
' --help       print this help and exit                                          ', &
' --version    print program version information and exit                        ', &
'                                                                                ', &
' So the default is equivalent to                                                ', &
'                                                                                ', &
'    fpm NAME --lib --app --test                                                 ', &
'EXAMPLES                                                                        ', &
' Sample use                                                                     ', &
'                                                                                ', &
'   fpm new myproject  # create new project directory and seed it                ', &
'   cd myproject       # Enter the new directory                                 ', &
'   # and run commands such as                                                   ', &
'   fpm build                                                                    ', &
'   fpm run            # run lone application program                            ', &
'   fpm test           # run test program(s)                                     ', &
'                                                                                ', &
'   fpm new A --full   # create project with example/ and annotated fpm.toml     ', &
'   cd A               # enter project directory                                 ', &
'   fpm run --example  # run lone example program                                ', &
'                                                                                ', &
'   fpm new B --bare   # create no directories                                   ', &
'   cd B               # enter project directory                                 ', &
'   # create any missing files in current directory                              ', &
'   fpm new `pwd` --full --backfill                                              ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_build=[character(len=80) :: &
'NAME                                                                            ', &
' build(1) - the fpm(1) subcommand to build a project                            ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
' fpm build [--profile PROF] [COMPILER_OPTIONS] [--list] [--tests]               ', &
'           [--directory PATH] [--show-model] [--verbose]                        ', &
'                                                                                ', &
' fpm build --help                                                               ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' The "fpm build" command                                                        ', &
'    o Fetches any dependencies                                                  ', &
'    o Scans your sources                                                        ', &
'    o Builds them in the proper order                                           ', &
'                                                                                ', &
' The Fortran source files are assumed by default to be in                       ', &
'    o src/     for modules and procedure source                                 ', &
'    o app/     main program(s) for applications                                 ', &
'    o test/    main program(s) and support files for project tests              ', &
'    o example/ main program(s) for example programs                             ', &
'    o include/ optional directory for INCLUDE files                             ', &
'                                                                                ', &
' Changed or new files found are rebuilt. The results are placed in              ', &
' the build/ directory.                                                          ', &
'                                                                                ', &
' Non-default pathnames and remote dependencies are used if                      ', &
' specified in the "fpm.toml" file.                                              ', &
'                                                                                ', &
'OPTIONS                                                                         ', &
' --profile PROF  selects the compilation profile for the build.                 ', &
'                 Currently available profiles are "release" for                 ', &
'                 high optimization and "debug" for full debug options.          ', &
'                 If --flag is not specified the "debug" flags are the           ', &
'                 default.                                                       ', &
' --list          list candidates instead of building or running them            ', &
' --tests         build all tests (otherwise only if needed)                     ', &
' --directory, -C PATH  Change working directory to PATH before running          ', &
'                       command                                                  ', &
' --show-model    show the model and exit (do not build)                         ', &
' --verbose       print more information                                         ', &
' --help          print this help and exit                                       ', &
'                                                                                ', &
'COMPILER OPTIONS                                                                ', &
' To customize compiler options, see "fpm help compiler".                        ', &
'                                                                                ', &
'EXAMPLES                                                                        ', &
' Sample commands:                                                               ', &
'                                                                                ', &
'  fpm build                   # build with debug options                        ', &
'  fpm build --profile release # build with high optimization                    ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_run=[character(len=80) :: &
'NAME                                                                            ', &
' run(1) - the fpm(1) subcommand to run project applications                     ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
' fpm run [[--target] NAME(s) [--profile PROF] [COMPILER_OPTIONS]                ', &
'         [--runner "CMD"] [--example] [--list] [--all] [-- ARGS]                ', &
'         [--directory PATH] [--verbose]                                         ', &
'                                                                                ', &
' fpm run --help|--version                                                       ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' Run the applications in your fpm(1) package. By default applications           ', &
' in /app or specified as "executable" in your "fpm.toml" manifest are           ', &
' used. Alternatively demonstration programs in example/ or specified in         ', &
' the "example" section in "fpm.toml" can be executed. The applications          ', &
' are automatically rebuilt before being run if they are out of date.            ', &
'                                                                                ', &
'OPTIONS                                                                         ', &
' --target NAME(s)  list of application names to execute. No name is             ', &
'                   required if only one target exists. If no name is            ', &
'                   supplied and more than one candidate exists or a             ', &
'                   name has no match a list is produced and fpm(1)              ', &
'                   exits.                                                       ', &
'                                                                                ', &
'                   Basic "globbing" is supported where "?" represents           ', &
'                   any single character and "*" represents any string.          ', &
'                   Note The glob string normally needs quoted to                ', &
'                   the special characters from shell expansion.                 ', &
' --all             Run all examples or applications. An alias for the           ', &
'                   option --target "*".                                         ', &
' --example         Run example programs instead of applications.                ', &
' --profile PROF    selects the compilation profile for the build.               ', &
'                   Currently available profiles are "release" for high          ', &
'                   optimization and "debug" for full debug options.             ', &
'                   If --flag is not specified the "debug" flags are the         ', &
'                   default.                                                     ', &
' --runner CMD  A command to prefix the program execution paths with.            ', &
'               see "fpm help runner" for further details.                       ', &
' --list     list basenames of candidates instead of running them. Note          ', &
'            out-of-date candidates will still be rebuilt before being           ', &
'            listed.                                                             ', &
' --directory, -C PATH  Change working directory to PATH before running          ', &
'                       command.                                                 ', &
' --verbose         print more information                                       ', &
' -- ARGS    optional arguments to pass to the program(s). The same              ', &
'            arguments are passed to all program names specified.                ', &
'COMPILER OPTIONS                                                                ', &
' To customize compiler options, see "fpm help compiler".                        ', &
'                                                                                ', &
'EXAMPLES                                                                        ', &
' fpm(1) - run or display project applications:                                  ', &
'                                                                                ', &
'  fpm run        # run a target when only one exists or list targets            ', &
'  fpm run --list # list basename of all targets, running nothing.               ', &
'  fpm run "demo*" --list # list target basenames starting with "demo*".         ', &
'  fpm run "psi*" --runner # list target pathnames starting with "psi*".         ', &
'  fpm run --all  # run all targets, no matter how many there are.               ', &
'                                                                                ', &
'  # run default program built or to be built with the compiler command          ', &
'  # "f90". If more than one app exists a list displays and target names         ', &
'  # are required.                                                               ', &
'  fpm run --compiler f90                                                        ', &
'                                                                                ', &
'  # run example programs instead of the application programs.                   ', &
'  fpm run --example "*"                                                         ', &
'                                                                                ', &
'  # run a specific program and pass arguments to the command                    ', &
'  fpm run myprog -- -x 10 -y 20 --title "my title line"                         ', &
'                                                                                ', &
'  # run production version of two applications                                  ', &
'  fpm run --target prg1,prg2 --profile release                                  ', &
'                                                                                ', &
'  # install executables in directory (assuming install(1) exists)               ', &
'  fpm run --runner ''install -b -m 0711 -p -t /usr/local/bin''                  ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_test=[character(len=80) :: &
'NAME                                                                            ', &
' test(1) - the fpm(1) subcommand to run project tests                           ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
' fpm test [[--target] NAME(s)] [--profile PROF] [COMPILER_OPTIONS]              ', &
'          [--runner "CMD"] [--directory PATH ] [--list][-- ARGS] [--verbose]    ', &
'                                                                                ', &
' fpm test --help|--version                                                      ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' Run applications you have built to test your project.                          ', &
'                                                                                ', &
'OPTIONS                                                                         ', &
' --target NAME(s)  optional list of specific test names to execute.             ', &
'                   The default is to run all the tests in test/                 ', &
'                   or the tests listed in the "fpm.toml" file.                  ', &
'                                                                                ', &
'                   Basic "globbing" is supported where "?" represents           ', &
'                   any single character and "*" represents any string.          ', &
'                   Note The glob string normally needs quoted to                ', &
'                   protect the special characters from shell expansion.         ', &
' --profile PROF    selects the compilation profile for the build.               ', &
'                   Currently available profiles are "release" for               ', &
'                   high optimization and "debug" for full debug options.        ', &
'                   If --flag is not specified the "debug" flags are the         ', &
'                   default.                                                     ', &
' --runner CMD      A command to prefix the program execution paths with.        ', &
'                   see "fpm help runner" for further details.                   ', &
' --list            list candidate basenames instead of running them. Note       ', &
'                   they will still be built if not currently up to date.        ', &
' --directory, -C PATH  Change working directory to PATH before running          ', &
'                       command                                                  ', &
' -- ARGS           optional arguments to pass to the test program(s).           ', &
'                   The same arguments are passed to all test names              ', &
'                   specified.                                                   ', &
' --verbose         print more information                                       ', &
'                                                                                ', &
'COMPILER OPTIONS                                                                ', &
' To customize compiler options, see "fpm help compiler".                        ', &
'                                                                                ', &
'EXAMPLES                                                                        ', &
'run tests                                                                       ', &
'                                                                                ', &
' # run default tests in /test or as specified in "fpm.toml"                     ', &
' fpm test                                                                       ', &
'                                                                                ', &
' # run using compiler command "f90"                                             ', &
' fpm test --compiler f90                                                        ', &
'                                                                                ', &
' # run a specific test and pass arguments to the command                        ', &
' fpm test mytest -- -x 10 -y 20 --title "my title line"                         ', &
'                                                                                ', &
' fpm test tst1 tst2 --profile PROF  # run production version of two tests       ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_install=[character(len=80) :: &
'NAME                                                                            ', &
' install(1) - install fpm projects                                              ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
' fpm install [--profile PROF] [COMPILER_OPTIONS] [--list] [--no-rebuild]        ', &
'             [--prefix DIR] [--bindir DIR] [--libdir DIR] [--includedir DIR]    ', &
'             [--verbose]                                                        ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' Subcommand to install fpm projects. Running install will export the            ', &
' current project to the selected prefix, this will by default install all       ', &
' executables (tests and examples are excluded) which are part of the projects.  ', &
' Libraries and module files are only installed for projects requiring the       ', &
' installation of those components in the package manifest.                      ', &
'                                                                                ', &
'OPTIONS                                                                         ', &
' --list            list all installable targets for this project,               ', &
'                   but do not install any of them                               ', &
' --profile PROF    selects the compilation profile for the build.               ', &
'                   Currently available profiles are "release" for               ', &
'                   high optimization and "debug" for full debug options.        ', &
'                   If --flag is not specified the "debug" flags are the         ', &
'                   default.                                                     ', &
' --no-rebuild      do not rebuild project before installation                   ', &
' --prefix DIR      path to installation directory (requires write access),      ', &
'                   the default prefix on Unix systems is $HOME/.local           ', &
'                   and %APPDATA%\local on Windows                               ', &
' --bindir DIR      subdirectory to place executables in (default: bin)          ', &
' --libdir DIR      subdirectory to place libraries and archives in              ', &
'                   (default: lib)                                               ', &
' --includedir DIR  subdirectory to place headers and module files in            ', &
'                   (default: include)                                           ', &
' --directory, -C PATH  Change working directory to PATH before running          ', &
'                       command                                                  ', &
' --verbose         print more information                                       ', &
'                                                                                ', &
'COMPILER OPTIONS                                                                ', &
' To customize compiler options, see "fpm help compiler".                        ', &
'                                                                                ', &
'EXAMPLES                                                                        ', &
'    fpm install --profile release  # Install release version of project:        ', &
'    # Install the project without rebuilding the executables:                   ', &
'    fpm install --no-rebuild                                                    ', &
'    # Install executables to a custom prefix into the exe directory:            ', &
'    fpm install --prefix $PWD --bindir exe                                      ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_update=[character(len=80) :: &
'NAME                                                                            ', &
' update(1) - manage project dependencies                                        ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
' fpm update [--fetch-only] [--clean] [--verbose] [NAME(s)] [--directory PATH]   ', &
' [--verbose]                                                                    ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' Manage and update project dependencies. If no dependency names are             ', &
' provided all the dependencies are updated automatically.                       ', &
'                                                                                ', &
'OPTIONS                                                                         ', &
' --fetch-only  Only fetch dependencies, do not update existing projects         ', &
' --clean       Do not use previous dependency cache                             ', &
' --directory, -C PATH  Change working directory to PATH before running          ', &
'                       command                                                  ', &
' --verbose     Show additional printout                                         ', &
'                                                                                ', &
'SEE ALSO                                                                        ', &
' The fpm(1) home page at https://github.com/fortran-lang/fpm                    ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_help=[character(len=80) :: &
'NAME                                                                            ', &
'   help(1) - the fpm(1) subcommand to display help                              ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
' subcommands                                                                    ', &
'                                                                                ', &
'   fpm help [fpm] [new] [build] [run] [test] [update] [install] [help] [list]   ', &
'                                                                                ', &
' special topics                                                                 ', &
'                                                                                ', &
'   [compiler] [runner] [response] [manual] [toc] [version]                      ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
'  The "fpm help" command displays descriptions on fpm(1) subcommands and        ', &
'  features (such as how to set compiler options).                               ', &
'                                                                                ', &
'  The default is to display help for the fpm(1) command itself.                 ', &
'OPTIONS                                                                         ', &
'   NAME(s)    A list of topic names to display. All the subcommands             ', &
'              have their own page (new, build, run, test, ...).                 ', &
'                                                                                ', &
'              Special topics include                                            ', &
'                + "toc" to generate a list of all topics                        ', &
'                + "manual" displays all the fpm(1) built-in documentation       ', &
'                + "compiler" for the compiler customization options             ', &
'                + "runner" for the --runner options                             ', &
'                + "response" for information on using response files as         ', &
'                  abbreviations for complex command line options.               ', &
'                                                                                ', &
'EXAMPLES                                                                        ', &
'   Sample usage:                                                                ', &
'                                                                                ', &
'     fpm help           # general fpm(1) command help                           ', &
'     # subcommand descriptions                                                  ', &
'     fpm help version   # show program version                                  ', &
'     fpm help new       # display help for "new" subcommand                     ', &
'     # special-purpose topics                                                   ', &
'     fpm help compiler  # customizing compiler options                          ', &
'     fpm help toc       # table of contents of help topics                      ', &
'     fpm help manual    # All fpm(1) built-in documentation                     ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_list=[character(len=80) :: &
'NAME                                                                            ', &
' list(1) - list summary of fpm(1) subcommands                                   ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
' fpm list [-list]                                                               ', &
'                                                                                ', &
' fpm list --help|--version                                                      ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' Display a short description for each fpm(1) subcommand.                        ', &
'                                                                                ', &
'OPTIONS                                                                         ', &
' --list     display a list of command options as well. This is the              ', &
'            same output as generated by "fpm --list".                           ', &
'                                                                                ', &
'EXAMPLES                                                                        ', &
' display a short list of fpm(1) subcommands                                     ', &
'                                                                                ', &
'  fpm list                                                                      ', &
'  fpm --list                                                                    ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
    help_runner=[character(len=80) :: &
'NAME                                                                            ', &
'   --runner(1) - a shared option for specifying an application to launch        ', &
'                 executables.                                                   ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
'   fpm run|test --runner CMD ... -- SUFFIX_OPTIONS                              ', &
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
' -- SUFFIX_OPTIONS  additional options to suffix the command CMD and executable ', &
'                    file names with.                                            ', &
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
'For example                                                                     ', &
'                                                                                ', &
'    fpm test --runner gdb                                                       ', &
'    fpm run --runner "tar cvfz $HOME/bundle.tgz"                                ', &
'    fpm run --runner ldd                                                        ', &
'    fpm run --runner strip                                                      ', &
'    fpm run --runner ''cp -t /usr/local/bin''                                   ', &
'                                                                                ', &
'    # options after executable name can be specified after the -- option        ', &
'    fpm --runner cp run -- /usr/local/bin/                                      ', &
'    # generates commands of the form "cp $FILENAME /usr/local/bin/"             ', &
'                                                                                ', &
'    # bash(1) alias example:                                                    ', &
'    alias fpm-install=\                                                         ', &
'    "fpm run --profile release --runner ''install -vbp -m 0711 -t ~/.local/bin''"', &
'    fpm-install                                                                 ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_env=[character(len=80) :: &
'NAME                                                                            ', &
'   compiler(1) - a description of methods for customizing compiler-related      ', &
'                 options.                                                       ', &
'SYNOPSIS                                                                        ', &
' Use command options to directly specify compiler-related options               ', &
'                                                                                ', &
'   fpm run|test|build|install --compiler FC --c-compiler CC --archiver AR       ', &
'      --flag FFLAGS --c-flag CFLAGS --link-flag LDFLAGS ...                     ', &
'                                                                                ', &
' Select a set of environment variables based on a specified "FPM" prefix        ', &
'                                                                                ', &
'   fpm run|test|build|install --env [MODIFIER] ...                              ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
' By default, the compiler options are automatically selected and depend only on ', &
' the modes selected via the --profile option on build(1), run(1), test(1),      ', &
' build and install(1).                                                          ', &
'                                                                                ', &
' But the compiler as well as the compilation-related options to use             ', &
' may be explicitly specified either by command-line options and/or by sets of   ', &
' environment variables.                                                         ', &
'                                                                                ', &
' Command line options override any default specified with environment variables.', &
'                                                                                ', &
'COMPILER COMMAND LINE OPTIONS                                                   ', &
' --compiler FC      Specify a Fortran compiler name. The default is "gfortran". ', &
' --flag  FFLAGS(*)  Selects compile arguments. These override the defaults      ', &
'                    set by profiles unless --profile is specified in which case ', &
'                    the options are joined.                                     ', &
'                    Note objects and .mod directory locations for files created ', &
'                    in the build/ director are always built in.                 ', &
' --c-compiler CC         C compiler name.                                       ', &
' --c-flag CFLAGS(*)      Select compile arguments specific for C source         ', &
' --link-flag LDFLAGS(*)  Select arguments passed to the linker.                 ', &
' --archiver AR           Archiver name.                                         ', &
'                                                                                ', &
' (*) indicates this option may be repeated multiple times.                      ', &
'                                                                                ', &
'ENVIRONMENT VARIABLES                                                           ', &
'                                                                                ', &
' Each of the above compiler options can have its defaults overridden by a       ', &
' corresponding environment variable.                                            ', &
'                                                                                ', &
' Environment variables are easy to use inadvertently, and in this case the      ', &
' desired values are very specific to a particular compiler -- so the method used', &
' allows for compiler-specific sets of variables to be used and for the sets     ', &
' to be ignored unless specifically invoked.                                     ', &
'                                                                                ', &
' The only environment variable detected by default is FPM_FC, which may         ', &
' be used to override the default compiler name. A full pathname may be used.    ', &
' The basename of the FPM_FC variable will become the default modifier           ', &
' as described below.                                                            ', &
'                                                                                ', &
' All other compiler-related environment variables are ignored unless the        ', &
' "--env" parameter is present on the command line.                              ', &
'                                                                                ', &
' --env MODIFIER  Indicates additional environment variables should be used and  ', &
'                  allows modifying the prefix of the variables searched for.    ', &
'                 The value is appended to "FPM_", an underscore ("_")           ', &
'                 is added, and the resulting prefix is added to the names       ', &
'                 FC, CC, AR, FFLAGS, CFLAGS, and LDFLAGS.                       ', &
'                                                                                ', &
'                 If no value is given the basename of the current compiler      ', &
'                 is assumed.                                                    ', &
'                                                                                ', &
'                 A special case is when MODIFIER is explicitly a blank string.  ', &
'                 In that case, the environment names searched for have no       ', &
'                 prefix, and are simply FC, CC, AR, FFLAGS, CFLAGS, and LDFLAGS.', &
'                                                                                ', &
' The variable names affected (with modifier shown as "*") are:                  ', &
'                                                                                ', &
'    variable name  CLI option   Description                                     ', &
'    -------------  -----------  ----------------------------------------------- ', &
'    FPM_*_FC       --compiler   sets the path to the Fortran compiler           ', &
'    FPM_*_FFLAGS   --flag       sets the arguments for the Fortran compiler     ', &
'    FPM_*_CC       --c-compiler sets the path to the C compiler                 ', &
'    FPM_*_CFLAGS   --c-flag     sets the arguments for the C compiler           ', &
'    FPM_*_AR       --archiver   sets the path to the archiver                   ', &
'    FPM_*_LDFLAGS  --link-flag  sets additional link arguments for creating     ', &
'                                executables                                     ', &
'EXAMPLE                                                                         ', &
' This can be used to change the defaults for a specific compiler, or to use     ', &
' variable names without the FPM_ prefix.                                        ', &
'                                                                                ', &
'     fpm -compiler ifort   # specify compiler using command-line                ', &
'     export FPM_FC=_nagfor # change default compiler using environment          ', &
'                           # variable in bash(1) shell.                         ', &
'     export FPM_TIME_FC=ifort                                                   ', &
'     export FPM_TIME_FFLAGS="-p"                                                ', &
'     export FPM_TIME_CC=icc                                                     ', &
'     export FPM_TIME_CFLAGS=""                                                  ', &
'     export FPM_TIME_AR=ar                                                      ', &
'     export FPM_TIME_LDFLAGS=""                                                 ', &
'     fpm build -env TIME # use the prefix "FPM_TIME_".                          ', &
'     fpm build -env ""  # use a null (no spaces!) to use no prefix and thus     ', &
'                        # to simply use FC, CC, AR, FFLAGS, CFLAGS, and LDFLAGS.', &
'     fpm build -env     # use FPM_"compiler_name"_ as the prefix for            ', &
'                        # environment variables                                 ', &
'' ]
!12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   help_response=[character(len=80) :: &
'NAME                                                                            ', &
'  response(1) - Using response files on the command line                        ', &
'                                                                                ', &
'SYNOPSIS                                                                        ', &
'  fpm SUBCOMMAND @name1 @name2 ... [COMMAND_OPTIONS]                            ', &
'                                                                                ', &
'DESCRIPTION                                                                     ', &
'  A response file can be used to abbreviate long command line options.          ', &
'  You may replace the default options for the fpm(1) command from a             ', &
'  file if your leading options begin with @file. Initial options will           ', &
'  then be read from the "response file" "file.rsp" in the current               ', &
'  project directory.                                                            ', &
'                                                                                ', &
'  If "file" does not exist or cannot be read, then an error occurs and          ', &
'  the program stops. Each line of the file is prefixed with "options"           ', &
'  and interpreted as a separate argument. The file itself may not               ', &
'  contain @file arguments. That is, it is not processed recursively.            ', &
'                                                                                ', &
'  For more information on response files see                                    ', &
'                                                                                ', &
'     https://urbanjost.github.io/M_CLI2/set_args.3m_cli2.html                   ', &
'                                                                                ', &
'  The basic functionality described here will remain the same, but              ', &
'  other features described at the above reference may change.                   ', &
'                                                                                ', &
'  An example response file:                                                     ', &
'                                                                                ', &
'     # my build options                                                         ', &
'     options build                                                              ', &
'     options --compiler gfortran                                                ', &
'     options --flag "-pg -pthread -L/usr/X11R6/lib -L/usr/X11R6/lib64 -lX11"    ', &
'                                                                                ', &
'  Note response files do not (currently) allow for continued lines or multiple  ', &
'  specifications of the same option.                                            ', &
'' ]
    end subroutine set_help

    subroutine get_char_arg(var, arg)
      character(len=:), allocatable, intent(out) :: var
      character(len=*), intent(in) :: arg
      var = sget(arg)
      if (len_trim(var) == 0) deallocate(var)
    end subroutine get_char_arg

    !> Get an environment variable for fpm, this routine ensures that every variable
    !> used by fpm is prefixed with VAL_ENV (which is "FPM_" by default).
    function get_fpm_env(env, default) result(val)
      character(len=*), intent(in) :: env
      character(len=*), intent(in) :: default
      character(len=:), allocatable :: val

         val = get_env(val_env//env, default)
         if(verbose)then
            write(*,'(1x,*(g0))')'<INFO> ENV:'//val_env//env//'==>'//val
         endif

    end function get_fpm_env

end module fpm_command_line
