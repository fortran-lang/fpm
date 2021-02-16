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
!> Add this point you should create a help page for the new command in a simple
!> catman-like format as well in the ``set_help`` procedure.
!> Make sure to register new subcommands in the ``fpm-manual`` command by adding
!> them to the manual character array and in the help/manual case as well.
!> You should add the new command to the synopsis section of the ``fpm-list``,
!> ``fpm-help`` and ``fpm --list`` help pages below to make sure the help output
!> is complete and consistent as well.
module fpm_command_line
use fpm_environment,  only : get_os_type, get_env, &
                             OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD
use M_CLI2,           only : set_args, lget, sget, unnamed, remaining, specified
use fpm_strings,      only : lower, split
use fpm_filesystem,   only : basename, canon_path, to_fortran_name, which
use fpm_environment,  only : run, get_command_arguments_quoted
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
    logical                      :: verbose=.true.
end type

integer,parameter :: ibug=4096
type, extends(fpm_cmd_settings)  :: fpm_new_settings
    character(len=:),allocatable :: name
    logical                      :: with_executable=.false.
    logical                      :: with_test=.false.
    logical                      :: with_lib=.true.
    logical                      :: backfill=.true.
end type

type, extends(fpm_cmd_settings)  :: fpm_build_settings
    logical                      :: list=.false.
    logical                      :: show_model=.false.
    character(len=:),allocatable :: compiler
    character(len=:),allocatable :: build_name
end type

type, extends(fpm_build_settings)  :: fpm_run_settings
    character(len=ibug),allocatable :: name(:)
    character(len=:),allocatable :: args
    character(len=:),allocatable :: runner
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
                 & help_list(:), help_list_dash(:), help_list_nodash(:)
character(len=20),parameter :: manual(*)=[ character(len=20) ::&
&  ' ',     'fpm',     'new',   'build',  'run',     &
&  'test',  'runner', 'install', 'update', 'list',   'help',   'version'  ]

character(len=:), allocatable :: val_runner, val_build, val_compiler

contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

        character(len=4096)           :: cmdarg
        integer                       :: i
        integer                       :: widest
        type(fpm_install_settings), allocatable :: install_settings

        call set_help()
        ! text for --version switch,
        select case (get_os_type())
            case (OS_LINUX);   os_type =  "OS Type:     Linux"
            case (OS_MACOS);   os_type =  "OS Type:     macOS"
            case (OS_WINDOWS); os_type =  "OS Type:     Windows"
            case (OS_CYGWIN);  os_type =  "OS Type:     Cygwin"
            case (OS_SOLARIS); os_type =  "OS Type:     Solaris"
            case (OS_FREEBSD); os_type =  "OS Type:     FreeBSD"
            case (OS_UNKNOWN); os_type =  "OS Type:     Unknown"
            case default     ; os_type =  "OS Type:     UNKNOWN"
        end select
        version_text = [character(len=80) :: &
         &  'Version:     0.1.3, alpha',                           &
         &  'Program:     fpm(1)',                                     &
         &  'Description: A Fortran package manager and build system', &
         &  'Home Page:   https://github.com/fortran-lang/fpm',        &
         &  'License:     MIT',                                        &
         &  os_type]
        ! find the subcommand name by looking for first word on command
        ! not starting with dash
        cmdarg = ''
        do i = 1, command_argument_count()
            call get_command_argument(i, cmdarg)
            if(adjustl(cmdarg(1:1)) .ne. '-')exit
        enddo

        ! now set subcommand-specific help text and process commandline
        ! arguments. Then call subcommand routine
        select case(trim(cmdarg))

        case('run')
            call set_args('&
            & --target " " &
            & --list F &
            & --release F&
            & --example F&
            & --runner " " &
            & --compiler "'//get_env('FPM_COMPILER','gfortran')//'" &
            & --verbose F&
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

            allocate(fpm_run_settings :: cmd_settings)
            val_runner=sget('runner')
            cmd_settings=fpm_run_settings(&
            & args=remaining,&
            & build_name=val_build,&
            & compiler=val_compiler, &
            & example=lget('example'), &
            & list=lget('list'),&
            & name=names,&
            & runner=val_runner,&
            & verbose=lget('verbose') )

        case('build')
            call set_args( '&
            & --release F &
            & --list F &
            & --show-model F &
            & --compiler "'//get_env('FPM_COMPILER','gfortran')//'" &
            & --verbose F&
            & --',help_build,version_text)

            call check_build_vals()

            allocate( fpm_build_settings :: cmd_settings )
            cmd_settings=fpm_build_settings(  &
            & build_name=val_build,&
            & compiler=val_compiler, &
            & list=lget('list'),&
            & show_model=lget('show-model'),&
            & verbose=lget('verbose') )

        case('new')
            call set_args('&
            & --src F &
            & --lib F &
            & --app F &
            & --test F &
            & --backfill F&
            & --verbose F',&
            & help_new, version_text)
            select case(size(unnamed))
            case(1)
                write(stderr,'(*(g0,/))')'<ERROR> directory name required'
                write(stderr,'(*(7x,g0,/))') &
                & '<USAGE> fpm new NAME [--lib|--src] [--app] [--test] [--backfill]'
                stop 1
            case(2)
                name=trim(unnamed(2))
            case default
                write(stderr,'(g0)')'<ERROR> only one directory name allowed'
                write(stderr,'(7x,g0)') &
                & '<USAGE> fpm new NAME [--lib|--src] [--app] [--test] [--backfill]'
                stop 2
            end select
            !*! canon_path is not converting ".", etc.
            name=canon_path(name)
            if( .not.is_fortran_name(to_fortran_name(basename(name))) )then
                write(stderr,'(g0)') [ character(len=72) :: &
                & '<ERROR> the fpm project name must be made of up to 63 ASCII letters,', &
                & '        numbers, underscores, or hyphens, and start with a letter.']
                stop 4
            endif

            allocate(fpm_new_settings :: cmd_settings)

            if (any( specified(['src ','lib ','app ','test']) ) )then
                cmd_settings=fpm_new_settings(&
                 & backfill=lget('backfill'),               &
                 & name=name,                               &
                 & with_executable=lget('app'),             &
                 & with_lib=any([lget('lib'),lget('src')]), &
                 & with_test=lget('test'),                  &
                 & verbose=lget('verbose') )
            else
                cmd_settings=fpm_new_settings(&
                 & backfill=lget('backfill') ,           &
                 & name=name,                            &
                 & with_executable=.true.,               &
                 & with_lib=.true.,                      &
                 & with_test=.true.,                     &
                 & verbose=lget('verbose') )
            endif

        case('help','manual')
            call set_args('&
            & --verbose F &
            & ',help_help,version_text)
            if(size(unnamed).lt.2)then
                if(unnamed(1).eq.'help')then
                   unnamed=['   ', 'fpm']
                else
                   unnamed=manual
                endif
            elseif(unnamed(2).eq.'manual')then
                unnamed=manual
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
                case default
                   help_text=[character(len=widest) :: help_text, &
                   & '<ERROR> unknown help topic "'//trim(unnamed(i))//'"']
                   !!& '<ERROR> unknown help topic "'//trim(unnamed(i)).'not found in:',manual]
                end select
            enddo
            call printhelp(help_text)

        case('install')
            call set_args('--release F --no-rebuild F --verbose F --prefix " " &
                & --list F &
                & --compiler "'//get_env('FPM_COMPILER','gfortran')//'" &
                & --libdir "lib" --bindir "bin" --includedir "include"', &
                help_install, version_text)

            call check_build_vals()

            allocate(install_settings)
            install_settings = fpm_install_settings(&
                list=lget('list'), &
                build_name=val_build, &
                compiler=val_compiler, &
                no_rebuild=lget('no-rebuild'), &
                verbose=lget('verbose'))
            call get_char_arg(install_settings%prefix, 'prefix')
            call get_char_arg(install_settings%libdir, 'libdir')
            call get_char_arg(install_settings%bindir, 'bindir')
            call get_char_arg(install_settings%includedir, 'includedir')
            call move_alloc(install_settings, cmd_settings)

        case('list')
            call set_args('&
            & --list F&
            & --verbose F&
            &', help_list, version_text)
            call printhelp(help_list_nodash)
            if(lget('list'))then
               call printhelp(help_list_dash)
            endif
        case('test')
            call set_args('&
            & --target " " &
            & --list F&
            & --release F&
            & --runner " " &
            & --compiler "'//get_env('FPM_COMPILER','gfortran')//'" &
            & --verbose F&
            & --',help_test,version_text)

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

            allocate(fpm_test_settings :: cmd_settings)
            val_runner=sget('runner')
            cmd_settings=fpm_test_settings(&
            & args=remaining, &
            & build_name=val_build, &
            & compiler=val_compiler, &
            & example=.false., &
            & list=lget('list'), &
            & name=names, &
            & runner=val_runner, &
            & verbose=lget('verbose') )

        case('update')
            call set_args('--fetch-only F --verbose F --clean F', &
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
                write(stderr,'(*(a))')'<CALLING> command [', 'fpm-'//trim(cmdarg)//' '//get_command_arguments_quoted(), ']'
                call run('fpm-'//trim(cmdarg)//' '// get_command_arguments_quoted() )
            else
               call set_args('&
               & --list F&
               & --verbose F&
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
    contains

    subroutine check_build_vals()

        val_compiler=sget('compiler')
        if(val_compiler.eq.'') then
            val_compiler='gfortran'
        endif

        val_build=trim(merge('release','debug  ',lget('release')))

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

    function is_fortran_name(line) result (lout)
    ! determine if a string is a valid Fortran name ignoring trailing spaces
    ! (but not leading spaces)
    character(len=*),parameter   :: int='0123456789'
    character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=*),parameter   :: allowed=upper//lower//int//'_'
    character(len=*),intent(in)  :: line
    character(len=:),allocatable :: name
    logical                      :: lout
        name=trim(line)
        if(len(name).ne.0)then
            lout = .true.                                  &
             & .and. verify(name(1:1), lower//upper) == 0  &
             & .and. verify(name,allowed) == 0             &
             & .and. len(name) <= 63
        else
            lout = .false.
        endif
    end function is_fortran_name

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
   '                                                                        ', &
   ' Enter "fpm --list" for a brief list of subcommand options. Enter       ', &
   ' "fpm --help" or "fpm SUBCOMMAND --help" for detailed descriptions.     ', &
   ' ']
   help_list_dash = [character(len=80) :: &
   '                                                                                ', &
   ' build [--compiler COMPILER_NAME] [--release] [--list]                          ', &
   ' help [NAME(s)]                                                                 ', &
   ' new NAME [--lib|--src] [--app] [--test] [--backfill]                           ', &
   ' update [NAME(s)] [--fetch-only] [--clean] [--verbose]                          ', &
   ' list [--list]                                                                  ', &
   ' run  [[--target] NAME(s)] [--release] [--runner "CMD"] [--list] [--example]    ', &
   '      [--compiler COMPILER_NAME] [-- ARGS]                                      ', &
   ' test [[--target] NAME(s)] [--release] [--runner "CMD"] [--list]                ', &
   '      [--compiler COMPILER_NAME] [-- ARGS]                                      ', &
   ' install [--release] [--no-rebuild] [--prefix PATH] [options]                   ', &
   ' ']
    help_usage=[character(len=80) :: &
    '' ]
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
   '                                                                                ', &
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
   ' For example                                                                    ', &
   '                                                                                ', &
   '  fpm test --runner gdb                                                         ', &
   '  fpm run --runner "tar cvfz $HOME/bundle.tgz"                                  ', &
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
   '  "fpm run --release --runner ''install -vbp -m 0711 -t ~/.local/bin''"           ', &
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
    '   from source.                                                        ', &
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
    '  + build Compile the packages into the "build/" directory.            ', &
    '  + new   Create a new Fortran package directory with sample files.    ', &
    '  + update  Update the project dependencies.                           ', &
    '  + run   Run the local package binaries. defaults to all binaries for ', &
    '          that release.                                                ', &
    '  + test  Run the tests.                                               ', &
    '  + help  Alternate method for displaying subcommand help.             ', &
    '  + list  Display brief descriptions of all subcommands.               ', &
    '  + install Install project                                            ', &
    '                                                                       ', &
    '  Their syntax is                                                      ', &
    '                                                                       ', &
    '     build [--release] [--list] [--compiler COMPILER_NAME]             ', &
    '     new NAME [--lib|--src] [--app] [--test] [--backfill]              ', &
    '     update [NAME(s)] [--fetch-only] [--clean]                         ', &
    '     run|test [[--target] NAME(s)] [--release] [--list]                ', &
    '              [--runner "CMD"] [--compiler COMPILER_NAME] [-- ARGS]    ', &
    '     help [NAME(s)]                                                    ', &
    '     list [--list]                                                     ', &
    '     install [--release] [--no-rebuild] [--prefix PATH] [options]      ', &
    '                                                                       ', &
    'SUBCOMMAND OPTIONS                                                     ', &
    '  --release  Builds or runs in release mode (versus debug mode). fpm(1)', &
    '             Defaults to using common compiler debug flags and building', &
    '             in "build/*_debug/". When this flag is present build      ', &
    '             output goes into "build/*_release/" and common compiler   ', &
    '             optimization flags are used.                              ', &
    '  --list     List candidates instead of building or running them. On   ', &
    '             the fpm(1) command this shows a brief list of subcommands.', &
    '  --runner CMD   Provides a command to prefix program execution paths. ', &
    '  --compiler COMPILER_NAME  Compiler name. The environment variable    ', &
    '                            FPM_COMPILER sets the default.             ', &
    '  -- ARGS    Arguments to pass to executables.                         ', &
    '                                                                       ', &
    'VALID FOR ALL SUBCOMMANDS                                              ', &
    '  --help     Show help text and exit                                   ', &
    '  --verbose  Display additional information when available             ', &
    '  --version  Show version information and exit.                        ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    '   sample commands:                                                    ', &
    '                                                                       ', &
    '    fpm new mypackage --app --test                                     ', &
    '    fpm build                                                          ', &
    '    fpm test                                                           ', &
    '    fpm run                                                            ', &
    '    fpm new --help                                                     ', &
    '    fpm run myprogram --release -- -x 10 -y 20 --title "my title"      ', &
    '    fpm install --prefix ~/.local                                      ', &
    '                                                                       ', &
    'SEE ALSO                                                               ', &
    '                                                                       ', &
    ' + The fpm(1) home page is at https://github.com/fortran-lang/fpm               ', &
    ' + Registered fpm(1) packages are at https://fortran-lang.org/packages          ', &
    ' + The fpm(1) TOML file format is described at                                  ', &
    '   https://github.com/fortran-lang/fpm/blob/master/manifest-reference.md        ', &
    '']
    help_list=[character(len=80) :: &
    'NAME                                                                   ', &
    ' list(1) - list summary of fpm(1) subcommands                          ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm list [-list]                                                      ', &
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
    ' fpm run [[--target] NAME(s)][--release][--compiler COMPILER_NAME]     ', &
    '         [--runner "CMD"] [--example] [--list][-- ARGS]                ', &
    '                                                                       ', &
    ' fpm run --help|--version                                              ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' Run applications you have built in your fpm(1) project.               ', &
    ' By default applications specified in as "executable" in your package  ', &
    ' manifest are used, alternatively also demonstration programs under    ', &
    ' "example" can be used with this subcommand.                           ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' --target NAME(s)  optional list of specific names to execute.         ', &
    '                   The default is to run all the applications in app/  ', &
    '                   or the programs listed in the "fpm.toml" file.      ', &
    ' --example  run example programs instead of applications               ', &
    ' --release  selects the optimized build instead of the debug           ', &
    '            build.                                                     ', &
    ' --compiler COMPILER_NAME  Specify a compiler name. The default is     ', &
    '                           "gfortran" unless set by the environment    ', &
    '                           variable FPM_COMPILER.                      ', &
    ' --runner CMD  A command to prefix the program execution paths with.   ', &
    '               see "fpm help runner" for further details.              ', &
    ' --list     list candidates instead of building or running them        ', &
    ' -- ARGS    optional arguments to pass to the program(s).              ', &
    '            The same arguments are passed to all names                 ', &
    '            specified.                                                 ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' fpm(1) "run" project applications                                     ', &
    '                                                                       ', &
    '  # run default programs in /app or as specified in "fpm.toml"         ', &
    '  fpm run                                                              ', &
    '                                                                       ', &
    '  # run default programs in /app or as specified in "fpm.toml"         ', &
    '  # using the compiler command "f90".                                  ', &
    '  fpm run --compiler f90                                               ', &
    '                                                                       ', &
    '  # run example and demonstration programs instead of the default      ', &
    '  # application programs (specified in "fpm.toml")                     ', &
    '  fpm run --example                                                    ', &
    '                                                                       ', &
    '  # run a specific program and pass arguments to the command           ', &
    '  fpm run mytest -- -x 10 -y 20 --title "my title line"                ', &
    '                                                                       ', &
    '  # run production version of two applications                         ', &
    '  fpm run --target prg1,prg2 --release                                 ', &
    '                                                                       ', &
    '  # install executables in directory (assuming install(1) exists)      ', &
    '  fpm run --runner ''install -b -m 0711 -p -t /usr/local/bin''         ', &
    '' ]
    help_build=[character(len=80) :: &
    'NAME                                                                   ', &
    ' build(1) - the fpm(1) subcommand to build a project                   ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm build [--release][--compiler COMPILER_NAME] [-list]               ', &
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
    ' Changed or new files found are rebuilt. The results are placed in     ', &
    ' the build/ directory.                                                 ', &
    '                                                                       ', &
    ' Non-default pathnames and remote dependencies are used if             ', &
    ' specified in the "fpm.toml" file.                                     ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' --release    build in build/*_release instead of build/*_debug with   ', &
    '              high optimization instead of full debug options.         ', &
    ' --compiler   COMPILER_NAME  Specify a compiler name. The default is   ', &
    '                           "gfortran" unless set by the environment    ', &
    '                           variable FPM_COMPILER.                      ', &
    ' --list       list candidates instead of building or running them      ', &
    ' --show-model show the model and exit (do not build)                   ', &
    ' --help       print this help and exit                                 ', &
    ' --version    print program version information and exit               ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' Sample commands:                                                      ', &
    '                                                                       ', &
    '  fpm build           # build with debug options                       ', &
    '  fpm build --release # build with high optimization                   ', &
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
    'SYNOPSIS                                                               ', &
    ' fpm new NAME [--lib|--src] [--app] [--test] [--backfill]              ', &
    '                                                                       ', &
    ' fpm new --help|--version                                              ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' "fpm new" creates and populates a new programming project directory.  ', &
    ' It                                                                    ', &
    '   o creates a directory with the specified name                       ', &
    '   o runs the command "git init" in that directory                     ', &
    '   o populates the directory with the default project directories      ', &
    '   o adds sample Fortran source files                                  ', &
    '   o adds a ".gitignore" file for ignoring the build/ directory        ', &
    '     (where fpm-generated output will be placed)                       ', &
    '                                                                       ', &
    ' The basic default file structure is                                   ', &
    '                                                                       ', &
    '     NAME/                                                             ', &
    '       fpm.toml                                                        ', &
    '       .gitignore                                                      ', &
    '       src/                                                            ', &
    '           NAME.f90                                                    ', &
    '       app/                                                            ', &
    '           main.f90                                                    ', &
    '       test/                                                           ', &
    '           main.f90                                                    ', &
    '                                                                       ', &
    ' Remember to update the information in the sample "fpm.toml"           ', &
    ' file with your name and e-mail address.                               ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' NAME   the name of the project directory to create. The name          ', &
    '        must be made of up to 63 ASCII letters, digits, underscores,   ', &
    '        or hyphens, and start with a letter.                           ', &
    '                                                                       ', &
    ' The default is to create all of the src/, app/, and test/             ', &
    ' directories. If any of the following options are specified            ', &
    ' then only selected subdirectories are generated:                      ', &
    '                                                                       ', &
    ' --lib,--src  create directory src/ and a placeholder module           ', &
    '              named "NAME.f90" for use with subcommand "build".        ', &
    ' --app        create directory app/ and a placeholder main             ', &
    '              program for use with subcommand "run".                   ', &
    ' --test       create directory test/ and a placeholder program         ', &
    '              for use with the subcommand "test". Note that sans       ', &
    '              "--lib" it really does not have anything to test.        ', &
    '                                                                       ', &
    ' So the default is equivalent to "fpm NAME --lib --app --test".        ', &
    '                                                                       ', &
    ' --backfill   By default the directory must not exist. If this         ', &
    '              option is present the directory may pre-exist and        ', &
    '              only subdirectories and files that do not                ', &
    '              already exist will be created. For example, if you       ', &
    '              previously entered "fpm new myname --lib" entering       ', &
    '              "fpm new myname --backfill" will create the missing      ', &
    '              app/ and test/ directories and programs.                 ', &
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
    '   fpm run            # run example application program                ', &
    '   fpm test           # run example test program                       ', &
    '' ]
    help_test=[character(len=80) :: &
    'NAME                                                                   ', &
    ' test(1) - the fpm(1) subcommand to run project tests                  ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm test [[--target] NAME(s)][--release][--compiler COMPILER_NAME ]   ', &
    '          [--runner "CMD"] [--list][-- ARGS]                           ', &
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
    ' --release  selects the optimized build instead of the debug           ', &
    '            build.                                                     ', &
    ' --compiler COMPILER_NAME  Specify a compiler name. The default is     ', &
    '                           "gfortran" unless set by the environment    ', &
    '                           variable FPM_COMPILER.                      ', &
    ' --runner CMD  A command to prefix the program execution paths with.   ', &
    '               see "fpm help runner" for further details.              ', &
    ' --list     list candidates instead of building or running them        ', &
    ' -- ARGS    optional arguments to pass to the test program(s).         ', &
    '            The same arguments are passed to all test names            ', &
    '            specified.                                                 ', &
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
    ' fpm test tst1 tst2 --release # run production version of two tests    ', &
    '' ]
    help_update=[character(len=80) :: &
    'NAME', &
    ' update(1) - manage project dependencies', &
    '', &
    'SYNOPSIS', &
    ' fpm update [--fetch-only] [--clean] [--verbose] [NAME(s)]', &
    '', &
    'DESCRIPTION', &
    ' Manage and update project dependencies. If no dependency names are', &
    ' provided all the dependencies are updated automatically.', &
    '', &
    'OPTIONS', &
    ' --fetch-only  Only fetch dependencies, do not update existing projects', &
    ' --clean       Do not use previous dependency cache', &
    ' --verbose     Show additional printout', &
    '', &
    'SEE ALSO', &
    ' The fpm(1) home page at https://github.com/fortran-lang/fpm', &
    '' ]
    help_install=[character(len=80) :: &
    'NAME', &
    ' install(1) - install fpm projects', &
    '', &
    'SYNOPSIS', &
    ' fpm install [--release] [--list] [--no-rebuild] [--prefix DIR]', &
    '             [--bindir DIR] [--libdir DIR] [--includedir DIR]', &
    '             [--verbose]', &
    '', &
    'DESCRIPTION', &
    ' Subcommand to install fpm projects. Running install will export the', &
    ' current project to the selected prefix, this will by default install all', &
    ' executables (test and examples are excluded) which are part of the projects.', &
    ' Libraries and module files are only installed for projects requiring the', &
    ' installation of those components in the package manifest.', &
    '', &
    'OPTIONS', &
    ' --list            list all installable targets for this project,', &
    '                   but do not install any of them', &
    ' --release         selects the optimized build instead of the debug build', &
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
    'EXAMPLES', &
    ' 1. Install release version of project:', &
    '', &
    '    fpm install --release', &
    '', &
    ' 2. Install the project without rebuilding the executables:', &
    '', &
    '    fpm install --no-rebuild', &
    '', &
    ' 3. Install executables to a custom prefix into the exe directory:', &
    '', &
    '    fpm install --prefix $PWD --bindir exe', &
    '' ]
    end subroutine set_help

    subroutine get_char_arg(var, arg)
      character(len=:), allocatable, intent(out) :: var
      character(len=*), intent(in) :: arg
      var = sget(arg)
      if (len_trim(var) == 0) deallocate(var)
    end subroutine get_char_arg

end module fpm_command_line
