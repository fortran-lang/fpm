module fpm_command_line
use fpm_environment,  only : get_os_type, &
                             OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD
use M_CLI2,           only : set_args, lget, unnamed, remaining, specified
use fpm_filesystem,   only : basename
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
          get_command_line_settings

type, abstract :: fpm_cmd_settings
end type

integer,parameter :: ibug=4096
type, extends(fpm_cmd_settings)  :: fpm_new_settings
    character(len=:),allocatable :: name
    logical                      :: with_executable=.false.
    logical                      :: with_test=.false.
    logical                      :: with_lib=.true.
end type

type, extends(fpm_cmd_settings)  :: fpm_build_settings
    logical                      :: release=.false.
    logical                      :: list=.false.
end type

type, extends(fpm_cmd_settings)  :: fpm_run_settings
    character(len=ibug),allocatable :: name(:)
    logical                      :: release=.false.
    logical                      :: list=.false.
    character(len=:),allocatable :: args
end type

type, extends(fpm_cmd_settings)  :: fpm_test_settings
    character(len=ibug),allocatable :: name(:)
    logical                      :: release=.false.
    logical                      :: list=.false.
    character(len=:),allocatable :: args
end type

type, extends(fpm_cmd_settings)  :: fpm_install_settings
end type

character(len=:),allocatable :: name
character(len=ibug),allocatable :: names(:)

character(len=:), allocatable :: version_text(:)
character(len=:), allocatable :: help_new(:), help_fpm(:), help_run(:), help_test(:), help_build(:) 
character(len=:), allocatable :: help_text(:), help_install(:), help_help(:)

contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

        character(len=4096)           :: cmdarg
        integer                       :: i

        call set_help()
        ! text for --version switch,
        version_text = [character(len=80) :: &
           &  'Version:     0.1.0, Pre-alpha',                              &
           &  'Program:     fpm(1)',                                        &
           &  'Description: A Fortran package manager and build system',    &
           &  'Home Page:   https://github.com/fortran-lang/fpm',           &
           &  'License:     MIT',                                           &
           &  '']
        ! find the subcommand name by looking for first word on command not starting with dash
        cmdarg = ''
        do i = 1, command_argument_count()
            call get_command_argument(i, cmdarg)
            if(adjustl(cmdarg(1:1)) .ne. '-')exit
        enddo

        ! now set subcommand-specific help text and process commandline arguments. Then call subcommand routine
        select case(trim(cmdarg))

        case('run')
            call set_args('--list F --release F --',help_run,version_text)

            if( size(unnamed) .gt. 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif

            allocate(fpm_run_settings :: cmd_settings)
            cmd_settings=fpm_run_settings( name=names, list=lget('list'), &
            & release=lget('release'), args=remaining )

        case('build')
            call set_args( '--release F --list F --',help_build,version_text )

            allocate( fpm_build_settings :: cmd_settings )
            cmd_settings=fpm_build_settings( release=lget('release'),list=lget('list') )

        case('new')
            call set_args(' --with-executable F --with-test F --lib F --app F --test F', help_new, version_text)
            select case(size(unnamed))
            case(1)
                write(stderr,'(*(g0))')'ERROR: directory name required'
                write(stderr,'(*(g0))')'       usage: fpm new NAME [--with-executable] [--with-test]'
                stop 1
            case(2)
                name=trim(unnamed(2))
            case default
                write(stderr,'(*(g0))')'ERROR: only one directory name allowed'
                write(stderr,'(*(g0))')'       usage: fpm new NAME [--with-executable] [--with-test]'
                stop 2
            end select

            if( .not.is_fortran_name(basename(name)) )then
                write(stderr,'(*(g0))')'ERROR: the new directory basename must be an allowed Fortran name.'
                write(stderr,'(*(g0))')'       It must be composed of 1 to 63 ASCII characters and start'
                write(stderr,'(*(g0))')'       with a letter and be composed entirely of alphanumeric'
                write(stderr,'(*(g0))')'       characters [A-Za-z0-9] and underscores.'
                stop 4
            endif

            allocate(fpm_new_settings :: cmd_settings)
            cmd_settings=fpm_new_settings(name=name,    &
             & with_executable=lget('with-executable'), &
             & with_test=lget('with-test'),             &
             & with_lib=.true.)

             ! use alternative --lib --test --app switches. In production pick one
             ! method or probably an error should be using --with and one of these
            if (any( specified(['lib ','app ','test']) ) )then
                if (any( specified(['with-executable','with-test      ']) ) )then
                   write(stderr,'(*(g0))') 'A BIT FROWARD:'
                   write(stderr,'(*(g0))') '   DO NOT MIX --with-* with [--lib|--app|--test]'
                   write(stderr,'(*(g0))') '   THEY ARE TWO DIFFERENT PROTOTYPE PROPOSALS   '
                   write(stderr,'(*(g0))') '   SEE ISSUES #111 #110 #109'
                   write(stderr,'(*(g0))') '   START WITH https://github.com/fortran-lang/fpm/issues/111'
                endif
                cmd_settings=fpm_new_settings(name=name, &
                 & with_executable=lget('app'),          &
                 & with_test=lget('test'),               &
                 & with_lib=lget('lib') )
            endif

        case('help')
            call set_args(' ',help_help,version_text)
            if(size(unnamed).lt.2)then
                unnamed=['help', 'fpm ']
            endif
            allocate(character(len=80) :: help_text(0))
            do i=2,size(unnamed)
                select case(unnamed(i))
                case('build  ' )
                   help_text=[character(len=80) :: help_text, help_build]
                case('run    ' )
                   help_text=[character(len=80) :: help_text, help_run]
                case('help   ' )
                   help_text=[character(len=80) :: help_text, help_help]
                case('test   ' )
                   help_text=[character(len=80) :: help_text, help_test]
                case('new    ' )
                   help_text=[character(len=80) :: help_text, help_new]
                case('fpm    ' )
                   help_text=[character(len=80) :: help_text, help_fpm]
                case('version' )
                   help_text=[character(len=80) :: help_text, version_text]
                case('all    ' )
                   help_text=[character(len=80) :: help_text, help_fpm]
                   help_text=[character(len=80) :: help_text, help_new]
                   help_text=[character(len=80) :: help_text, help_build]
                   help_text=[character(len=80) :: help_text, help_run]
                   help_text=[character(len=80) :: help_text, help_test]
                   help_text=[character(len=80) :: help_text, version_text]
                case default
                   help_text=[character(len=80) :: help_text, 'unknown subcommand'//unnamed(i)]
                end select
            enddo
            write(stderr,'(g0)')(trim(help_text(i)), i=1, size(help_text) )

        case('install')
            call set_args('--release F ', help_install, version_text)

            allocate(fpm_install_settings :: cmd_settings)

        case('test')
            call set_args('--list F --release F --',help_test,version_text)

            if( size(unnamed) .gt. 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif

            allocate(fpm_test_settings :: cmd_settings)
            cmd_settings=fpm_test_settings( name=names, list=lget('list'), &
            & release=lget('release'), args=remaining )

        case default

            call set_args(' ', help_fpm, version_text)
            ! Note: will not get here if --version or --usage or --help is present on commandline
            if(len_trim(cmdarg).eq.0)then
                write(stderr,'(*(a))')'ERROR: missing subcommand'
            else
                write(stderr,'(*(a))')'ERROR: unknown subcommand [', trim(cmdarg), ']'
            endif
            help_text=[character(len=80) ::                                            &
             'USAGE: fpm [ SUBCOMMAND [SUBCOMMAND_OPTIONS] ] | [|--help|--version] ', &
             '       Enter "fpm --help" for more information                       ', &
             '' ]
            write(stderr,'(g0)')(trim(help_text(i)), i=1, size(help_text) )

        end select
    end subroutine get_command_line_settings

    function is_fortran_name(line) result (lout)
    ! determine if a string is a valid Fortran name ignoring trailing spaces (but not leading spaces)
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
    help_fpm=[character(len=80) :: &
    'NAME', &
    '   fpm(1) - A Fortran package manager and build system', &
    'OS TYPE' ]
    select case (get_os_type())
       case (OS_LINUX);   help_fpm=[character(len=80) :: help_fpm, "    Linux"   ]
       case (OS_MACOS);   help_fpm=[character(len=80) :: help_fpm, "    macOS"   ]
       case (OS_WINDOWS); help_fpm=[character(len=80) :: help_fpm, "    Windows" ]
       case (OS_CYGWIN);  help_fpm=[character(len=80) :: help_fpm, "    Cygwin"  ]
       case (OS_SOLARIS); help_fpm=[character(len=80) :: help_fpm, "    Solaris" ]
       case (OS_FREEBSD); help_fpm=[character(len=80) :: help_fpm, "    FreeBSD" ]
       case (OS_UNKNOWN); help_fpm=[character(len=80) :: help_fpm, "    Unknown" ]
       case default     ; help_fpm=[character(len=80) :: help_fpm, "    UNKNOWN" ]
    end select
    help_fpm=[character(len=80) :: help_fpm, &
    'SYNOPSIS                                                                        ', &
    '   fpm SUBCOMMAND [SUBCOMMAND_OPTIONS]                                          ', &
    '                                                                                ', &
    '   fpm --help|--version                                                         ', &
    '                                                                                ', &
    'DESCRIPTION                                                                     ', &
    '   fpm is a package manager that helps you create Fortran projects that are     ', &
    '   optionally dependent on multiple files and other fpm(1) packages.            ', &
    '                                                                                ', &
    '   Most significantly fpm(1) lets you pull upon other fpm(1) packages in        ', &
    '   distributed git(1) repositories as if the packages were a basic part         ', &
    '   of your default programming environment, as well as letting you share        ', &
    '   your projects with others in a similar manner.                               ', &
    '                                                                                ', &
    '   See the fpm(1) repository for a listing of such available projects.          ', &
    '                                                                                ', &
    '   All output goes into the directory "build/".                                 ', &
    '                                                                                ', &
    'SUBCOMMANDS                                                                     ', &
    '   Valid fpm subcommands are:                                                   ', &
    '                                                                                ', &
    '     build [--release] [--list]                                                 ', &
    '                        Compile the packages into the "build/" directory.       ', &
    '     new NAME [--with-executable] [--with-test]                                 ', &
    '                        Create a new Fortran package directory                  ', &
    '                        with sample files                                       ', &
    '     run [NAME(s)] [--release] [--list] [-- ARGS]                               ', &
    '                        Run the local package binaries. defaults to all         ', &
    '                        binaries for that release.                              ', &
    '     test [NAME(s)] [--release] [--list] [-- ARGS]                              ', &
    '                        Run the tests                                           ', &
    '     help [NAME(s)]     Alternate method for displaying subcommand help         ', &
    '                                                                                ', &
    'SUBCOMMAND OPTIONS                                                              ', &
    '   --release       Builds or runs in release mode (versus debug mode). fpm(1)   ', &
    '                   Defaults to using common compiler debug flags and building   ', &
    '                   in "build/gfortran_debug/". When this flag is present build  ', &
    '                   output goes into "build/gfortran_release/" and common        ', &
    '                   compiler optimization flags are used.                        ', &
    '   --list          list candidates instead of building or running them          ', &
    '   -- ARGS         Arguments to pass to executables/tests                       ', &
    '   --help          Show this help text and exit. Valid for all subcommands.     ', &
    '   --version       Show version information and exit. Valid for all subcommands.', &
    'EXAMPLES                                                                        ', &
    ' sample commands:                                                               ', &
    '                                                                                ', &
    '    fpm build                                                                   ', &
    '    fpm test                                                                    ', &
    '    fpm run                                                                     ', &
    '    fpm new --help                                                              ', &
    '    fpm new mypackage --with-executable --with-test                             ', &
    '    fpm run myprogram --release -- -x 10 -y 20 --title "my title"               ', &
    'SEE ALSO                                                                        ', &
    ' The fpm(1) home page is https://github.com/fortran-lang/fpm                    ', &
    '']
    help_run=[character(len=80) :: &
    'NAME                                                           ', &
    '  run(1) - the fpm(1) subcommand to run project applications   ', &
    '                                                               ', &
    'SYNOPSIS                                                       ', &
    ' fpm run [NAME(s)] [--release] [-- ARGS]                       ', &
    '                                                               ', &
    ' fpm run --help|--version                                      ', &
    '                                                               ', &
    'DESCRIPTION                                                    ', &
    ' Run applications you have built in your fpm(1) project.       ', &
    '                                                               ', &
    'OPTIONS                                                        ', &
    ' NAME(s)    optional list of specific names to execute.        ', &
    '            The default is to run all the applications in app/ ', &
    '            or the programs listed in the "fpm.toml" file.     ', &
    ' --release  selects the optimized build instead of the debug   ', &
    '            build.                                             ', &
    ' --list     list candidates instead of building or running them', &
    ' -- ARGS    optional arguments to pass to the program(s).      ', &
    '            The same arguments are passed to all names         ', &
    '            specified.                                         ', &
    '                                                               ', &
    'EXAMPLES                                                       ', &
    'run fpm(1) project applications                                ', &
    '                                                               ', &
    ' # run default programs in /app or as specified in "fpm.toml"  ', &
    ' fpm run                                                       ', &
    '                                                               ', &
    ' # run a specific program and pass arguments to the command    ', &
    ' fpm run mytest -- -x 10 -y 20 -title "my title line"          ', &
    '                                                               ', &
    ' # production version of two applications                      ', &
    ' fpm run tst1 test2 -release                                   ', &
    'SEE ALSO                                                       ', &
    ' The fpm(1) home page is https://github.com/fortran-lang/fpm   ', &
    '' ]
    help_build=[character(len=80) :: &
    'NAME                                                                  ', &
    '    build(1) - the fpm(1) subcommand to build a project               ', &
    'SYNOPSIS                                                              ', &
    ' fpm build [--release]|[-list]                                        ', &
    ' fpm build --help|--version                                           ', &
    '                                                                      ', &
    'DESCRIPTION                                                           ', &
    ' The "fpm build" command                                              ', &
    '    o Fetches any dependencies                                        ', &
    '    o Scans your sources                                              ', &
    '    o Builds them in the proper order                                 ', &
    '                                                                      ', &
    ' The Fortran source files are assumed to be in app/, test/, and src/  ', &
    ' by default. The changed or new files found are rebuilt.              ', &
    ' The results are placed in the build/ directory.                      ', &
    '                                                                      ', &
    ' Non-default pathnames and remote dependencies are used if            ', &
    ' specified in the "fpm.toml" file.                                    ', &
    '                                                                      ', &
    'OPTIONS                                                               ', &
    ' --release  build in build/*_release instead of build/*_debug with    ', &
    '            high optimization instead of full debug options.          ', &
    ' --list     list candidates instead of building or running them       ', &
    ' --help     print this help and exit                                  ', &
    ' --version  print program version information and exit                ', &
    '                                                                      ', &
    'EXAMPLES                                                              ', &
    ' Sample commands:                                                     ', &
    '                                                                      ', &
    '  fpm build          # build with debug options                       ', &
    '  fpm build -release # build with high optimization                   ', &
    'SEE ALSO                                                              ', &
    ' The fpm(1) home page is https://github.com/fortran-lang/fpm          ', &
    '' ]
    help_help=[character(len=80) :: &
    'NAME                                                                  ', &
    '    help(1) - the fpm(1) subcommand to display help                   ', &
    'SYNOPSIS                                                              ', &
    ' fpm help [fpm][new][build][run][test][help][version][all]            ', &
    '                                                                      ', &
    'DESCRIPTION                                                           ', &
    ' The "fpm help" command is an alternative to the --help parameter     ', &
    ' on the fpm(1) command and its subcommands.                           ', &
    '                                                                      ', &
    'OPTIONS                                                               ', &
    ' NAME(s)    A list of topic names to display.                         ', &
    '                                                                      ', &
    'EXAMPLES                                                              ', &
    '                                                                      ', &
    '  fpm help                                                            ', &
    '  fpm help version                                                    ', &
    '  fpm all                                                             ', &
    'SEE ALSO                                                              ', &
    ' The fpm(1) home page is https://github.com/fortran-lang/fpm          ', &
    '' ]
    help_new=[character(len=80) ::                                      &
    'NAME                                                            ', &
    '   new(1) - the fpm(1) subcommand to initialize a new project   ', &
    'SYNOPSIS                                                        ', &
    ' fpm new NAME [--with-executable] [--with-test]                 ', &
    '                                                                ', &
    ' fpm new --help|--version                                       ', &
    '                                                                ', &
    'DESCRIPTION                                                     ', &
    ' Create a new programming project in a new directory            ', &
    '                                                                ', &
    ' The "new" subcommand creates a directory and runs the command  ', &
    ' "git init" in that directory and makes an example "fpm.toml"   ', &
    ' file. and src/ directory and a sample module file. It          ', &
    ' optionally also creates a test/ and app/ directory with        ', &
    ' trivial example Fortran program sources.                       ', &
    '                                                                ', &
    ' Remember to update the information in the sample "fpm.toml"    ', &
    ' file with such information as your name and e-mail address.    ', &
    '                                                                ', &
    'OPTIONS                                                         ', &
    ' NAME   the name of the project directory to create. The name   ', &
    '        must be a valid Fortran name composed of 1 to 63        ', &
    '        ASCII alphanumeric characters and underscores,          ', &
    '        starting with a letter.                                 ', &
    ' --with-executable  additionally create optional directory app/ ', &
    '                    and placeholder program for "fpm run".      ', &
    ' --with-test        additionally create optional directory test/', &
    '                    and placeholder program for "fpm test".     ', &
    ' --help             print this help and exit                    ', &
    ' --version          print program version information and exit  ', &
    '                                                                ', &
    'EXAMPLES                                                        ', &
    ' Sample use                                                     ', &
    '                                                                ', &
    '   fpm new myproject  # create new project directory and seed it', &
    '   cd myproject       # Enter the new directory                 ', &
    '   # and run commands such as                                   ', &
    '   fpm build                                                    ', &
    '   fpm run            # if you selected --with-executable       ', &
    '   fpm test           # if you selected --with-test             ', &
    'SEE ALSO                                                        ', &
    ' The fpm(1) home page is https://github.com/fortran-lang/fpm    ', &
    '                                                                ', &
    ' Registered packages are at https://fortran-lang.org/packages   ', &
    '' ]
    help_test=[character(len=80) :: &
    'NAME                                                           ', &
    '  test(1) - the fpm(1) subcommand to run project tests         ', &
    '                                                               ', &
    'SYNOPSIS                                                       ', &
    ' fpm test [NAME(s)] [--release] [--list] [-- ARGS]             ', &
    '                                                               ', &
    ' fpm test --help|--version                                     ', &
    '                                                               ', &
    'DESCRIPTION                                                    ', &
    ' Run applications you have built to test your project.         ', &
    '                                                               ', &
    'OPTIONS                                                        ', &
    ' NAME(s)    optional list of specific test names to execute.   ', &
    '            The default is to run all the tests in test/       ', &
    '            or the tests listed in the "fpm.toml" file.        ', &
    ' --release  selects the optimized build instead of the debug   ', &
    '            build.                                             ', &
    ' --list     list candidates instead of building or running them', &
    ' -- ARGS    optional arguments to pass to the test program(s). ', &
    '            The same arguments are passed to all test names    ', &
    '            specified.                                         ', &
    '                                                               ', &
    'EXAMPLES                                                       ', &
    'run tests                                                      ', &
    '                                                               ', &
    ' # run default tests in /test or as specified in "fpm.toml"    ', &
    ' fpm test                                                      ', &
    '                                                               ', &
    ' # run a specific test and pass arguments to the command       ', &
    ' fpm test mytest -- -x 10 -y 20 -title "my title line"         ', &
    '                                                               ', &
    ' fpm test tst1 test2 -release # production version of two tests', &
    'SEE ALSO                                                       ', &
    ' The fpm(1) home page is https://github.com/fortran-lang/fpm   ', &
    '' ]
    help_install=[character(len=80) :: &
    ' fpm(1) subcommand "install"                          ', &
    '                                                      ', &
    ' Usage: fpm install NAME                              ', &
    '' ]
    end subroutine set_help

end module fpm_command_line
