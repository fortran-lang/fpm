!! new             are full pathnames allowed? Is more than one pathname allowed?
!! fpm --search    search keywords, descriptions, names of fpm(1) package registry
!! install         not sure what it is supposed to do. Install files in build/ to a user-specified area?
module fpm_command_line
    use fpm_environment, only: get_os_type, &
                               OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                               OS_CYGWIN, OS_SOLARIS, OS_FREEBSD
    use M_CLI2,      only : set_args, lget, unnamed, remaining
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
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
    type, extends(fpm_cmd_settings)     :: fpm_new_settings
        character(len=:),allocatable    :: name
        logical                         :: with_executable=.false.
        logical                         :: with_test=.false.
    end type

    type, extends(fpm_cmd_settings)     :: fpm_build_settings
        logical                         :: release=.false.
    end type

    type, extends(fpm_cmd_settings)     :: fpm_run_settings
        character(len=ibug),allocatable :: name(:)
        logical                         :: release=.false.
        logical                         :: list=.false.
        character(len=:),allocatable    :: args
    end type

    type, extends(fpm_cmd_settings)     :: fpm_test_settings
        character(len=ibug),allocatable :: name(:)
        logical                         :: release=.false.
        character(len=:),allocatable    :: args
    end type

    type, extends(fpm_cmd_settings)     :: fpm_install_settings
    end type

    character(len=:),allocatable        :: name
    character(len=ibug),allocatable     :: names(:)

contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

        character(len=4096)           :: cmdarg
        integer                       :: i
        character(len=:), allocatable :: help_text(:), version_text(:)

        ! text for --version switch,
        version_text = [character(len=80) :: &
           &  'VERSION:     0.1.0, Pre-alpha',                              &
           &  'PROGRAM:     fpm(1)',                                        &
           &  'DESCRIPTION: A Fortran package manager and build system',    &
           &  'HOME PAGE:   https://github.com/fortran-lang/fpm',           &
           &  'LICENSE:     MIT',                                           &
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
         help_text=[character(len=80) :: &
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
          '' ]
         call set_args('--list F --release F --',help_text,version_text)

         if( size(unnamed) .gt. 1 )then
            names=unnamed(2:)
         else
            names=[character(len=len(names)) :: ]
         endif

         allocate(fpm_run_settings :: cmd_settings)
         cmd_settings=fpm_run_settings( name=names, list=lget('list'), &
          & release=lget('release'), args=remaining )

        case('build')
         help_text=[character(len=80) :: &
          'NAME                                                    ', &
          '    build(1) - the fpm(1) subcommand to build a project ', &
          'SYNOPSIS                                                ', &
          ' fpm build [--release] build                            ', &
          ' fpm build --help|--version                             ', &
          '                                                        ', &
          'DESCRIPTION                                             ', &
          ' Finds the Fortran source files in app/, test/, and     ', &
          ' src/ by default; determines the dependencies           ', &
          ' between the files and rebuilds unbuilt or changed      ', &
          ' files. The results are placed in the build/ directory. ', &
          '                                                        ', &
          ' Non-default pathnames are used if specified in the     ', &
          ' "fpm.toml" file.                                       ', &
          '                                                        ', &
          ' Remote dependencies are satisfied as well if specified ', &
          ' in the "fpm.toml" file.                                ', &
          '                                                        ', &
          'OPTIONS                                                 ', &
          ' --release  build in build/*_release instead of         ', &
          '            build/*_debug/ with high optimization       ', &
          '            instead of full debug options.              ', &
          ' --help     print this help and exit                    ', &
          ' --version  print program version information and exit  ', &
          '                                                        ', &
          'EXAMPLES                                                ', &
          ' Sample commands:                                       ', &
          '                                                        ', &
          '  fpm build          # build with debug options         ', &
          '  fpm build -release # build with high optimization     ', &
          '' ]
         call set_args( '--release F --',help_text,version_text )

         allocate( fpm_build_settings :: cmd_settings )
         cmd_settings=fpm_build_settings( release=lget('release') )

        case('new')
         help_text=[character(len=80) ::                                     &
          'NAME                                                           ', &
          '   new(1) - the fpm(1) subcommand to initialize a new project  ', &
          'SYNOPSIS                                                       ', &
          ' fpm new NAME [--with-executable] [--with-test]                ', &
          '                                                               ', &
          ' fpm new --help|--version                                      ', &
          '                                                               ', &
          'DESCRIPTION                                                    ', &
          ' Create a new programming project in a new directory           ', &
          '                                                               ', &
          ' The "new" subcommand creates a directory and runs the command ', &
          ' "git init" in that directory and makes an example "fpm.toml"  ', &
          ' file, a src/ directory, and optionally a test/ and app/       ', &
          ' directory with trivial example Fortran source files.          ', &
          '                                                               ', &
          ' Remember to update the information in the sample "fpm.toml"   ', &
          ' file with such information as your name and e-mail address.   ', &
          '                                                               ', &
          'EXAMPLES                                                       ', &
          ' Sample use                                                    ', &
          '                                                               ', &
          '   # create new project directory and seed it                  ', &
          '   fpm new myproject                                           ', &
          '   # Enter the new directory                                   ', &
          '   cd myproject                                                ', &
          '   # and run commands such as                                  ', &
          '   fpm build                                                   ', &
          '   fpm run  # if you selected --with-executable                ', &
          '   fpm test # if you selected --with-test                      ', &
          '                                                               ', &
          '' ]
         call set_args(' --with-executable F --with-test F ', help_text, version_text)
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

         allocate(fpm_new_settings :: cmd_settings)
         cmd_settings=fpm_new_settings(name=name, with_executable=lget('with-executable'), with_test=lget('with-test') )

        case('install')
         help_text=[character(len=80) :: &
          ' fpm(1) subcommand "install"                          ', &
          '                                                      ', &
          ' Usage: fpm install NAME                              ', &
          '' ]
         call set_args('--release F ', help_text, version_text)

         allocate(fpm_install_settings :: cmd_settings)

        case('test')
         help_text=[character(len=80) :: &
          'NAME                                                           ', &
          '  test(1) - the fpm(1) subcommand to run project tests         ', &
          '                                                               ', &
          'SYNOPSIS                                                       ', &
          ' fpm test [NAME(s)] [--release] [-- ARGS]                      ', &
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
          '                                                               ', &
          '' ]
         call set_args(' -release F --', help_text, version_text)

         if( size(unnamed) .gt. 1 )then
            names=unnamed(2:)
         else
            names=[character(len=len(names)) :: ]
         endif

         allocate(fpm_test_settings :: cmd_settings)
         cmd_settings=fpm_test_settings(name=names, release=lget('release'), args=remaining )

        case default
         help_text=[character(len=80) :: &
          'NAME', &
          '   fpm(1) - A Fortran package manager and build system', &
          'OS TYPE' ]
            select case (get_os_type())
               case (OS_LINUX);   help_text=[character(len=80) :: help_text, "    Linux"   ]
               case (OS_MACOS);   help_text=[character(len=80) :: help_text, "    macOS"   ]
               case (OS_WINDOWS); help_text=[character(len=80) :: help_text, "    Windows" ]
               case (OS_CYGWIN);  help_text=[character(len=80) :: help_text, "    Cygwin"  ]
               case (OS_SOLARIS); help_text=[character(len=80) :: help_text, "    Solaris" ]
               case (OS_FREEBSD); help_text=[character(len=80) :: help_text, "    FreeBSD" ]
               case (OS_UNKNOWN); help_text=[character(len=80) :: help_text, "    Unknown" ]
               case default     ; help_text=[character(len=80) :: help_text, "    UNKNOWN" ]
            end select
         help_text=[character(len=80) :: help_text, &
           'SYNTAX                                                                          ', &
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
           '     build [--release]  Compile the packages into the "build/" directory.       ', &
           '     new NAME [--with-executable] [--with-test]                                 ', &
           '                        Create a new Fortran package directory                  ', &
           '                        with sample files                                       ', &
           '     run [NAME(s)] [--release] [--list] [-- ARGS]                               ', &
           '                        Run the local package binaries. defaults to all         ', &
           '                        binaries for that release.                              ', &
           '     test [NAME(s)] [--release] [-- ARGS]                                       ', &
           '                        Run the tests                                           ', &
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
           ' For examples and documentation see https://github.com/fortran-lang/fpm         ', &
           '']

         call set_args(' ', help_text, version_text)
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
         !!stop 3 ! causes github site tests to fail
         stop

        end select
   end subroutine get_command_line_settings

end module fpm_command_line
