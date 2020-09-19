module fpm_command_line
    use fpm_environment, only: get_os_type, &
                               OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                               OS_CYGWIN, OS_SOLARIS, OS_FREEBSD
    use M_CLI2,      only : set_args
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

    type, extends(fpm_cmd_settings) :: fpm_new_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_build_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_run_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_test_settings
    end type

    type, extends(fpm_cmd_settings) :: fpm_install_settings
    end type

contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

        character(len=100) :: cmdarg
        integer :: i
        character(len=:), allocatable :: help_text(:), version_text(:)

        ! text for --version switch, 
        version_text = [character(len=132) :: &
           &  'VERSION:     0.0.0',                                         &
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
         help_text=[character(len=132) :: &
          ' fpm(1) subcommand "run"          ',                                                        &
          '                                  ',                                                        &
          ' Usage: fpm run [NAME(s)] [--release] [-- ARGS]|[--help|--version|--usage]  ', &
          '' ]
         call set_args('--list F --release F --',help_text,version_text)
         allocate(fpm_run_settings :: cmd_settings)
     
        case('build')
         help_text=[character(len=132) :: &
          ' fpm(1) subcommand "build"                              ', &
          '                                                        ', &
          ' Usage: fpm [--release] build |--help|--version|--usage ', &
          '' ]
         call set_args('--release F --',help_text,version_text)
         allocate(fpm_build_settings :: cmd_settings)
     
        case('new')
         help_text=[character(len=132) ::                           &
          ' fpm(1) subcommand "new"                              ', &
          ' Create a new project in a new directory              ', &
          '                                                      ', &
          ' Usage: fpm new NAME [--with-executable] [--with-test]', &
          '' ]
         call set_args(' --with-executable F --with-test F ',help_text,version_text)
         allocate(fpm_new_settings :: cmd_settings)
     
        case('install')
         help_text=[character(len=132) :: &
          ' fpm(1) subcommand "install"                          ', &
          '                                                      ', &
          ' Usage: fpm install NAME                              ', &
          '' ]
         call set_args('--release F ',help_text,version_text)
         allocate(fpm_install_settings :: cmd_settings)
     
        case('test')
         help_text=[character(len=132) :: &
          ' fpm(1) subcommand "test"                                                                  ', &
          '                                                                                           ', &
          ' Usage:  fpm test [NAME(s)] [--release] [-- ARGS]|[--help|--version|--usage]  ', &
          '' ]
         call set_args(' -release F --',help_text,version_text)
         allocate(fpm_test_settings :: cmd_settings)
     
        case default
         help_text=[character(len=132) :: &
          'NAME', &
          '   fpm(1) - A Fortran package manager and build system', &
          'OS TYPE' ]
            select case (get_os_type())
               case (OS_LINUX);   help_text=[character(len=132) :: help_text, "    Linux"   ]
               case (OS_MACOS);   help_text=[character(len=132) :: help_text, "    macOS"   ]
               case (OS_WINDOWS); help_text=[character(len=132) :: help_text, "    Windows" ]
               case (OS_CYGWIN);  help_text=[character(len=132) :: help_text, "    Cygwin"  ]
               case (OS_SOLARIS); help_text=[character(len=132) :: help_text, "    Solaris" ]
               case (OS_FREEBSD); help_text=[character(len=132) :: help_text, "    FreeBSD" ]
               case (OS_UNKNOWN); help_text=[character(len=132) :: help_text, "    Unknown" ]
               case default     ; help_text=[character(len=132) :: help_text, "    UNKNOWN" ]
            end select
         help_text=[character(len=132) :: help_text, &
           'SYNTAX', &
           '   fpm [SUBCOMMAND [SUBCOMMAND_OPTIONS]|[--help|--version|--usage]              ', &
           '                                                                                ', &
           'DESCRIPTION                                                                     ', &
           '   A package manager that helps you create Fortran projects that are            ', &
           '   optionally dependent on multiple files and other fpm(1) packages.            ', &
           '   All output goes into the directory "build/".                                 ', &
           '                                                                                ', &
           'SUBCOMMANDS                                                                     ', &
           '   Valid fpm subcommands are:                                                   ', &
           '                                                                                ', &
           '     build [NAMES(s)] [--release]                                               ', &
           '                       Compile the packages into the "build/" directory.        ', &
           '     new NAME [--with-executable] [--with-test]                                 ', &
           '                       Create a new Fortran package directory                   ', &
           '                       with sample files                                        ', &
           '     run [NAME(s)] [--release] [--list] [-- ARGS]                               ', &
           '                       Run the local package binaries. defaults to all          ', &
           '                       binaries for that release.                               ', &
           '     test [NAME(s)] [--release] [-- ARGS]                                       ', &
           '                       Run the tests                                            ', &
           'SUBCOMMAND OPTIONS                                                              ', &
           '   --release       Builds or runs in release mode (versus debug mode). fpm(1)   ', &
           '                   Defaults to using common compiler debug flags and building   ', &
           '                   in "build/gfortran_debug/". When this flag is present build  ', &
           '                   output goes into "build/gfortran_release/" and common        ', &
           '                   compiler optimization flags are used.                        ', &
           '   --list          list candidates instead of building or running them          ', &
           '   -- ARGS         Arguments to pass to executables/tests                       ', &
           '   --help          Show this help text and exit                                 ', &
           '   --version       Show version information and exit                            ', &
           'EXAMPLES                                                                        ', &
           '    fpm build                                                                   ', &
           '    fpm new mypackage --with-executable                                         ', &
           '']
     
         call set_args(' ',help_text,version_text)
         ! Note: will not get here if --version or --usage or --help is present on commandline
         write(stderr,'(*(a))')'*fpm* error: unknown or missing subcommand [',trim(cmdarg),']'
         help_text=[character(len=132) ::                           &
           'Usage: fpm [COMMAND [[--release] [--]|[--help|--version|--usage]     ', &
           '       Enter "fpm --help" for more information', &
           '' ]
         write(stderr,'(g0)')(trim(help_text(i)),i=1,size(help_text))
         stop
  
        end select
   end subroutine get_command_line_settings

end module fpm_command_line
