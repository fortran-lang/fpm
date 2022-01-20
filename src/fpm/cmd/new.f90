module fpm_cmd_new
!># Definition of the "new" subcommand
!>
!> A type of the general command base class [[fpm_cmd_settings]]
!> was created for the "new" subcommand ==> type [[fpm_new_settings]].
!> This procedure read the values that were set on the command line
!> from this type to decide what actions to take.
!>
!> It is virtually self-contained and so independant of the rest of the
!> application that it could function as a separate program.
!>
!> The "new" subcommand options currently consist of a SINGLE top
!> directory name to create that must have a name that is an
!> allowable Fortran variable name. That should have been ensured
!> by the command line processing before this procedure is called.
!> So basically this routine has already had the options vetted and
!> just needs to conditionally create a few files.
!>
!> As described in the documentation it will selectively
!> create the subdirectories app/, test/, src/, and example/
!> and populate them with sample files.
!>
!> It also needs to create an initial manifest file "fpm.toml".
!>
!> It then calls the system command "git init".
!>
!> It should test for file existence and not overwrite existing
!> files and inform the user if there were conflicts.
!>
!> Any changes should be reflected in the documentation in
!> [[fpm_command_line.f90]]
!>
!> FUTURE
!> A filename like "." would need system commands or a standard routine
!> like realpath(3c) to process properly.
!>
!> Perhaps allow more than one name on a single command. It is an arbitrary
!> restriction based on a concensus preference, not a required limitation.
!>
!> Initially the name of the directory is used as the module name in the
!> src file so it must be an allowable Fortran variable name. If there are
!> complaints about it it might be changed. Handling unicode at this point
!> might be problematic as not all current compilers handle it. Other
!> utilities like content trackers (ie. git) or repositories like github
!> might also have issues with alternative names or names with spaces, etc.
!> So for the time being it seems prudent to encourage simple ASCII top directory
!> names (similiar to the primary programming language Fortran itself).
!>
!> Should be able to create or pull more complicated initial examples
!> based on various templates. It should place or mention other relevant
!> documents such as a description of the manifest file format in user hands;
!> or how to access registered packages and local packages,
!> although some other command might provide that (and the help command should
!> be the first go-to for a CLI utility).

use fpm_command_line, only : fpm_new_settings
use fpm_environment, only : run, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only : join_path, exists, basename, mkdir, is_dir
use fpm_filesystem, only : fileopen, fileclose, filewrite, warnwrite
use fpm_strings, only : join, to_fortran_name
use fpm_error, only : fpm_stop
use,intrinsic :: iso_fortran_env, only : stderr=>error_unit
implicit none
private
public :: cmd_new

contains

subroutine cmd_new(settings)
type(fpm_new_settings), intent(in) :: settings
integer,parameter            :: tfc = selected_char_kind('DEFAULT')
character(len=:,kind=tfc),allocatable :: bname          ! baeename of NAME
character(len=:,kind=tfc),allocatable :: tomlfile(:)
character(len=:,kind=tfc),allocatable :: littlefile(:)

    !> TOP DIRECTORY NAME PROCESSING
    !> see if requested new directory already exists and process appropriately
    if(exists(settings%name) .and. .not.settings%backfill )then
        write(stderr,'(*(g0,1x))')&
        & '<ERROR>',settings%name,'already exists.'
        write(stderr,'(*(g0,1x))')&
        & '        perhaps you wanted to add --backfill ?'
        return
    elseif(is_dir(settings%name) .and. settings%backfill )then
        write(*,'(*(g0))')'backfilling ',settings%name
    elseif(exists(settings%name) )then
        write(stderr,'(*(g0,1x))')&
        & '<ERROR>',settings%name,'already exists and is not a directory.'
        return
    else
        ! make new directory
        call mkdir(settings%name)
    endif

    !> temporarily change to new directory as a test. NB: System dependent
    call run('cd '//settings%name)
    ! NOTE: need some system routines to handle filenames like "."
    ! like realpath() or getcwd().
    bname=basename(settings%name)

    littlefile=[character(len=80) :: '# '//bname, 'My cool new project!']

    ! create NAME/README.md
    call warnwrite(join_path(settings%name, 'README.md'), littlefile)

    ! start building NAME/fpm.toml
    if(settings%with_full)then
        tomlfile=[character(len=80) :: &
        &'  # This is your fpm(Fortran Package Manager) manifest file                     ',&
        &'  # ("fpm.toml"). It is heavily annotated to help guide you though              ',&
        &'  # customizing a package build, although the defaults are sufficient           ',&
        &'  # for many basic packages.                                                    ',&
        &'  #                                                                             ',&
        &'  # The manifest file is not only used to provide metadata identifying          ',&
        &'  # your project (so it can be used by others as a dependency). It can          ',&
        &'  # specify where your library and program sources live, what the name          ',&
        &'  # of the executable(s) will be, what files to build, dependencies on          ',&
        &'  # other fpm packages, and what external libraries are required.               ',&
        &'  #                                                                             ',&
        &'  # The manifest format must conform to the TOML configuration file             ',&
        &'  # standard.                                                                   ',&
        &'  #                                                                             ',&
        &'  # TOML files support flexible use of white-space and commenting of the        ',&
        &'  # configuration data, but for clarity in this sample active directives        ',&
        &'  # begin in column one. Inactive example directives are commented              ',&
        &'  # out with a pound character ("#") but begin in column one as well.           ',&
        &'  # Commentary begins with a pound character in column three.                   ',&
        &'  #                                                                             ',&
        &'  # This file draws heavily upon the following references:                      ',&
        &'  #                                                                             ',&
        &'  # The fpm home page at                                                        ',&
        &'  #     https://github.com/fortran-lang/fpm                                     ',&
        &'  # A complete list of keys and their attributes at                             ',&
        &'  #     https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md     ',&
        &'  # examples of fpm project packaging at                                        ',&
        &'  #     https://github.com/fortran-lang/fpm/blob/main/PACKAGING.md              ',&
        &'  # The Fortran TOML file interface and it''s references at                     ',&
        &'  #     https://github.com/toml-f/toml-f                                        ',&
        &'  #                                                                             ',&
        &'  #-----------------------                                                      ',&
        &'  # project Identification                                                      ',&
        &'  #-----------------------                                                      ',&
        &'  # We begin with project metadata at the manifest root. This data is designed  ',&
        &'  # to aid others when searching for the project in a repository and to         ',&
        &'  # identify how and when to contact the package supporters.                    ',&
        &'                                                                                ',&
        &'name = "'//bname//'"',&
        &'  # The project name (required) is how the project will be referred to.         ',&
        &'  # The name is used by other packages using it as a dependency. It also        ',&
        &'  # is used as the default name of any library built and the optional           ',&
        &'  # default executable built from app/main.f90. It must conform to the rules    ',&
        &'  # for a Fortran variable name.                                                ',&
        &'                                                                                ',&
        &'version = "0.1.0"                                                               ',&
        &'  # The project version number is a string. A recommended scheme for            ',&
        &'  # specifying versions is the Semantic Versioning scheme.                      ',&
        &'                                                                                ',&
        &'license = "license"                                                             ',&
        &'  # Licensing information specified using SPDX identifiers is preferred         ',&
        &'  # (eg. "Apache-2.0 OR MIT" or "LGPL-3.0-or-later").                           ',&
        &'                                                                                ',&
        &'maintainer = "jane.doe@example.com"                                             ',&
        &'  # Information on the project maintainer and means to reach out to them.       ',&
        &'                                                                                ',&
        &'author = "Jane Doe"                                                             ',&
        &'  # Information on the project author.                                          ',&
        &'                                                                                ',&
        &'copyright = "Copyright 2020 Jane Doe"                                           ',&
        &'  # A statement clarifying the Copyright status of the project.                 ',&
        &'                                                                                ',&
        &'#description = "A short project summary in plain text"                          ',&
        &'  # The description provides a short summary on the project. It should be       ',&
        &'  # plain text and not use any markup formatting.                               ',&
        &'                                                                                ',&
        &'#categories = ["fortran", "graphics"]                                           ',&
        &'  # Categories associated with the project. Listing only one is preferred.      ',&
        &'                                                                                ',&
        &'#keywords = ["hdf5", "mpi"]                                                     ',&
        &'  # The keywords field is an array of strings describing the project.           ',&
        &'                                                                                ',&
        &'#homepage = "https://stdlib.fortran-lang.org"                                   ',&
        &'  # URL to the webpage of the project.                                          ',&
        &'                                                                                ',&
        &'  # -----------------------------------------                                   ',&
        &'  # We are done with identifying the project.                                   ',&
        &'  # -----------------------------------------                                   ',&
        &'  #                                                                             ',&
        &'  # Now lets start describing how the project should be built.                  ',&
        &'  #                                                                             ',&
        &'  # Note tables would go here but we will not be talking about them (much)!!'    ,&
        &'  #                                                                             ',&
        &'  # Tables are a way to explicitly specify large numbers of programs in         ',&
        &'  # a compact format instead of individual per-program entries in the           ',&
        &'  # [[executable]], [[test]], and [[example]] sections to follow but            ',&
        &'  # will not be discussed further except for the following notes:               ',&
        &'  #                                                                             ',&
        &'  # + Tables must appear (here) before any sections are declared. Once a        ',&
        &'  #   section is specified in a TOML file everything afterwards must be         ',&
        &'  #   values for that section or the beginning of a new section. A simple       ',&
        &'  #   example looks like:                                                       ',&
        &'                                                                                ',&
        &'#executable = [                                                                 ',&
        &'#  { name = "a-prog" },                                                         ',&
        &'#  { name = "app-tool", source-dir = "tool" },                                  ',&
        &'#  { name = "fpm-man", source-dir = "tool", main="fman.f90" }                   ',&
        &'#]                                                                              ',&
        &'                                                                                ',&
        &'  # This would be in lieue of the [[executable]] section found later in this    ',&
        &'  # configuration file.                                                         ',&
        &'  # + See the reference documents (at the beginning of this document)           ',&
        &'  #   for more information on tables if you have long lists of programs         ',&
        &'  #   to build and are not simply depending on auto-detection.                  ',&
        &'  #                                                                             ',&
        &'  # Now lets begin the TOML sections (lines beginning with "[") ...             ',&
        &'  #                                                                             ',&
        &'                                                                                ',&
        &'[install] # Options for the "install" subcommand                                ',&
        &'                                                                                ',&
        &'  # When you run the "install" subcommand only executables are installed by     ',&
        &'  # default on the local system. Library projects that will be used outside of  ',&
        &'  # "fpm" can set the "library" boolean to also allow installing the module     ',&
        &'  # files and library archive. Without this being set to "true" an "install"    ',&
        &'  # subcommand ignores parameters that specify library installation.            ',&
        &'                                                                                ',&
        &'library = false                                                                 ',&
        &'                                                                                ',&
        &'[build] # General Build Options                                                 ',&
        &'                                                                                ',&
        &'  ###  Automatic target discovery                                               ',&
        &'  #                                                                             ',&
        &'  # Normally fpm recursively searches the app/, example/, and test/ directories ',&
        &'  # for program sources and builds them. To disable this automatic discovery of ',&
        &'  # program targets set the following to "false":                               ',&
        &'                                                                                ',&
        &'#auto-executables = true                                                        ',&
        &'#auto-examples = true                                                           ',&
        &'#auto-tests = true                                                              ',&
        &'                                                                                ',&
        &'  ### Package-level External Library Links                                      ',&
        &'  #                                                                             ',&
        &'  # To declare link-time dependencies on external libraries a list of           ',&
        &'  # native libraries can be specified with the "link" entry. You may            ',&
        &'  # have one library name or a list of strings in case several                  ',&
        &'  # libraries should be linked. This list of library dependencies is            ',&
        &'  # exported to dependent packages. You may have to alter your library          ',&
        &'  # search-path to ensure the libraries can be accessed. Typically,             ',&
        &'  # this is done with the LD_LIBRARY_PATH environment variable on ULS           ',&
        &'  # (Unix-Like Systems). You only specify the core name of the library          ',&
        &'  # (as is typical with most programming environments, where you                ',&
        &'  # would specify "-lz" on your load command to link against the zlib           ',&
        &'  # compression library even though the library file would typically be         ',&
        &'  # a file called "libz.a" "or libz.so"). So to link against that library       ',&
        &'  # you would specify:                                                          ',&
        &'                                                                                ',&
        &'#link = "z"                                                                     ',&
        &'                                                                                ',&
        &'  # Note that in some cases the order of the libraries matters:                 ',&
        &'                                                                                ',&
        &'#link = ["blas", "lapack"]                                                      ',&
        &'']
    endif

    if(settings%with_bare)then
    elseif(settings%with_lib)then
        call mkdir(join_path(settings%name,'src') )
        ! create next section of fpm.toml
        if(settings%with_full)then
            tomlfile=[character(len=80) ::  tomlfile, &
            &'[library]                                                                       ',&
            &'                                                                                ',&
            &'  # You can change the name of the directory to search for your library         ',&
            &'  # source from the default of "src/". Library targets are exported             ',&
            &'  # and usable by other projects.                                               ',&
            &'                                                                                ',&
            &'source-dir="src"                                                                ',&
            &'                                                                                ',&
            &'  # this can be a list:                                                         ',&
            &'                                                                                ',&
            &'#source-dir=["src", "src2"]                                                     ',&
            &'                                                                                ',&
            &'  # More complex libraries may organize their modules in subdirectories.        ',&
            &'  # For modules in a top-level directory fpm requires (but does not             ',&
            &'  # enforce) that:                                                              ',&
            &'  #                                                                             ',&
            &'  #  + The module has the same name as the source file. This is important.      ',&
            &'  #  + There should be only one module per file.                                ',&
            &'  #                                                                             ',&
            &'  # These two requirements simplify the build process for fpm. As Fortran       ',&
            &'  # compilers emit module files (.mod) with the same name as the module         ',&
            &'  # itself (but not the source file, .f90), naming the module the same          ',&
            &'  # as the source file allows fpm to:                                           ',&
            &'  #                                                                             ',&
            &'  #  + Uniquely and exactly map a source file (.f90) to its object (.o)         ',&
            &'  #    and module (.mod) files.                                                 ',&
            &'  #  + Avoid conflicts with modules of the same name that could appear          ',&
            &'  #    in dependency packages.                                                  ',&
            &'  #                                                                             ',&
            &'  ### Multi-level library source                                                ',&
            &'  # You can place your module source files in any number of levels of           ',&
            &'  # subdirectories inside your source directory, but there are certain naming   ',&
            &'  # conventions to be followed -- module names must contain the path components ',&
            &'  # of the directory that its source file is in.                                ',&
            &'  #                                                                             ',&
            &'  # This rule applies generally to any number of nested directories and         ',&
            &'  # modules. For example, src/a/b/c/d.f90 must define a module called a_b_c_d.  ',&
            &'  # Again, this is not enforced but may be required in future releases.         ',&
            &'']
        endif
        ! create placeholder module src/bname.f90
        littlefile=[character(len=80) ::          &
        &'module '//to_fortran_name(bname),       &
        &'  implicit none',                       &
        &'  private',                             &
        &'',                                      &
        &'  public :: say_hello',                 &
        &'contains',                              &
        &'  subroutine say_hello',                &
        &'    print *, "Hello, '//bname//'!"',    &
        &'  end subroutine say_hello',            &
        &'end module '//to_fortran_name(bname)]
        ! create NAME/src/NAME.f90
        call warnwrite(join_path(settings%name, 'src', bname//'.f90'),&
         & littlefile)
    endif

    if(settings%with_full)then
        tomlfile=[character(len=80) ::  tomlfile ,&
        &'[dependencies]                                                                  ',&
        &'                                                                                ',&
        &'  # Inevitably, you will want to be able to include other packages in           ',&
        &'  # a project. Fpm makes this incredibly simple, by taking care of              ',&
        &'  # fetching and compiling your dependencies for you. You just tell it          ',&
        &'  # what your dependencies names are, and where to find them.                   ',&
        &'  #                                                                             ',&
        &'  # If you are going to distribute your package only place dependencies         ',&
        &'  # here someone using your package as a remote dependency needs built.         ',&
        &'  # You can define dependencies just for developer executables in the           ',&
        &'  # next section, or even for specific executables as we will see below         ',&
        &'  # (Then fpm will still fetch and compile it when building your                ',&
        &'  # developer executables, but users of your library will not have to).         ',&
        &'  #                                                                             ',&
        &'  ## GLOBAL DEPENDENCIES (exported with your project)                           ',&
        &'  #                                                                             ',&
        &'  # Typically, dependencies are defined by specifying the project''s            ',&
        &'  # git repository.                                                             ',&
        &'  #                                                                             ',&
        &'  # You can be specific about which version of a dependency you would           ',&
        &'  # like. By default the latest default branch is used. You can           ',&
        &'  # optionally specify a branch, a tag or a commit value.                       ',&
        &'  #                                                                             ',&
        &'  # So here are several alternates for specifying a remote dependency (you      ',&
        &'  # can have at most one of "branch", "rev" or "tag" present):                  ',&
        &'                                                                                ',&
        &'#stdlib = { git = "https://github.com/LKedward/stdlib-fpm.git" }                ',&
        &'#stdlib = {git="https://github.com/LKedward/stdlib-fpm.git",branch = "master" },',&
        &'#stdlib = {git="https://github.com/LKedward/stdlib-fpm.git", tag = "v0.1.0" },  ',&
        &'#stdlib = {git="https://github.com/LKedward/stdlib-fpm.git", rev = "5a9b7a8" }. ',&
        &'                                                                                ',&
        &'  # There may be multiple packages listed:                                      ',&
        &'                                                                                ',&
        &'#M_strings = { git = "https://github.com/urbanjost/M_strings.git" }             ',&
        &'#M_time    = { git = "https://github.com/urbanjost/M_time.git" }                ',&
        &'                                                                                ',&
        &'  #                                                                             ',&
        &'  # You can even specify the local path to another project if it is in          ',&
        &'  # a sub-folder (If for example you have got another fpm package **in          ',&
        &'  # the same repository**) like this:                                           ',&
        &'                                                                                ',&
        &'#M_strings = { path = "M_strings" }                                             ',&
        &'                                                                                ',&
        &'  #  If you specify paths outside of your repository (ie. paths with a          ',&
        &'  #  slash in them) things will not work for your users!                        ',&
        &'  #                                                                             ',&
        &'  # For a more verbose layout use normal tables rather than inline tables       ',&
        &'  # to specify dependencies:                                                    ',&
        &'                                                                                ',&
        &'#[dependencies.toml-f]                                                          ',&
        &'#git = "https://github.com/toml-f/toml-f"                                       ',&
        &'#rev = "2f5eaba864ff630ba0c3791126a3f811b6e437f3"                               ',&
        &'                                                                                ',&
        &'  # Now you can use any modules from these libraries anywhere in your           ',&
        &'  # code -- whether is in your library source or a program source.              ',&
        &'                                                                                ',&
        &'[dev-dependencies]                                                              ',&
        &'                                                                                ',&
        &'  ## Dependencies Only for Development                                          ',&
        &'  #                                                                             ',&
        &'  # You can specify dependencies your library or application does not           ',&
        &'  # depend on in a similar way. The difference is that these will not           ',&
        &'  # be exported as part of your project to those using it as a remote           ',&
        &'  # dependency.                                                                 ',&
        &'  #                                                                             ',&
        &'  # Currently, like a global dependency it will still be available for          ',&
        &'  # all codes. It is up to the developer to ensure that nothing except          ',&
        &'  # developer test programs rely upon it.                                       ',&
        &'                                                                                ',&
        &'#M_msg    = { git = "https://github.com/urbanjost/M_msg.git" }                  ',&
        &'#M_verify = { git = "https://github.com/urbanjost/M_verify.git" }               ',&
        &'']
    endif
    if(settings%with_bare)then
    elseif(settings%with_executable)then
        ! create next section of fpm.toml
        call mkdir(join_path(settings%name, 'app'))
        ! create NAME/app or stop
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile, &
           &'  #-----------------------------------                                          ',&
           &'  ## Application-specific declarations                                          ',&
           &'  #-----------------------------------                                          ',&
           &'  # Now lets begin entries for the TOML tables (lines beginning with "[[")      ',&
           &'  # that describe the program sources -- applications, tests, and examples.     ',&
           &'  #                                                                             ',&
           &'  # First we will configuration individual applications run with "fpm run".     ',&
           &'  #                                                                             ',&
           &'  #   + the "name" entry for the executable to be built must always             ',&
           &'  #     be specified. The name must satisfy the rules for a Fortran             ',&
           &'  #     variable name. This will be the name of the binary installed by         ',&
           &'  #     the "install" subcommand and used on the "run" subcommand.              ',&
           &'  #   + The source directory for each executable can be adjusted by the         ',&
           &'  #     "source-dir" entry.                                                     ',&
           &'  #   + The basename of the source file containing the program body can         ',&
           &'  #     be specified with the "main" entry.                                     ',&
           &'  #   + Executables can also specify their own external package and             ',&
           &'  #     library link dependencies.                                              ',&
           &'  #                                                                             ',&
           &'  #     Currently, like a global dependency any external package dependency     ',&
           &'  #     will be available for all codes. It is up to the developer to ensure    ',&
           &'  #     that nothing except the application programs specified rely upon it.    ',&
           &'  #                                                                             ',&
           &'  # Note if your application needs to use a module internally, but you do not   ',&
           &'  # intend to build it as a library to be used in other projects, you can       ',&
           &'  # include the module in your program source file or directory as well.        ',&
           &'                                                                                ',&
           &'[[executable]]                                                                  ',&
           &'name="'//bname//'"',&
           &'source-dir="app"                                                                ',&
           &'main="main.f90"                                                                 ',&
           &'                                                                                ',&
           &'  # You may repeat this pattern to define additional applications. For instance,',&
           &'  # the following sample illustrates all accepted options, where "link" and     ',&
           &'  # "executable.dependencies" keys are the same as the global external library  ',&
           &'  # links and package dependencies described previously except they apply       ',&
           &'  # only to this executable:                                                    ',&
           &'                                                                                ',&
           &'#[[ executable ]]                                                               ',&
           &'#name = "app-name"                                                              ',&
           &'#source-dir = "prog"                                                            ',&
           &'#main = "program.f90"                                                           ',&
           &'#link = "z"                                                                     ',&
           &'#[executable.dependencies]                                                      ',&
           &'#M_CLI   = { git = "https://github.com/urbanjost/M_CLI.git" }                   ',&
           &'#helloff = { git = "https://gitlab.com/everythingfunctional/helloff.git" }      ',&
           &'#M_path  = { git = "https://github.com/urbanjost/M_path.git" }                  ',&
           &'']
        endif

        if(exists(bname//'/src/'))then
            littlefile=[character(len=80) ::          &
            &'program main',                          &
            &'  use '//to_fortran_name(bname)//', only: say_hello',    &
            &'  implicit none',                       &
            &'',                                      &
            &'  call say_hello()',                    &
            &'end program main']
        else
            littlefile=[character(len=80) ::                 &
            &'program main',                                 &
            &'  implicit none',                              &
            &'',                                             &
            &'  print *, "hello from project '//bname//'"',  &
            &'end program main']
        endif
        call warnwrite(join_path(settings%name, 'app/main.f90'), littlefile)
    endif

    if(settings%with_bare)then
    elseif(settings%with_test)then

       ! create NAME/test or stop
       call mkdir(join_path(settings%name, 'test'))
        ! create next section of fpm.toml
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile ,&
           &'[[test]]                                                                        ',&
           &'                                                                                ',&
           &'  # The same declarations can be made for test programs, which are              ',&
           &'  # executed with the "fpm test" command and are not build when your            ',&
           &'  # package is used as a dependency by other packages. These are                ',&
           &'  # typically unit tests of the package only used during package                ',&
           &'  # development.                                                                ',&
           &'                                                                                ',&
           &'name="runTests"                                                                 ',&
           &'source-dir="test"                                                               ',&
           &'main="check.f90"                                                                ',&
           &'                                                                                ',&
           &'  # you may repeat this pattern to add additional explicit test program         ',&
           &'  # parameters. The following example contains a sample of all accepted         ',&
           &'  # options.                                                                    ',&
           &'                                                                                ',&
           &'#[[ test ]]                                                                     ',&
           &'#name = "tester"                                                                ',&
           &'#source-dir="test"                                                              ',&
           &'#main="tester.f90"                                                              ',&
           &'#link = ["blas", "lapack"]                                                      ',&
           &'#[test.dependencies]                                                            ',&
           &'#M_CLI2  = { git = "https://github.com/urbanjost/M_CLI2.git" }                  ',&
           &'#M_io    = { git = "https://github.com/urbanjost/M_io.git" }                    ',&
           &'#M_system= { git = "https://github.com/urbanjost/M_system.git" }                ',&
           &'']
        endif

        littlefile=[character(len=80) ::       &
        &'program check',                      &
        &'implicit none',                      &
        &'',                                   &
        &'print *, "Put some tests in here!"', &
        &'end program check']
        ! create NAME/test/check.f90
        call warnwrite(join_path(settings%name, 'test/check.f90'), littlefile)
    endif

    if(settings%with_bare)then
    elseif(settings%with_example)then

       ! create NAME/example or stop
       call mkdir(join_path(settings%name, 'example'))
        ! create next section of fpm.toml
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile, &
           &'[[example]]                                                                     ',&
           &'                                                                                ',&
           &'  # Example applications for a project are defined here.                        ',&
           &'  # These are run via "fpm run --example NAME" and like the                     ',&
           &'  # test applications, are not built when this package is used as a             ',&
           &'  # dependency by other packages.                                               ',&
           &'                                                                                ',&
           &'name="demo"                                                                     ',&
           &'source-dir="example"                                                            ',&
           &'main="demo.f90"                                                                 ',&
           &'                                                                                ',&
           &'  #                                                                             ',&
           &'  # you may add additional programs to the example table. The following         ',&
           &'  # example contains a sample of all accepted options                           ',&
           &'                                                                                ',&
           &'#[[ example ]]                                                                  ',&
           &'#name = "example-tool"                                                          ',&
           &'#source-dir="example"                                                           ',&
           &'#main="tool.f90"                                                                ',&
           &'#link = "z"                                                                     ',&
           &'#[example.dependencies]                                                         ',&
           &'#M_kracken95  = { git = "https://github.com/urbanjost/M_kracken95.git" }        ',&
           &'#datetime = {git = "https://github.com/wavebitscientific/datetime-fortran.git" }',&
           &'']
        endif

        littlefile=[character(len=80) ::          &
        &'program demo',                          &
        &'implicit none',                         &
        &'',                                      &
        &'print *, "Put some examples in here!"', &
        &'end program demo']
        ! create NAME/example/demo.f90
        call warnwrite(join_path(settings%name, 'example/demo.f90'), littlefile)
    endif

    ! now that built it write NAME/fpm.toml
    if( allocated(tomlfile) )then
        call validate_toml_data(tomlfile)
        call warnwrite(join_path(settings%name, 'fpm.toml'), tomlfile)
    else
        call create_verified_basic_manifest(join_path(settings%name, 'fpm.toml'))
    endif
    ! assumes git(1) is installed and in path
    call run('git init ' // settings%name)
contains

function default_user(what) result(user)
  character(len=*), intent(in) :: what
  character(len=:), allocatable :: user
  if (what=="uname") then
    user = "Jane Doe"
  else
    user = "jane.doe@example.com"
  end if
  return
end function default_user

function git_user(what) result(user)
  use fpm_filesystem, only : get_temp_filename, getline
  character(len=*), intent(in) :: what
  character(len=:), allocatable :: user
  character(len=:), allocatable :: temp_user, iomsg
  integer :: stat, unit
  allocate(temp_user, source=get_temp_filename())
  if (what=="uname") then
    user = "git config --get user.name > " // temp_user
  else
    user = "git config --get user.email > " // temp_user
  end if
  call execute_command_line(user, exitstat=stat)
  if (stat /= 0) then
    user = default_user(what)
    return
  end if
  open(file=temp_user, newunit=unit)
  call getline(unit, user, stat, iomsg)
  if (stat /= 0) then
    user = default_user(what)
  end if
  close(unit, status="delete")
  if (len(user)==0) then
    user = default_user(what)
  end if
  return
end function git_user

subroutine create_verified_basic_manifest(filename)
!> create a basic but verified default manifest file
use fpm_toml, only : toml_table, toml_serializer, set_value
use fpm_manifest_package, only : package_config_t, new_package
use fpm_error, only : error_t
implicit none
character(len=*),intent(in) :: filename
   type(toml_table)            :: table
   type(toml_serializer)       :: ser
   type(package_config_t)      :: package
   type(error_t), allocatable  :: error
   integer                     :: lun
   character(len=8)            :: date

    if(exists(filename))then
       write(stderr,'(*(g0,1x))')'<INFO>  ',filename,&
       & 'already exists. Not overwriting'
       return
    endif
    !> get date to put into metadata in manifest file "fpm.toml"
    call date_and_time(DATE=date)
    table = toml_table()
    ser = toml_serializer()
    call fileopen(filename,lun) ! fileopen stops on error

    call set_value(table, "name",       BNAME)
    call set_value(table, "version",    "0.1.0")
    call set_value(table, "license",    "license")
    call set_value(table, "author",     git_user("uname"))
    call set_value(table, "maintainer", git_user("email"))
    call set_value(table, "copyright",  'Copyright '//date(1:4)//', '//git_user("uname"))
    ! continue building of manifest
    ! ...
    call new_package(package, table, error=error)
    if (allocated(error)) call fpm_stop( 3,'')
    if(settings%verbose)then
       call table%accept(ser)
    endif
    ser%unit=lun
    call table%accept(ser)
    call fileclose(lun) ! fileopen stops on error

end subroutine create_verified_basic_manifest


subroutine validate_toml_data(input)
!> verify a string array is a valid fpm.toml file
!
use tomlf, only : toml_parse
use fpm_toml, only : toml_table, toml_serializer
implicit none
character(kind=tfc,len=:),intent(in),allocatable :: input(:)
character(len=1), parameter                      :: nl = new_line('a')
type(toml_table), allocatable                    :: table
character(kind=tfc, len=:), allocatable          :: joined_string
type(toml_serializer)                            :: ser

! you have to add a newline character by using the intrinsic
! function `new_line("a")` to get the lines processed correctly.
joined_string = join(input,right=nl)

if (allocated(table)) deallocate(table)
call toml_parse(table, joined_string)
if (allocated(table)) then
   if(settings%verbose)then
      ! If the TOML file is successfully parsed the table will be allocated and
      ! can be written to the standard output by passing the `toml_serializer`
      ! as visitor to the table.
      call table%accept(ser)
   endif
   call table%destroy
endif

end subroutine validate_toml_data

end subroutine cmd_new

end module fpm_cmd_new
