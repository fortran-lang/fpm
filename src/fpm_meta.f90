!># The fpm meta-package model
!>
!> This is a wrapper data type that encapsulate all pre-processing information
!> (compiler flags, linker libraries, etc.) required to correctly enable a package
!> to use a core library.
!>
!>
!>### Available core libraries
!>
!> - OpenMP
!> - MPI
!> - HDF5
!> - fortran-lang stdlib
!> - fortran-lang minpack
!>
!>
!> @note Core libraries are enabled in the [build] section of the fpm.toml manifest
!>
!>
module fpm_meta
use fpm_strings, only: string_t, len_trim, remove_newline_characters, str_begins_with_str, &
                       str_ends_with
use fpm_error, only: error_t, fatal_error, syntax_error, fpm_stop
use fpm_compiler
use fpm_model
use fpm_command_line
use fpm_manifest_dependency, only: dependency_config_t
use fpm_git, only : git_target_branch, git_target_tag
use fpm_manifest, only: package_config_t
use fpm_environment, only: get_env,os_is_unix,set_env,delete_env
use fpm_filesystem, only: run, get_temp_filename, getline, exists, canon_path, is_dir, get_dos_path
use fpm_versioning, only: version_t, new_version, regex_version_from_text
use fpm_os, only: get_absolute_path
use fpm_pkg_config
use shlex_module, only: shlex_split => split
use regex_module, only: regex
use iso_fortran_env, only: stdout => output_unit

implicit none

private

public :: resolve_metapackages

!> Type for describing a source file
type, public :: metapackage_t

    !> Package version (if supported)
    type(version_t), allocatable :: version

    logical :: has_link_libraries   = .false.
    logical :: has_link_flags       = .false.
    logical :: has_build_flags      = .false.
    logical :: has_fortran_flags    = .false.
    logical :: has_c_flags          = .false.
    logical :: has_cxx_flags        = .false.
    logical :: has_include_dirs     = .false.
    logical :: has_dependencies     = .false.
    logical :: has_run_command      = .false.
    logical :: has_external_modules = .false.

    !> List of compiler flags and options to be added
    type(string_t) :: flags
    type(string_t) :: fflags
    type(string_t) :: cflags
    type(string_t) :: cxxflags
    type(string_t) :: link_flags
    type(string_t) :: run_command
    type(string_t), allocatable :: incl_dirs(:)
    type(string_t), allocatable :: link_libs(:)
    type(string_t), allocatable :: external_modules(:)

    !> Special fortran features
    type(fortran_features_t), allocatable :: fortran

    !> List of Development dependency meta data.
    !> Metapackage dependencies are never exported from the model
    type(dependency_config_t), allocatable :: dependency(:)

    contains

       !> Clean metapackage structure
       procedure :: destroy

       !> Initialize the metapackage structure from its given name
       procedure :: new => init_from_name

       !> Add metapackage dependencies to the model
       procedure, private :: resolve_cmd
       procedure, private :: resolve_model
       procedure, private :: resolve_package_config
       generic :: resolve => resolve_cmd,resolve_model,resolve_package_config

end type metapackage_t

interface resolve_metapackages
    module procedure resolve_metapackage_model
end interface resolve_metapackages

integer, parameter :: MPI_TYPE_NONE    = 0
integer, parameter :: MPI_TYPE_OPENMPI = 1
integer, parameter :: MPI_TYPE_MPICH   = 2
integer, parameter :: MPI_TYPE_INTEL   = 3
integer, parameter :: MPI_TYPE_MSMPI   = 4
public             :: MPI_TYPE_NAME

!> Debugging information
logical, parameter, private :: verbose = .false.

integer, parameter, private :: LANG_FORTRAN = 1
integer, parameter, private :: LANG_C       = 2
integer, parameter, private :: LANG_CXX     = 3

character(*), parameter :: LANG_NAME(*) = [character(7) :: 'Fortran','C','C++']

contains

!> Return a name for the MPI library
pure function MPI_TYPE_NAME(mpilib) result(name)
   integer, intent(in) :: mpilib
   character(len=:), allocatable :: name
   select case (mpilib)
      case (MPI_TYPE_NONE);    name = "none"
      case (MPI_TYPE_OPENMPI); name = "OpenMPI"
      case (MPI_TYPE_MPICH);   name = "MPICH"
      case (MPI_TYPE_INTEL);   name = "INTELMPI"
      case (MPI_TYPE_MSMPI);   name = "MS-MPI"
      case default;            name = "UNKNOWN"
   end select
end function MPI_TYPE_NAME

!> Clean the metapackage structure
elemental subroutine destroy(this)
   class(metapackage_t), intent(inout) :: this

   this%has_link_libraries   = .false.
   this%has_link_flags       = .false.
   this%has_build_flags      = .false.
   this%has_fortran_flags    = .false.
   this%has_c_flags          = .false.
   this%has_cxx_flags        = .false.
   this%has_include_dirs     = .false.
   this%has_dependencies     = .false.
   this%has_run_command      = .false.
   this%has_external_modules = .false.

   if (allocated(this%fortran)) deallocate(this%fortran)
   if (allocated(this%version)) deallocate(this%version)
   if (allocated(this%flags%s)) deallocate(this%flags%s)
   if (allocated(this%fflags%s)) deallocate(this%fflags%s)
   if (allocated(this%cflags%s)) deallocate(this%cflags%s)
   if (allocated(this%cxxflags%s)) deallocate(this%cxxflags%s)
   if (allocated(this%link_flags%s)) deallocate(this%link_flags%s)
   if (allocated(this%run_command%s)) deallocate(this%run_command%s)
   if (allocated(this%link_libs)) deallocate(this%link_libs)
   if (allocated(this%dependency)) deallocate(this%dependency)
   if (allocated(this%incl_dirs)) deallocate(this%incl_dirs)
   if (allocated(this%external_modules)) deallocate(this%external_modules)

end subroutine destroy

!> Initialize a metapackage from the given name
subroutine init_from_name(this,name,compiler,error)
    class(metapackage_t), intent(inout) :: this
    character(*), intent(in) :: name
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    !> Initialize metapackage by name
    select case(name)
        case("openmp");  call init_openmp (this,compiler,error)
        case("stdlib");  call init_stdlib (this,compiler,error)
        case("minpack"); call init_minpack(this,compiler,error)
        case("mpi");     call init_mpi    (this,compiler,error)
        case("hdf5");    call init_hdf5   (this,compiler,error)
        case default
            call syntax_error(error, "Package "//name//" is not supported in [metapackages]")
            return
    end select

end subroutine init_from_name

!> Initialize OpenMP metapackage for the current system
subroutine init_openmp(this,compiler,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    !> Cleanup
    call destroy(this)

    !> OpenMP has compiler flags
    this%has_build_flags = .true.
    this%has_link_flags  = .true.

    !> OpenMP flags should be added to
    which_compiler: select case (compiler%id)
       case (id_gcc,id_f95)
            this%flags      = string_t(flag_gnu_openmp)
            this%link_flags = string_t(flag_gnu_openmp)

       case (id_intel_classic_windows,id_intel_llvm_windows)
            this%flags      = string_t(flag_intel_openmp_win)
            this%link_flags = string_t(flag_intel_openmp_win)

       case (id_intel_classic_nix,id_intel_classic_mac,&
             id_intel_llvm_nix)
            this%flags      = string_t(flag_intel_openmp)
            this%link_flags = string_t(flag_intel_openmp)

       case (id_pgi,id_nvhpc)
            this%flags      = string_t(flag_pgi_openmp)
            this%link_flags = string_t(flag_pgi_openmp)

       case (id_ibmxl)
            this%flags      = string_t(" -qsmp=omp")
            this%link_flags = string_t(" -qsmp=omp")

       case (id_nag)
            this%flags      = string_t(flag_nag_openmp)
            this%link_flags = string_t(flag_nag_openmp)

       case (id_lfortran)
            this%flags      = string_t(flag_lfortran_openmp)
            this%link_flags = string_t(flag_lfortran_openmp)

       case default

          call fatal_error(error,'openmp not supported on compiler '//compiler%name()//' yet')

    end select which_compiler


end subroutine init_openmp

!> Initialize minpack metapackage for the current system
subroutine init_minpack(this,compiler,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    !> Cleanup
    call destroy(this)

    !> minpack is queried as a dependency from the official repository
    this%has_dependencies = .true.

    allocate(this%dependency(1))

    !> 1) minpack. There are no true releases currently. Fetch HEAD
    this%dependency(1)%name = "minpack"
    this%dependency(1)%git = git_target_tag("https://github.com/fortran-lang/minpack", "v2.0.0-rc.1")
    if (.not.allocated(this%dependency(1)%git)) then
        call fatal_error(error,'cannot initialize git repo dependency for minpack metapackage')
        return
    end if

end subroutine init_minpack

!> Initialize stdlib metapackage for the current system
subroutine init_stdlib(this,compiler,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    !> Cleanup
    call destroy(this)

    !> Stdlib is queried as a dependency from the official repository
    this%has_dependencies = .true.

    allocate(this%dependency(2))

    !> 1) Test-drive
    this%dependency(1)%name = "test-drive"
    this%dependency(1)%git = git_target_branch("https://github.com/fortran-lang/test-drive","v0.4.0")
    if (.not.allocated(this%dependency(1)%git)) then
        call fatal_error(error,'cannot initialize test-drive git dependency for stdlib metapackage')
        return
    end if

    !> 2) stdlib
    this%dependency(2)%name = "stdlib"
    this%dependency(2)%git = git_target_branch("https://github.com/fortran-lang/stdlib","stdlib-fpm")
    if (.not.allocated(this%dependency(2)%git)) then
        call fatal_error(error,'cannot initialize git repo dependency for stdlib metapackage')
        return
    end if

end subroutine init_stdlib

! Resolve metapackage dependencies into the command line settings
subroutine resolve_cmd(self,settings,error)
    class(metapackage_t), intent(in) :: self
    class(fpm_cmd_settings), intent(inout) :: settings
    type(error_t), allocatable, intent(out) :: error

    ! Add customize run commands
    if (self%has_run_command) then

        select type (cmd=>settings)
           class is (fpm_run_settings) ! includes fpm_test_settings

              ! Only override runner if user has not provided a custom one
              if (.not.len_trim(cmd%runner)>0) cmd%runner = self%run_command%s

        end select

    endif

end subroutine resolve_cmd

! Resolve metapackage dependencies into the model
subroutine resolve_model(self,model,error)
    class(metapackage_t), intent(in) :: self
    type(fpm_model_t), intent(inout) :: model
    type(error_t), allocatable, intent(out) :: error

    ! Add global build flags, to apply to all sources
    if (self%has_build_flags) then
        model%fortran_compile_flags = model%fortran_compile_flags//self%flags%s
        model%c_compile_flags       = model%c_compile_flags//self%flags%s
        model%cxx_compile_flags     = model%cxx_compile_flags//self%flags%s
    endif

    ! Add language-specific flags
    if (self%has_fortran_flags) model%fortran_compile_flags = model%fortran_compile_flags//self%fflags%s
    if (self%has_c_flags)       model%c_compile_flags       = model%c_compile_flags//self%cflags%s
    if (self%has_cxx_flags)     model%cxx_compile_flags     = model%cxx_compile_flags//self%cxxflags%s

    if (self%has_link_flags) then
        model%link_flags            = model%link_flags//' '//self%link_flags%s
    end if

    if (self%has_link_libraries) then
        model%link_libraries        = [model%link_libraries,self%link_libs]
    end if

    if (self%has_include_dirs) then
        model%include_dirs          = [model%include_dirs,self%incl_dirs]
    end if

    if (self%has_external_modules) then
        model%external_modules      = [model%external_modules,self%external_modules]
    end if

end subroutine resolve_model

subroutine resolve_package_config(self,package,error)
    class(metapackage_t), intent(in) :: self
    type(package_config_t), intent(inout) :: package
    type(error_t), allocatable, intent(out) :: error

    ! All metapackage dependencies are added as dev-dependencies,
    ! as they may change if built upstream
    if (self%has_dependencies) then
        if (allocated(package%dev_dependency)) then
           package%dev_dependency = [package%dev_dependency,self%dependency]
        else
           package%dev_dependency = self%dependency
        end if
    end if

    ! Check if there are any special fortran requests which the package does not comply to
    if (allocated(self%fortran)) then

        if (self%fortran%implicit_external.neqv.package%fortran%implicit_external) then
            call fatal_error(error,'metapackage fortran error: metapackage '// &
                                   dn(self%fortran%implicit_external)//' require implicit-external, main package '//&
                                   dn(package%fortran%implicit_external))
            return
        end if

        if (self%fortran%implicit_typing.neqv.package%fortran%implicit_typing) then
            call fatal_error(error,'metapackage fortran error: metapackage '// &
                                   dn(self%fortran%implicit_external)//' require implicit-typing, main package '//&
                                   dn(package%fortran%implicit_external))
            return
        end if

    end if

    contains

    pure function dn(bool)
       logical, intent(in) :: bool
       character(len=:), allocatable :: dn
       if (bool) then
          dn = "does"
       else
          dn = "does not"
       end if
    end function dn


end subroutine resolve_package_config

! Add named metapackage dependency to the model
subroutine add_metapackage_model(model,package,settings,name,error)
    type(fpm_model_t), intent(inout) :: model
    type(package_config_t), intent(inout) :: package
    class(fpm_cmd_settings), intent(inout) :: settings
    character(*), intent(in) :: name
    type(error_t), allocatable, intent(out) :: error

    type(metapackage_t) :: meta

    !> Init metapackage
    call meta%new(name,model%compiler,error)
    if (allocated(error)) return

    !> Add it into the model
    call meta%resolve(model,error)
    if (allocated(error)) return

    !> Add it into the package
    call meta%resolve(package,error)
    if (allocated(error)) return

    !> Add it into the settings
    call meta%resolve(settings,error)
    if (allocated(error)) return

    ! If we need to run executables, there should be an MPI runner
    if (name=="mpi") then
        select type (settings)
           class is (fpm_run_settings) ! run, test
              if (.not.meta%has_run_command) &
              call fatal_error(error,"cannot find a valid mpi runner on the local host")
        end select
    endif

end subroutine add_metapackage_model

!> Resolve all metapackages into the package config
subroutine resolve_metapackage_model(model,package,settings,error)
    type(fpm_model_t), intent(inout) :: model
    type(package_config_t), intent(inout) :: package
    class(fpm_build_settings), intent(inout) :: settings
    type(error_t), allocatable, intent(out) :: error

    ! Dependencies are added to the package config, so they're properly resolved
    ! into the dependency tree later.
    ! Flags are added to the model (whose compiler needs to be already initialized)
    if (model%compiler%is_unknown()) &
    write(stdout,'(a)') '<WARNING> compiler not initialized: metapackages may not be available'

    ! OpenMP
    if (package%meta%openmp%on) then
        call add_metapackage_model(model,package,settings,"openmp",error)
        if (allocated(error)) return
    endif

    ! stdlib
    if (package%meta%stdlib%on) then
        call add_metapackage_model(model,package,settings,"stdlib",error)
        if (allocated(error)) return
    endif

    ! minpack
    if (package%meta%minpack%on) then
        call add_metapackage_model(model,package,settings,"minpack",error)
        if (allocated(error)) return
    endif

    ! Stdlib is not 100% thread safe. print a warning to the user
    if (package%meta%stdlib%on .and. package%meta%openmp%on) then
        write(stdout,'(a)')'<WARNING> both openmp and stdlib requested: some functions may not be thread-safe!'
    end if

    ! MPI
    if (package%meta%mpi%on) then
        call add_metapackage_model(model,package,settings,"mpi",error)
        if (allocated(error)) return
    endif

    ! hdf5
    if (package%meta%hdf5%on) then
        call add_metapackage_model(model,package,settings,"hdf5",error)
        if (allocated(error)) return
    endif

end subroutine resolve_metapackage_model

!> Initialize MPI metapackage for the current system
subroutine init_mpi(this,compiler,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error


    type(string_t), allocatable :: c_wrappers(:),cpp_wrappers(:),fort_wrappers(:)
    type(string_t) :: output,fwrap,cwrap,cxxwrap
    character(256) :: msg_out
    character(len=:), allocatable :: tokens(:)
    integer :: wcfit(3),mpilib(3),ic,icpp,i
    logical :: found

    !> Cleanup
    call destroy(this)

    !> Get all candidate MPI wrappers
    call mpi_wrappers(compiler,fort_wrappers,c_wrappers,cpp_wrappers)
    if (verbose) print 1, size(fort_wrappers),size(c_wrappers),size(cpp_wrappers)

    call wrapper_compiler_fit(fort_wrappers,c_wrappers,cpp_wrappers,compiler,wcfit,mpilib,error)

    if (allocated(error) .or. all(wcfit==0)) then

        !> No wrapper compiler fit. Are we on Windows? use MSMPI-specific search
        found = msmpi_init(this,compiler,error)
        if (allocated(error)) return

        !> All attempts failed
        if (.not.found) then
            call fatal_error(error,"cannot find MPI wrappers or libraries for "//compiler%name()//" compiler")
            return
        endif

    else

        if (wcfit(LANG_FORTRAN)>0) fwrap   = fort_wrappers(wcfit(LANG_FORTRAN))
        if (wcfit(LANG_C)>0)       cwrap   = c_wrappers   (wcfit(LANG_C))
        if (wcfit(LANG_CXX)>0)     cxxwrap = cpp_wrappers (wcfit(LANG_CXX))

        !> If there's only an available Fortran wrapper, and the compiler's different than fpm's baseline
        !> fortran compiler suite, we still want to enable C language flags as that is most likely being
        !> ABI-compatible anyways. However, issues may arise.
        !> see e.g. Homebrew with clabng C/C++ and GNU fortran at https://gitlab.kitware.com/cmake/cmake/-/issues/18139
        if (wcfit(LANG_FORTRAN)>0 .and. all(wcfit([LANG_C,LANG_CXX])==0)) then
            cwrap   = fort_wrappers(wcfit(LANG_FORTRAN))
            cxxwrap = fort_wrappers(wcfit(LANG_FORTRAN))
        end if

        if (verbose) print *, '+ MPI fortran wrapper: ',fwrap%s
        if (verbose) print *, '+ MPI c       wrapper: ',cwrap%s
        if (verbose) print *, '+ MPI c++     wrapper: ',cxxwrap%s

        !> Initialize MPI package from wrapper command
        call init_mpi_from_wrappers(this,compiler,mpilib(LANG_FORTRAN),fwrap,cwrap,cxxwrap,error)
        if (allocated(error)) return

        !> Request Fortran implicit typing
        if (mpilib(LANG_FORTRAN)/=MPI_TYPE_INTEL) then
            allocate(this%fortran)
            this%fortran%implicit_typing   = .true.
            this%fortran%implicit_external = .true.
        endif

    end if

    !> Not all MPI implementations offer modules mpi and mpi_f08: hence, include them
    !> to the list of external modules, so they won't be requested as standard source files
    this%has_external_modules = .true.
    this%external_modules = [string_t("mpi"),string_t("mpi_f08")]

    1 format('MPI wrappers found: fortran=',i0,' c=',i0,' c++=',i0)

end subroutine init_mpi

!> Check if we're on a 64-bit environment
!> Accept answer from https://stackoverflow.com/questions/49141093/get-system-information-with-fortran
logical function is_64bit_environment()
   use iso_c_binding, only: c_intptr_t
   integer, parameter :: nbits = bit_size(0_c_intptr_t)
   is_64bit_environment = nbits==64
end function is_64bit_environment

!> Check if there is a wrapper-compiler fit
subroutine wrapper_compiler_fit(fort_wrappers,c_wrappers,cpp_wrappers,compiler,wrap,mpi,error)
   type(string_t), allocatable, intent(in) :: fort_wrappers(:),c_wrappers(:),cpp_wrappers(:)
   type(compiler_t), intent(in) :: compiler
   type(error_t), allocatable, intent(out) :: error
   integer, intent(out), dimension(3) :: wrap, mpi

   type(error_t), allocatable :: wrap_error

   wrap = 0
   mpi  = MPI_TYPE_NONE

   if (size(fort_wrappers)>0) &
   call mpi_compiler_match(LANG_FORTRAN,fort_wrappers,compiler,wrap(LANG_FORTRAN),mpi(LANG_FORTRAN),wrap_error)

   if (size(c_wrappers)>0) &
   call mpi_compiler_match(LANG_C,c_wrappers,compiler,wrap(LANG_C),mpi(LANG_C),wrap_error)

   if (size(cpp_wrappers)>0) &
   call mpi_compiler_match(LANG_CXX,cpp_wrappers,compiler,wrap(LANG_CXX),mpi(LANG_CXX),wrap_error)

   !> Find a Fortran wrapper for the current compiler
   if (all(wrap==0)) then
        call fatal_error(error,'no valid wrappers match current compiler, '//compiler_name(compiler))
        return
   end if

end subroutine wrapper_compiler_fit

!> Check if a local MS-MPI SDK build is found
logical function msmpi_init(this,compiler,error) result(found)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: incdir,windir,libdir,bindir,post,reall,msysdir
    type(version_t) :: ver,ver10
    type(string_t) :: cpath,msys_path,runner_path
    logical :: msys2

    !> Default: not found
    found = .false.

    if (get_os_type()==OS_WINDOWS) then

        ! to run MSMPI on Windows,
        is_minGW: if (compiler%id==id_gcc) then

            call compiler_get_version(compiler,ver,msys2,error)
            if (allocated(error)) return

        endif is_minGW

        ! Check we're on a 64-bit environment
        if (is_64bit_environment()) then
            libdir = get_env('MSMPI_LIB64')
            post   = 'x64'
        else
            libdir = get_env('MSMPI_LIB32')
            post   = 'x86'

            !> Not working on 32-bit Windows yet
            call fatal_error(error,'MS-MPI error: this package requires 64-bit Windows environment')
            return

        end if

        ! Check that the runtime is installed
        bindir = ""
        call get_absolute_path(get_env('MSMPI_BIN'),bindir,error)
        if (verbose) print *, '+ %MSMPI_BIN%=',bindir

        ! In some environments, variable %MSMPI_BIN% is missing (i.e. in GitHub Action images).
        ! Do a second attempt: search for the default location
        if (len_trim(bindir)<=0 .or. allocated(error)) then
            if (verbose) print *, '+ %MSMPI_BIN% empty, searching C:\Program Files\Microsoft MPI\Bin\ ...'
            call get_absolute_path('C:\Program Files\Microsoft MPI\Bin\mpiexec.exe',bindir,error)
        endif

        ! Third attempt for bash-style shell
        if (len_trim(bindir)<=0 .or. allocated(error)) then
            if (verbose) print *, '+ %MSMPI_BIN% empty, searching /c/Program Files/Microsoft MPI/Bin/ ...'
            call get_absolute_path('/c/Program Files/Microsoft MPI/Bin/mpiexec.exe',bindir,error)
        endif

        ! Do a fourth attempt: search for mpiexec.exe in PATH location
        if (len_trim(bindir)<=0 .or. allocated(error)) then
            if (verbose) print *, '+ C:\Program Files\Microsoft MPI\Bin\ not found. searching %PATH%...'

            call get_mpi_runner(runner_path,verbose,error)

            if (.not.allocated(error)) then
               if (verbose) print *, '+ mpiexec found: ',runner_path%s
               call find_command_location(runner_path%s,bindir,verbose=verbose,error=error)
            endif

        endif

        if (allocated(error)) then
            call fatal_error(error,'MS-MPI error: MS-MPI Runtime directory is missing. '//&
                                   'check environment variable %MSMPI_BIN% or that the folder is in %PATH%.')
            return
        end if

        ! Success!
        found = .true.

        ! Init ms-mpi
        call destroy(this)

        ! MSYS2 provides a pre-built static msmpi.dll.a library. Use that if possible
        use_prebuilt: if (msys2) then

            ! MSYS executables are in %MSYS_ROOT%/bin
            call compiler_get_path(compiler,cpath,error)
            if (allocated(error)) return

            call get_absolute_path(join_path(cpath%s,'..'),msys_path%s,error)
            if (allocated(error)) return

            call get_absolute_path(join_path(msys_path%s,'include'),incdir,error)
            if (allocated(error)) return

            call get_absolute_path(join_path(msys_path%s,'lib'),libdir,error)
            if (allocated(error)) return

            if (verbose) print 1, 'include',incdir,exists(incdir)
            if (verbose) print 1, 'library',libdir,exists(libdir)

            ! Check that the necessary files exist
            call get_absolute_path(join_path(libdir,'libmsmpi.dll.a'),post,error)
            if (allocated(error)) return

            if (len_trim(post)<=0 .or. .not.exists(post)) then
                call fatal_error(error,'MS-MPI available through the MSYS2 system not found. '// &
                                       'Run <pacman -Sy mingw64/mingw-w64-x86_64-msmpi> '// &
                                       'or your system-specific version to install.')
                return
            end if

            ! Add dir cpath
            this%has_link_flags = .true.
            this%link_flags = string_t(' -L'//get_dos_path(libdir,error))

            this%has_link_libraries = .true.
            this%link_libs = [string_t('msmpi.dll')]

            if (allocated(error)) return

            this%has_include_dirs = .true.
            this%incl_dirs = [string_t(get_dos_path(incdir,error))]
            if (allocated(error)) return

        else

            call fatal_error(error,'MS-MPI cannot work with non-MSYS2 GNU compilers yet')
            return

            ! Add dir path
            this%has_link_flags = .true.
            this%link_flags = string_t(' -L'//get_dos_path(libdir,error))

            this%has_link_libraries = .true.
            this%link_libs = [string_t('msmpi'),string_t('msmpifec'),string_t('msmpifmc')]

            if (allocated(error)) return

            this%has_include_dirs = .true.
            this%incl_dirs = [string_t(get_dos_path(incdir,error)), &
                              string_t(get_dos_path(incdir//post,error))]
            if (allocated(error)) return


        end if use_prebuilt

        !> Request Fortran implicit typing
        allocate(this%fortran)
        this%fortran%implicit_typing = .true.
        this%fortran%implicit_external = .true.

        ! gfortran>=10 is incompatible with the old-style mpif.h MS-MPI headers.
        ! If so, add flags to allow old-style BOZ constants in mpif.h
        allow_BOZ: if (compiler%id==id_gcc) then

            call new_version(ver10,'10.0.0',error)
            if (allocated(error)) return

            if (ver>=ver10) then
                this%has_build_flags = .true.
                this%flags = string_t(' -fallow-invalid-boz')
            end if

        endif allow_BOZ

        !> Add default run command
        this%has_run_command = .true.
        this%run_command = string_t(join_path(get_dos_path(bindir,error),'mpiexec.exe')//' -np * ')

    else

        !> Not on Windows
        found = .false.

    end if

    1 format('MSMSPI ',a,' directory: PATH=',a,' EXISTS=',l1)

end function msmpi_init

!> Check if we're under a WSL bash shell
logical function wsl_shell()
    if (get_os_type()==OS_WINDOWS) then
        wsl_shell = exists('/proc/sys/fs/binfmt_misc/WSLInterop')
    else
        wsl_shell = .false.
    endif
end function wsl_shell

!> Find the location of a valid command
subroutine find_command_location(command,path,echo,verbose,error)
    character(*), intent(in) :: command
    character(len=:), allocatable, intent(out) :: path
    logical, optional, intent(in) :: echo,verbose
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: tmp_file,screen_output,line,fullpath,search_command
    integer :: stat,iunit,ire,length,try
    character(*), parameter :: search(2) = ["where ","which "]

    if (len_trim(command)<=0) then
        call fatal_error(error,'empty command provided in find_command_location')
        return
    end if

    tmp_file = get_temp_filename()

    ! On Windows, we try both commands because we may be on WSL
    do try=merge(1,2,get_os_type()==OS_WINDOWS),2
       search_command = search(try)//command
       call run(search_command, echo=echo, exitstat=stat, verbose=verbose, redirect=tmp_file)
       if (stat==0) exit
    end do
    if (stat/=0) then
        call fatal_error(error,'find_command_location failed for '//command)
        return
    end if

    ! Only read first instance (first line)
    allocate(character(len=0) :: screen_output)
    open(newunit=iunit,file=tmp_file,status='old',iostat=stat)
    if (stat == 0)then
       do
           call getline(iunit, line, stat)
           if (stat /= 0) exit
           if (len(screen_output)>0) then
                screen_output = screen_output//new_line('a')//line
           else
                screen_output = line
           endif
       end do
       ! Close and delete file
       close(iunit,status='delete')
    else
       call fatal_error(error,'cannot read temporary file from successful find_command_location')
       return
    endif

    ! Only use the first instance
    length = index(screen_output,new_line('a'))

    multiline: if (length>1) then
        fullpath = screen_output(1:length-1)
    else
        fullpath = screen_output
    endif multiline
    if (len_trim(fullpath)<1) then
        call fatal_error(error,'no paths found to command ('//command//')')
        return
    end if

    ! Extract path only
    length = index(fullpath,command,BACK=.true.)
    if (length<=0) then
        call fatal_error(error,'full path to command ('//command//') does not include command name')
        return
    elseif (length==1) then
        ! Compiler is in the current folder
        path = '.'
    else
        path = fullpath(1:length-1)
    end if
    if (allocated(error)) return

    ! On Windows, be sure to return a path with no spaces
    if (get_os_type()==OS_WINDOWS) path = get_dos_path(path,error)

    if (allocated(error) .or. .not.is_dir(path)) then
        call fatal_error(error,'full path ('//path//') to command ('//command//') is not a directory')
        return
    end if

end subroutine find_command_location

!> Get MPI runner in $PATH
subroutine get_mpi_runner(command,verbose,error)
    type(string_t), intent(out) :: command
    logical, intent(in) :: verbose
    type(error_t), allocatable, intent(out) :: error

    character(*), parameter :: try(*) = ['mpiexec    ','mpirun     ','mpiexec.exe','mpirun.exe ']
    character(:), allocatable :: bindir
    integer :: itri
    logical :: success

    ! Try several commands
    do itri=1,size(try)
       call find_command_location(trim(try(itri)),command%s,verbose=verbose,error=error)
       if (allocated(error)) cycle

       ! Success!
       success = len_trim(command%s)>0
       if (success) then
           if (verbose) print *, '+ runner folder found: '//command%s
           command%s = join_path(command%s,trim(try(itri)))
           return
       endif
    end do

    ! On windows, also search in %MSMPI_BIN%
    if (get_os_type()==OS_WINDOWS) then
        ! Check that the runtime is installed
        bindir = ""
        call get_absolute_path(get_env('MSMPI_BIN'),bindir,error)
        if (verbose) print *, '+ %MSMPI_BIN%=',bindir
        ! In some environments, variable %MSMPI_BIN% is missing (i.e. in GitHub Action images).
        ! Do a second attempt: search for the default location
        if (len_trim(bindir)<=0 .or. allocated(error)) then
            if (verbose) print *, '+ %MSMPI_BIN% empty, searching C:\Program Files\Microsoft MPI\Bin\ ...'
            call get_absolute_path('C:\Program Files\Microsoft MPI\Bin\mpiexec.exe',bindir,error)
        endif
        if (len_trim(bindir)>0 .and. .not.allocated(error)) then
            ! MSMPI_BIN directory found
            command%s = join_path(bindir,'mpiexec.exe')
            return
        endif
    endif

    ! No valid command found
    call fatal_error(error,'cannot find a valid mpi runner command')
    return

end subroutine get_mpi_runner

!> Return compiler path
subroutine compiler_get_path(self,path,error)
    type(compiler_t), intent(in) :: self
    type(string_t), intent(out) :: path
    type(error_t), allocatable, intent(out) :: error

    call find_command_location(self%fc,path%s,self%echo,self%verbose,error)

end subroutine compiler_get_path

!> Return compiler version
subroutine compiler_get_version(self,version,is_msys2,error)
    type(compiler_t), intent(in) :: self
    type(version_t), intent(out) :: version
    logical, intent(out) :: is_msys2
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: tmp_file,screen_output,line
    type(string_t) :: ver
    integer :: stat,iunit,ire,length

    is_msys2 = .false.

    select case (self%id)
       case (id_gcc)

            tmp_file = get_temp_filename()

            call run(self%fc // " --version ", echo=self%echo, verbose=self%verbose, redirect=tmp_file, exitstat=stat)
            if (stat/=0) then
                call fatal_error(error,'compiler_get_version failed for '//self%fc)
                return
            end if

            allocate(character(len=0) :: screen_output)
            open(newunit=iunit,file=tmp_file,status='old',iostat=stat)
            if (stat == 0)then
               do
                   call getline(iunit, line, stat)
                   if (stat /= 0) exit
                   screen_output = screen_output//' '//line//' '
               end do
               ! Close and delete file
               close(iunit,status='delete')
            else
               call fatal_error(error,'cannot read temporary file from successful compiler_get_version')
               return
            endif

            ! Check if this gcc is from the MSYS2 project
            is_msys2 = index(screen_output,'MSYS2')>0

            ver = regex_version_from_text(screen_output,self%fc//' compiler',error)
            if (allocated(error)) return

            ! Extract version
            call new_version(version,ver%s,error)


       case default
            call fatal_error(error,'compiler_get_version not yet implemented for compiler '//self%fc)
            return
    end select

end subroutine compiler_get_version

!> Initialize an MPI metapackage from a valid wrapper command ('mpif90', etc...)
subroutine init_mpi_from_wrappers(this,compiler,mpilib,fort_wrapper,c_wrapper,cxx_wrapper,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    integer, intent(in) :: mpilib
    type(string_t), intent(in) :: fort_wrapper,c_wrapper,cxx_wrapper
    type(error_t), allocatable, intent(out) :: error

    type(version_t) :: version
    type(error_t), allocatable :: runner_error

    ! Cleanup structure
    call destroy(this)

    ! Get linking flags
    this%link_flags = mpi_wrapper_query(mpilib,fort_wrapper,'link',verbose,error)
    if (allocated(error)) return

    ! Remove useless/dangerous flags
    call filter_link_arguments(compiler,this%link_flags)

    this%has_link_flags = len_trim(this%link_flags)>0

    ! Request to use libs in arbitrary order
    if (this%has_link_flags .and. compiler%is_gnu() .and. os_is_unix() .and. get_os_type()/=OS_MACOS) then
        this%link_flags = string_t(' -Wl,--start-group '//this%link_flags%s)
    end if

    ! Add language-specific flags
    call set_language_flags(compiler,mpilib,fort_wrapper,this%has_fortran_flags,this%fflags,verbose,error)
    if (allocated(error)) return
    call set_language_flags(compiler,mpilib,c_wrapper,this%has_c_flags,this%cflags,verbose,error)
    if (allocated(error)) return
    call set_language_flags(compiler,mpilib,cxx_wrapper,this%has_cxx_flags,this%cxxflags,verbose,error)
    if (allocated(error)) return

    ! Get library version
    version = mpi_version_get(mpilib,fort_wrapper,error)
    if (allocated(error)) then
       return
    else
       allocate(this%version,source=version)
    end if

    !> Add default run command, if present
    this%run_command = mpi_wrapper_query(mpilib,fort_wrapper,'runner',verbose,runner_error)
    this%has_run_command = (len_trim(this%run_command)>0) .and. .not.allocated(runner_error)

    contains

    subroutine set_language_flags(compiler,mpilib,wrapper,has_flags,flags,verbose,error)
        type(compiler_t), intent(in) :: compiler
        integer, intent(in) :: mpilib
        type(string_t), intent(in) :: wrapper
        logical, intent(inout) :: has_flags
        type(string_t), intent(inout) :: flags
        logical, intent(in) :: verbose
        type(error_t), allocatable, intent(out) :: error

        ! Get build flags for each language
        if (len_trim(wrapper)>0) then
            flags = mpi_wrapper_query(mpilib,wrapper,'flags',verbose,error)

            if (allocated(error)) return
            has_flags = len_trim(flags)>0

            ! Add heading space
            flags = string_t(' '//flags%s)

            if (verbose) print *, '+ MPI language flags from wrapper <',wrapper%s,'>: flags=',flags%s

            call filter_build_arguments(compiler,flags)

        endif

    end subroutine set_language_flags

end subroutine init_mpi_from_wrappers

!> Match one of the available compiler wrappers with the current compiler
subroutine mpi_compiler_match(language,wrappers,compiler,which_one,mpilib,error)
    integer, intent(in) :: language
    type(string_t), intent(in) :: wrappers(:)
    type(compiler_t), intent(in) :: compiler
    integer, intent(out) :: which_one, mpilib
    type(error_t), allocatable, intent(out) :: error

    integer :: i, same_vendor, vendor_mpilib
    type(string_t) :: screen
    character(128) :: msg_out
    type(compiler_t) :: mpi_compiler

    which_one   = 0
    same_vendor = 0
    mpilib      = MPI_TYPE_NONE

    if (verbose) print *, '+ Trying to match available ',LANG_NAME(language),' MPI wrappers to ',compiler%fc,'...'

    do i=1,size(wrappers)

        mpilib = which_mpi_library(wrappers(i),compiler,verbose=.false.)

        screen = mpi_wrapper_query(mpilib,wrappers(i),'compiler',verbose=.false.,error=error)
        if (allocated(error)) return

        if (verbose) print *, '  Wrapper ',wrappers(i)%s,' lib=',MPI_TYPE_NAME(mpilib),' uses ',screen%s

        select case (language)
           case (LANG_FORTRAN)
               ! Build compiler type. The ID is created based on the Fortran name
               call new_compiler(mpi_compiler,screen%s,'','',echo=.true.,verbose=.false.)

               ! Fortran match found!
               if (mpi_compiler%id == compiler%id) then
                   which_one = i
                   return
               end if
           case (LANG_C)
               ! For other languages, we can only hope that the name matches the expected one
               if (screen%s==compiler%cc .or. screen%s==compiler%fc) then
                   which_one = i
                   return
               end if
           case (LANG_CXX)
               if (screen%s==compiler%cxx .or. screen%s==compiler%fc) then
                   which_one = i
                   return
               end if
        end select

        ! Because the intel mpi library does not support llvm_ compiler wrappers yet,
        ! we must check for that manually
        if (is_intel_classic_option(language,same_vendor,screen,compiler,mpi_compiler)) then
            same_vendor = i
            vendor_mpilib = mpilib
        end if
    end do

    ! Intel compiler: if an exact match is not found, attempt closest wrapper
    if (which_one==0 .and. same_vendor>0) then
        which_one = same_vendor
        mpilib    = vendor_mpilib
    end if

    ! None of the available wrappers matched the current Fortran compiler
    write(msg_out,1) size(wrappers),compiler%fc
    call fatal_error(error,trim(msg_out))
    1 format('<ERROR> None out of ',i0,' valid MPI wrappers matches compiler ',a)

end subroutine mpi_compiler_match

!> Because the Intel mpi library does not support llvm_ compiler wrappers yet,
!> we must save the Intel-classic option and later manually replace it
logical function is_intel_classic_option(language,same_vendor_ID,screen_out,compiler,mpi_compiler)
    integer, intent(in) :: language,same_vendor_ID
    type(string_t), intent(in) :: screen_out
    type(compiler_t), intent(in) :: compiler,mpi_compiler

    if (same_vendor_ID/=0) then
        is_intel_classic_option = .false.
    else
        select case (language)
           case (LANG_FORTRAN)
               is_intel_classic_option = mpi_compiler%is_intel() .and. compiler%is_intel()
           case (LANG_C)
               is_intel_classic_option = screen_out%s=='icc' .and. compiler%cc=='icx'
           case (LANG_CXX)
               is_intel_classic_option = screen_out%s=='icpc' .and. compiler%cc=='icpx'
        end select
    end if

end function is_intel_classic_option

!> Return library version from the MPI wrapper command
type(version_t) function mpi_version_get(mpilib,wrapper,error)
   integer, intent(in) :: mpilib
   type(string_t), intent(in) :: wrapper
   type(error_t), allocatable, intent(out) :: error

   type(string_t) :: version_line

   ! Get version string
   version_line = mpi_wrapper_query(mpilib,wrapper,'version',error=error)
   if (allocated(error)) return

   ! Wrap to object
   call new_version(mpi_version_get,version_line%s,error)

end function mpi_version_get

!> Return several mpi wrappers, and return
subroutine mpi_wrappers(compiler,fort_wrappers,c_wrappers,cpp_wrappers)
    type(compiler_t), intent(in) :: compiler
    type(string_t), allocatable, intent(out) :: c_wrappers(:),cpp_wrappers(:),fort_wrappers(:)

    character(len=:), allocatable :: mpi_root,intel_wrap
    type(error_t), allocatable :: error

    ! Attempt gathering MPI wrapper names from the environment variables
    c_wrappers    = [string_t(get_env('MPICC' ,'mpicc'))]
    cpp_wrappers  = [string_t(get_env('MPICXX','mpic++'))]
    fort_wrappers = [string_t(get_env('MPIFC' ,'mpifc' )),&
                     string_t(get_env('MPIf90','mpif90')),&
                     string_t(get_env('MPIf77','mpif77'))]

    if (get_os_type()==OS_WINDOWS) then
        c_wrappers = [c_wrappers,string_t('mpicc.bat')]
        cpp_wrappers = [cpp_wrappers,string_t('mpicxx.bat')]
        fort_wrappers = [fort_wrappers,string_t('mpifc.bat')]
    endif

    ! Add compiler-specific wrappers
    compiler_specific: select case (compiler%id)
       case (id_gcc,id_f95)

            c_wrappers = [c_wrappers,string_t('mpigcc'),string_t('mpgcc')]
          cpp_wrappers = [cpp_wrappers,string_t('mpig++'),string_t('mpg++')]
         fort_wrappers = [fort_wrappers,string_t('mpigfortran'),string_t('mpgfortran'),&
                          string_t('mpig77'),string_t('mpg77')]

       case (id_intel_classic_windows,id_intel_llvm_windows, &
             id_intel_classic_nix,id_intel_classic_mac,id_intel_llvm_nix,id_intel_llvm_unknown)

            c_wrappers = [string_t(get_env('I_MPI_CC','mpiicc'))]
          cpp_wrappers = [string_t(get_env('I_MPI_CXX','mpiicpc'))]
         fort_wrappers = [string_t(get_env('I_MPI_F90','mpiifort'))]

         ! Also search MPI wrappers via the base MPI folder
         mpi_root = get_env('I_MPI_ROOT')
         if (mpi_root/="") then

             mpi_root = join_path(mpi_root,'bin')

             intel_wrap = join_path(mpi_root,'mpiifort')
             if (get_os_type()==OS_WINDOWS) intel_wrap = get_dos_path(intel_wrap,error)
             if (intel_wrap/="") fort_wrappers = [fort_wrappers,string_t(intel_wrap)]

             intel_wrap = join_path(mpi_root,'mpiicc')
             if (get_os_type()==OS_WINDOWS) intel_wrap = get_dos_path(intel_wrap,error)
             if (intel_wrap/="") c_wrappers = [c_wrappers,string_t(intel_wrap)]

             intel_wrap = join_path(mpi_root,'mpiicpc')
             if (get_os_type()==OS_WINDOWS) intel_wrap = get_dos_path(intel_wrap,error)
             if (intel_wrap/="") cpp_wrappers = [cpp_wrappers,string_t(intel_wrap)]

         end if

       case (id_pgi,id_nvhpc)

            c_wrappers = [c_wrappers,string_t('mpipgicc'),string_t('mpgcc')]
          cpp_wrappers = [cpp_wrappers,string_t('mpipgic++')]
         fort_wrappers = [fort_wrappers,string_t('mpipgifort'),string_t('mpipgf90')]

       case (id_cray)

            c_wrappers = [c_wrappers,string_t('cc')]
          cpp_wrappers = [cpp_wrappers,string_t('CC')]
         fort_wrappers = [fort_wrappers,string_t('ftn')]

    end select compiler_specific

    call assert_mpi_wrappers(fort_wrappers,compiler)
    call assert_mpi_wrappers(c_wrappers,compiler)
    call assert_mpi_wrappers(cpp_wrappers,compiler)

end subroutine mpi_wrappers

!> Filter out invalid/unavailable mpi wrappers
subroutine assert_mpi_wrappers(wrappers,compiler,verbose)
    type(string_t), allocatable, intent(inout) :: wrappers(:)
    type(compiler_t), intent(in) :: compiler
    logical, optional, intent(in) :: verbose

    integer :: i
    integer, allocatable :: works(:)

    allocate(works(size(wrappers)))

    do i=1,size(wrappers)
        if (present(verbose)) then
            if (verbose) print *, '+ MPI test wrapper <',wrappers(i)%s,'>'
        endif
        works(i) = which_mpi_library(wrappers(i),compiler,verbose)
    end do

    ! Filter out non-working wrappers
    wrappers = pack(wrappers,works/=MPI_TYPE_NONE)

end subroutine assert_mpi_wrappers

!> Get MPI library type from the wrapper command. Currently, only OpenMPI is supported
integer function which_mpi_library(wrapper,compiler,verbose)
    type(string_t), intent(in) :: wrapper
    type(compiler_t), intent(in) :: compiler
    logical, intent(in), optional :: verbose

    logical :: is_mpi_wrapper
    integer :: stat

    ! Init as currently unsupported library
    which_mpi_library = MPI_TYPE_NONE

    if (len_trim(wrapper)<=0) return

    ! Run mpi wrapper first
    call run_wrapper(wrapper,verbose=verbose,cmd_success=is_mpi_wrapper)

    if (is_mpi_wrapper) then

        if (compiler%is_intel()) then
            which_mpi_library = MPI_TYPE_INTEL
            return
        end if

        ! Attempt to decipher which library this wrapper comes from.

        ! OpenMPI responds to '--showme' calls
        call run_wrapper(wrapper,[string_t('--showme')],verbose,&
                             exitcode=stat,cmd_success=is_mpi_wrapper)
        if (stat==0 .and. is_mpi_wrapper) then
            which_mpi_library = MPI_TYPE_OPENMPI
            return
        endif

        ! MPICH responds to '-show' calls
        call run_wrapper(wrapper,[string_t('-show')],verbose,&
                             exitcode=stat,cmd_success=is_mpi_wrapper)
        if (stat==0 .and. is_mpi_wrapper) then
            which_mpi_library = MPI_TYPE_MPICH
            return
        endif

    end if

end function which_mpi_library

!> Test if an MPI wrapper works
type(string_t) function mpi_wrapper_query(mpilib,wrapper,command,verbose,error) result(screen)
    integer, intent(in) :: mpilib
    type(string_t), intent(in) :: wrapper
    character(*), intent(in) :: command
    logical, intent(in), optional :: verbose
    type(error_t), allocatable, intent(out) :: error

    logical :: success
    character(:), allocatable :: redirect_str,tokens(:),unsupported_msg
    type(string_t) :: cmdstr
    type(compiler_t) :: mpi_compiler
    integer :: stat,cmdstat,ire,length

    unsupported_msg = 'the MPI library of wrapper '//wrapper%s//' does not support task '//trim(command)

    select case (command)

       ! Get MPI compiler name
       case ('compiler')

           select case (mpilib)
              case (MPI_TYPE_OPENMPI); cmdstr = string_t('--showme:command')
              case (MPI_TYPE_MPICH);   cmdstr = string_t('-compile-info')
              case (MPI_TYPE_INTEL);   cmdstr = string_t('-show')
              case default
                 call fatal_error(error,unsupported_msg)
                 return
           end select

           call run_wrapper(wrapper,[cmdstr],verbose=verbose, &
                                exitcode=stat,cmd_success=success,screen_output=screen)

           if (stat/=0 .or. .not.success) then
              call syntax_error(error,'local '//MPI_TYPE_NAME(mpilib)//&
                                      ' library wrapper does not support flag '//cmdstr%s)
              return
           end if

           ! Take out the first command from the whole line
           call remove_newline_characters(screen)
           call split(screen%s,tokens,delimiters=' ')
           screen%s = trim(adjustl(tokens(1)))

       ! Get a list of additional compiler flags
       case ('flags')

           select case (mpilib)
              case (MPI_TYPE_OPENMPI); cmdstr = string_t('--showme:compile')
              case (MPI_TYPE_MPICH);   cmdstr = string_t('-compile-info')
              case (MPI_TYPE_INTEL);   cmdstr = string_t('-show')
              case default
                 call fatal_error(error,unsupported_msg)
                 return
           end select

           call run_wrapper(wrapper,[cmdstr],verbose=verbose, &
                                exitcode=stat,cmd_success=success,screen_output=screen)

           if (stat/=0 .or. .not.success) then
              call syntax_error(error,'local '//MPI_TYPE_NAME(mpilib)//&
                                      ' library wrapper does not support flag '//cmdstr%s)
              return
           end if

           ! Post-process output
           select case (mpilib)
              case (MPI_TYPE_OPENMPI)
                 ! This library reports the compiler name only
                 call remove_newline_characters(screen)
              case (MPI_TYPE_MPICH,MPI_TYPE_INTEL)
                 ! These libraries report the full command including the compiler name. Remove it if so
                 call remove_newline_characters(screen)
                 call split(screen%s,tokens)
                 ! Remove trailing compiler name
                 screen%s = screen%s(len_trim(tokens(1))+1:)
              case default
                 call fatal_error(error,'invalid MPI library type')
                 return
           end select

       ! Get a list of additional linker flags
       case ('link')

           select case (mpilib)
              case (MPI_TYPE_OPENMPI); cmdstr = string_t('--showme:link')
              case (MPI_TYPE_MPICH);   cmdstr = string_t('-link-info')
              case (MPI_TYPE_INTEL);   cmdstr = string_t('-show')
              case default
                 call fatal_error(error,unsupported_msg)
                 return
           end select

           call run_wrapper(wrapper,[cmdstr],verbose=verbose, &
                                exitcode=stat,cmd_success=success,screen_output=screen)

           if (stat/=0 .or. .not.success) then
              call syntax_error(error,'local '//MPI_TYPE_NAME(mpilib)//&
                                      ' library wrapper does not support flag '//cmdstr%s)
              return
           end if

           select case (mpilib)
              case (MPI_TYPE_OPENMPI)
                 call remove_newline_characters(screen)
              case (MPI_TYPE_MPICH,MPI_TYPE_INTEL)
                 ! MPICH reports the full command including the compiler name. Remove it if so
                 call remove_newline_characters(screen)
                 call split(screen%s,tokens)
                 ! Remove trailing compiler name
                 screen%s = screen%s(len_trim(tokens(1))+1:)
              case default
                 call fatal_error(error,unsupported_msg)
                 return
           end select

       ! Get a list of MPI library directories
       case ('link_dirs')

           select case (mpilib)
              case (MPI_TYPE_OPENMPI)

                 ! --showme:command returns the build command of this wrapper
                 call run_wrapper(wrapper,[string_t('--showme:libdirs')],verbose=verbose, &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local OpenMPI library does not support --showme:libdirs')
                    return
                 end if

              case default

                 call fatal_error(error,unsupported_msg)
                 return

           end select

       ! Get a list of include directories for the MPI headers/modules
       case ('incl_dirs')

           select case (mpilib)
              case (MPI_TYPE_OPENMPI)
                 ! --showme:command returns the build command of this wrapper
                 call run_wrapper(wrapper,[string_t('--showme:incdirs')],verbose=verbose, &
                                      exitcode=stat,cmd_success=success,screen_output=screen)
                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local OpenMPI library does not support --showme:incdirs')
                    return
                 end if
              case default
                 call fatal_error(error,unsupported_msg)
                 return
           end select

           call remove_newline_characters(screen)

       ! Retrieve library version
       case ('version')

           select case (mpilib)
              case (MPI_TYPE_OPENMPI)

                 ! --showme:command returns the build command of this wrapper
                 call run_wrapper(wrapper,[string_t('--showme:version')],verbose=verbose, &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local OpenMPI library does not support --showme:version')
                    return
                 else
                    call remove_newline_characters(screen)
                 end if

              case (MPI_TYPE_MPICH)

                 !> MPICH offers command "mpichversion" in the same system folder as the MPI wrappers.
                 !> So, attempt to run that first
                 cmdstr = string_t('mpichversion')
                 call run_wrapper(cmdstr,verbose=verbose, &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 ! Second option: run mpich wrapper + "-v"
                 if (stat/=0 .or. .not.success) then
                    call run_wrapper(wrapper,[string_t('-v')],verbose=verbose, &
                                         exitcode=stat,cmd_success=success,screen_output=screen)
                    call remove_newline_characters(screen)
                 endif

                 ! Third option: mpiexec --version
                 if (stat/=0 .or. .not.success) then
                     cmdstr = string_t('mpiexec --version')
                     call run_wrapper(cmdstr,verbose=verbose, &
                                          exitcode=stat,cmd_success=success,screen_output=screen)
                 endif

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'cannot retrieve MPICH library version from <mpichversion, '//wrapper%s//', mpiexec>')
                    return
                 end if

              case (MPI_TYPE_INTEL)

                 ! --showme:command returns the build command of this wrapper
                 call run_wrapper(wrapper,[string_t('-v')],verbose=verbose, &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local INTEL MPI library does not support -v')
                    return
                 else
                    call remove_newline_characters(screen)
                 end if

              case default

                 call fatal_error(error,unsupported_msg)
                 return

           end select

           ! Extract version
           screen = regex_version_from_text(screen%s,MPI_TYPE_NAME(mpilib)//' library',error)
           if (allocated(error)) return

       ! Get path to the MPI runner command
       case ('runner')

           select case (mpilib)
              case (MPI_TYPE_OPENMPI,MPI_TYPE_MPICH,MPI_TYPE_MSMPI,MPI_TYPE_INTEL)
                 call get_mpi_runner(screen,verbose,error)
              case default
                 call fatal_error(error,unsupported_msg)
                 return
           end select

       case default;
           call fatal_error(error,'an invalid MPI wrapper command ('//command//&
                                  ') was invoked for wrapper <'//wrapper%s//'>.')
           return
    end select


end function mpi_wrapper_query

!> Check if input is a useful linker argument
logical function is_link_argument(compiler,string)
   type(compiler_t), intent(in) :: compiler
   character(*), intent(in) :: string

   select case (compiler%id)
      case (id_intel_classic_windows,id_intel_llvm_windows)
          is_link_argument = string=='/link' &
                             .or. str_begins_with_str(string,'/LIBPATH')&
                             .or. str_ends_with(string,'.lib') ! always .lib whether static or dynamic
      case default

          ! fix OpenMPI's Fortran wrapper bug (https://github.com/open-mpi/ompi/issues/11636) here
          is_link_argument = (    str_begins_with_str(string,'-L') &
                             .or. str_begins_with_str(string,'-l') &
                             .or. str_begins_with_str(string,'-Xlinker') &
                             .or. string=='-pthread' &
                             .or. (str_begins_with_str(string,'-W') .and. &
                                   (string/='-Wall') .and. (.not.str_begins_with_str(string,'-Werror'))) ) &
                             .and. .not. ( &
                                 (get_os_type()==OS_MACOS .and. index(string,'-commons,use_dylibs')>0) )
   end select

end function is_link_argument

!> From build, remove optimization and other unnecessary flags
subroutine filter_build_arguments(compiler,command)
    type(compiler_t), intent(in) :: compiler
    type(string_t), intent(inout) :: command
    character(len=:), allocatable :: tokens(:)

    integer :: i,n,re_i,re_l
    logical, allocatable :: keep(:)
    logical :: keep_next
    character(len=:), allocatable :: module_flag,include_flag

    if (len_trim(command)<=0) return

    ! Split command into arguments
    tokens = shlex_split(command%s)

    module_flag  = get_module_flag(compiler,"")
    include_flag = get_include_flag(compiler,"")

    n = size(tokens)
    allocate(keep(n),source=.false.)
    keep_next = .false.

    do i=1,n

        if (get_os_type()==OS_MACOS .and. index(tokens(i),'-commons,use_dylibs')>0) then
            keep(i) = .false.
            keep_next = .false.
        elseif (str_begins_with_str(tokens(i),'-D') .or. &
                str_begins_with_str(tokens(i),'-f') .or. &
                str_begins_with_str(tokens(i),'-I') .or. &
                str_begins_with_str(tokens(i),module_flag) .or. &
                str_begins_with_str(tokens(i),include_flag) .or. &
                tokens(i)=='-pthread' .or. &
                (str_begins_with_str(tokens(i),'-W') .and. tokens(i)/='-Wall' .and. .not.str_begins_with_str(tokens(i),'-Werror')) &
                ) then
                   keep(i) = .true.
                   if (tokens(i)==module_flag .or. tokens(i)==include_flag .or. tokens(i)=='-I') keep_next = .true.
        elseif (keep_next) then
            keep(i) = .true.
            keep_next = .false.
        end if
    end do

    ! Backfill
    command = string_t("")
    do i=1,n
        if (.not.keep(i)) cycle

        command%s = command%s//' '//trim(tokens(i))
    end do


end subroutine filter_build_arguments

!> From the linker flags, remove optimization and other unnecessary flags
subroutine filter_link_arguments(compiler,command)
    type(compiler_t), intent(in) :: compiler
    type(string_t), intent(inout) :: command
    character(len=:), allocatable :: tokens(:)

    integer :: i,n
    logical, allocatable :: keep(:)
    logical :: keep_next

    if (len_trim(command)<=0) return

    ! Split command into arguments
    tokens = shlex_split(command%s)

    n = size(tokens)
    allocate(keep(n),source=.false.)
    keep_next = .false.

    do i=1,n
       if (is_link_argument(compiler,tokens(i))) then
           keep(i) = .true.
           if (tokens(i)=='-L' .or. tokens(i)=='-Xlinker') keep_next = .true.
       elseif (keep_next) then
           keep(i) = .true.
           keep_next = .false.
       end if
    end do

    ! Backfill
    command = string_t("")
    do i=1,n
        if (.not.keep(i)) cycle
        command%s = command%s//' '//trim(tokens(i))
    end do

end subroutine filter_link_arguments

!> Given a library name and folder, find extension and prefix
subroutine lib_get_trailing(lib_name,lib_dir,prefix,suffix,found)
   character(*), intent(in) :: lib_name,lib_dir 
   character(:), allocatable, intent(out) :: prefix,suffix
   logical, intent(out) :: found
   
   character(*), parameter :: extensions(*) = [character(11) :: '.dll.a','.a','.dylib','.dll']           
   logical :: is_file
   character(:), allocatable :: noext,tokens(:),path
   integer :: l,k
    
   ! Extract name with no extension
   call split(lib_name,tokens,'.')
   noext = trim(tokens(1))        
    
   ! Get library extension: find file name: NAME.a, NAME.dll.a, NAME.dylib, libNAME.a, etc.
   found = .false.
   suffix = ""
   prefix = ""
   with_pref: do l=1,2
       if (l==2) then 
          prefix = "lib"
       else
          prefix = ""  
       end if
       find_ext: do k=1,size(extensions)                     
           path = join_path(lib_dir,prefix//noext//trim(extensions(k)))
           inquire(file=path,exist=is_file)
           
           if (is_file) then 
              suffix = trim(extensions(k))
              found = .true.
              exit with_pref
           end if
       end do find_ext
   end do with_pref   
   
   if (.not.found) then 
        prefix = ""
        suffix = ""
   end if
    
end subroutine lib_get_trailing

!> Initialize HDF5 metapackage for the current system
subroutine init_hdf5(this,compiler,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    character(*), parameter :: find_hl(*) = &
                 [character(11) :: '_hl_fortran','hl_fortran','_fortran','_hl']
    character(*), parameter :: candidates(*) = &
                 [character(15) :: 'hdf5_hl_fortran','hdf5-hl-fortran','hdf5_fortran','hdf5-fortran',&
                                   'hdf5_hl','hdf5','hdf5-serial']

    integer :: i,j,k,l
    logical :: s,found_hl(size(find_hl)),found
    type(string_t) :: log,this_lib
    type(string_t), allocatable :: libs(:),flags(:),modules(:),non_fortran(:)
    character(len=:), allocatable :: name,module_flag,include_flag,libdir,ext,pref

    module_flag  = get_module_flag(compiler,"")
    include_flag = get_include_flag(compiler,"")

    !> Cleanup
    call destroy(this)
    allocate(this%link_libs(0),this%incl_dirs(0),this%external_modules(0),non_fortran(0))
    this%link_flags = string_t("")
    this%flags = string_t("")
    
    !> Assert pkg-config is installed
    if (.not.assert_pkg_config()) then 
        call fatal_error(error,'hdf5 metapackage requires pkg-config')
        return
    end if
    
    !> Find pkg-config package file by priority
    name = 'NOT_FOUND'
    find_package: do i=1,size(candidates)
        if (pkgcfg_has_package(trim(candidates(i)))) then 
            name = trim(candidates(i))
            exit find_package
        end if
    end do find_package
    
    !> some distros put hdf5-1.2.3.pc with version number in .pc filename.
    if (name=='NOT_FOUND') then 
        modules = pkgcfg_list_all(error) 
        find_global_package: do i=1,size(modules)
            if (str_begins_with_str(modules(i)%s,'hdf5')) then 
                name = modules(i)%s
                exit find_global_package
            end if
        end do find_global_package
    end if
    
    if (name=='NOT_FOUND') then 
        call fatal_error(error,'pkg-config could not find a suitable hdf5 package.')
        return
    end if

    !> Get version
    log = pkgcfg_get_version(name,error)
    if (allocated(error)) return
    allocate(this%version)    
    call new_version(this%version,log%s,error)
    if (allocated(error)) return
    
    !> Get libraries
    libs = pkgcfg_get_libs(name,error)
    if (allocated(error)) return
    
    libdir = ""
    do i=1,size(libs)
        
        if (str_begins_with_str(libs(i)%s,'-l')) then 
            this%has_link_libraries = .true.
            this%link_libs = [this%link_libs, string_t(libs(i)%s(3:))]
            
        else ! -L and others: concatenate
            this%has_link_flags = .true.
            this%link_flags = string_t(trim(this%link_flags%s)//' '//libs(i)%s)

            ! Also save library dir
            if (str_begins_with_str(libs(i)%s,'-L')) then 
               libdir = libs(i)%s(3:)  
            elseif (str_begins_with_str(libs(i)%s,'/LIBPATH')) then
               libdir = libs(i)%s(9:)
            endif
            
        end if
    end do
    
    ! Some pkg-config hdf5.pc (e.g. Ubuntu) don't include the commonly-used HL HDF5 libraries,
    ! so let's add them if they exist
    if (len_trim(libdir)>0) then 
        do i=1,size(this%link_libs)
            
            found_hl = .false.
                    
            if (.not.str_ends_with(this%link_libs(i)%s, find_hl)) then 
                
               ! Extract name with no extension
               call lib_get_trailing(this%link_libs(i)%s, libdir, pref, ext, found)
               
               ! Search how many versions with the Fortran endings there are
               finals: do k=1,size(find_hl)
                  do j=1,size(this%link_libs)
                   if (str_begins_with_str(this%link_libs(j)%s,this%link_libs(i)%s) .and. &
                       str_ends_with(this%link_libs(j)%s,trim(find_hl(k)))) then 
                       found_hl(k) = .true.
                       cycle finals
                   end if
                  end do
               end do finals
               
               ! For each of the missing ones, if there is a file, add it
               add_missing: do k=1,size(find_hl)
                  if (found_hl(k)) cycle add_missing
                  
                  ! Build file name
                  this_lib%s = join_path(libdir,pref//this%link_libs(i)%s//trim(find_hl(k))//ext)                  
                  inquire(file=this_lib%s,exist=found)
                  
                  ! File exists, but it is not linked against
                  if (found) this%link_libs = [this%link_libs, &
                                               string_t(this%link_libs(i)%s//trim(find_hl(k)))]
                  
               end do add_missing
                
            end if

        end do
    endif
    
    !> Get compiler flags
    flags = pkgcfg_get_build_flags(name,.true.,error)
    if (allocated(error)) return

    do i=1,size(flags)
        
        if (str_begins_with_str(flags(i)%s,include_flag)) then 
            this%has_include_dirs = .true.
            this%incl_dirs = [this%incl_dirs, string_t(flags(i)%s(len(include_flag)+1:))]
        else
            this%has_build_flags = .true.
            this%flags = string_t(trim(this%flags%s)//' '//flags(i)%s)
        end if
        
    end do
    
    !> Add HDF5 modules as external 
    this%has_external_modules = .true.
    this%external_modules = [string_t('h5a'), &
                             string_t('h5d'), &
                             string_t('h5es'), &
                             string_t('h5e'), &
                             string_t('h5f'), &
                             string_t('h5g'), &
                             string_t('h5i'), &
                             string_t('h5l'), &
                             string_t('h5o'), &
                             string_t('h5p'), &
                             string_t('h5r'), &
                             string_t('h5s'), &
                             string_t('h5t'), &
                             string_t('h5vl'), &
                             string_t('h5z'), &
                             string_t('h5lib'), &
                             string_t('h5global'), &
                             string_t('h5_gen'), &
                             string_t('h5fortkit'), &
                             string_t('hdf5')]

end subroutine init_hdf5

end module fpm_meta
