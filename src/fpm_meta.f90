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
!>
!> @note Core libraries are enabled in the [build] section of the fpm.toml manifest
!>
!>
module fpm_meta
use fpm_strings, only: string_t, len_trim
use fpm_error, only: error_t, fatal_error, syntax_error, fpm_stop
use fpm_compiler
use fpm_model
use fpm_manifest_dependency, only: dependency_config_t
use fpm_git, only : git_target_branch
use fpm_manifest, only: package_config_t
use fpm_environment, only: get_env,os_is_unix
use fpm_filesystem, only: run, get_temp_filename, getline
use iso_fortran_env, only: stdout => output_unit

implicit none

private

public :: resolve_metapackages

!> Type for describing a source file
type, public :: metapackage_t

    logical :: has_link_libraries  = .false.
    logical :: has_link_flags      = .false.
    logical :: has_build_flags     = .false.
    logical :: has_include_dirs    = .false.
    logical :: has_dependencies    = .false.

    !> List of compiler flags and options to be added
    type(string_t) :: flags
    type(string_t) :: link_flags
    type(string_t), allocatable :: incl_dirs(:)
    type(string_t), allocatable :: link_libs(:)

    !> List of Development dependency meta data.
    !> Metapackage dependencies are never exported from the model
    type(dependency_config_t), allocatable :: dependency(:)

    contains

       !> Clean metapackage structure
       procedure :: destroy

       !> Initialize the metapackage structure from its given name
       procedure :: new => init_from_name

       !> Add metapackage dependencies to the model
       procedure, private :: resolve_model
       procedure, private :: resolve_package_config
       generic :: resolve => resolve_model,resolve_package_config

end type metapackage_t

interface resolve_metapackages
    module procedure resolve_metapackage_model
end interface resolve_metapackages

integer, parameter :: MPI_TYPE_NONE    = 0
integer, parameter :: MPI_TYPE_OPENMPI = 1
integer, parameter :: MPI_TYPE_MPICH   = 2
integer, parameter :: MPI_TYPE_INTEL   = 3
integer, parameter :: MPI_TYPE_MSMPI   = 4

contains

!> Clean the metapackage structure
elemental subroutine destroy(this)
   class(metapackage_t), intent(inout) :: this

   this%has_link_libraries  = .false.
   this%has_link_flags      = .false.
   this%has_build_flags     = .false.
   this%has_include_dirs    = .false.
   this%has_dependencies    = .false.

   if (allocated(this%flags%s)) deallocate(this%flags%s)
   if (allocated(this%link_flags%s)) deallocate(this%link_flags%s)
   if (allocated(this%link_libs)) deallocate(this%link_libs)
   if (allocated(this%dependency)) deallocate(this%dependency)
   if (allocated(this%incl_dirs)) deallocate(this%incl_dirs)

end subroutine destroy

!> Initialize a metapackage from the given name
subroutine init_from_name(this,name,compiler,error)
    class(metapackage_t), intent(inout) :: this
    character(*), intent(in) :: name
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    !> Initialize metapackage by name
    select case(name)
        case("openmp"); call init_openmp(this,compiler,error)
        case("stdlib"); call init_stdlib(this,compiler,error)
        case("mpi");    call init_mpi   (this,compiler,error)
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

! Resolve metapackage dependencies into the model
subroutine resolve_model(self,model,error)
    class(metapackage_t), intent(in) :: self
    type(fpm_model_t), intent(inout) :: model
    type(error_t), allocatable, intent(out) :: error

    ! For now, additional flags are assumed to apply to all sources
    if (self%has_build_flags) then
        model%fortran_compile_flags = model%fortran_compile_flags//self%flags%s
        model%c_compile_flags       = model%c_compile_flags//self%flags%s
        model%cxx_compile_flags     = model%cxx_compile_flags//self%flags%s
    endif

    if (self%has_link_flags) then
        model%link_flags            = model%link_flags//self%link_flags%s
    end if

    if (self%has_link_libraries) then
        model%link_libraries        = [model%link_libraries,self%link_libs]
    end if

    if (self%has_include_dirs) then
        model%include_dirs          = [model%include_dirs,self%incl_dirs]
    end if

    ! Dependencies are resolved in the package config

end subroutine resolve_model

subroutine resolve_package_config(self,package,error)
    class(metapackage_t), intent(in) :: self
    type(package_config_t), intent(inout) :: package
    type(error_t), allocatable, intent(out) :: error

    ! All metapackage dependencies are added as full dependencies,
    ! as upstream projects will not otherwise compile without them
    if (self%has_dependencies) then
        if (allocated(package%dependency)) then
           package%dependency = [package%dependency,self%dependency]
        else
           package%dependency = self%dependency
        end if
    end if

end subroutine resolve_package_config

! Add named metapackage dependency to the model
subroutine add_metapackage_model(model,name,error)
    type(fpm_model_t), intent(inout) :: model
    character(*), intent(in) :: name
    type(error_t), allocatable, intent(out) :: error

    type(metapackage_t) :: meta

    !> Init metapackage
    call meta%new(name,model%compiler,error)
    if (allocated(error)) return

    !> Add it to the model
    call meta%resolve(model,error)
    if (allocated(error)) return

end subroutine add_metapackage_model

! Add named metapackage dependency to the model
subroutine add_metapackage_config(package,compiler,name,error)
    type(package_config_t), intent(inout) :: package
    type(compiler_t), intent(in) :: compiler
    character(*), intent(in) :: name
    type(error_t), allocatable, intent(out) :: error

    type(metapackage_t) :: meta

    !> Init metapackage
    call meta%new(name,compiler,error)
    if (allocated(error)) return

    !> Add it to the model
    call meta%resolve(package,error)
    if (allocated(error)) return

end subroutine add_metapackage_config

!> Resolve all metapackages into the package config
subroutine resolve_metapackage_model(model,package,error)
    type(fpm_model_t), intent(inout) :: model
    type(package_config_t), intent(inout) :: package
    type(error_t), allocatable, intent(out) :: error

    ! Dependencies are added to the package config, so they're properly resolved
    ! into the dependency tree later.
    ! Flags are added to the model (whose compiler needs to be already initialized)
    if (model%compiler%is_unknown()) then
        call fatal_error(error,"compiler not initialized: cannot build metapackages")
        return
    end if

    ! OpenMP
    if (package%meta%openmp) then
        call add_metapackage_model(model,"openmp",error)
        if (allocated(error)) return
        call add_metapackage_config(package,model%compiler,"openmp",error)
        if (allocated(error)) return
    endif

    ! stdlib
    if (package%meta%stdlib) then
        call add_metapackage_model(model,"stdlib",error)
        if (allocated(error)) return
        call add_metapackage_config(package,model%compiler,"stdlib",error)
        if (allocated(error)) return
    endif

    ! Stdlib is not 100% thread safe. print a warning to the user
    if (package%meta%stdlib .and. package%meta%openmp) then
        write(stdout,'(a)')'<WARNING> both openmp and stdlib requested: some functions may not be thread-safe!'
    end if

    ! MPI
    if (package%meta%mpi) then

        print *, 'resolving MPI...'

        call add_metapackage_model(model,"mpi",error)
        if (allocated(error)) return
        call add_metapackage_config(package,model%compiler,"mpi",error)
        if (allocated(error)) return
    endif

end subroutine resolve_metapackage_model

!> Initialize MPI metapackage for the current system
subroutine init_mpi(this,compiler,error)
    class(metapackage_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    logical, parameter :: verbose = .true.
    type(string_t), allocatable :: c_wrappers(:),cpp_wrappers(:),fort_wrappers(:)
    type(string_t) :: output
    character(256) :: msg_out
    character(len=:), allocatable :: tokens(:)
    integer :: ifort,ic,icpp,i

    !> Cleanup
    call destroy(this)

    !> Get all candidate MPI wrappers
    call mpi_wrappers(compiler,fort_wrappers,c_wrappers,cpp_wrappers)
    if (verbose) print 1, size(fort_wrappers),size(c_wrappers),size(cpp_wrappers)

    if (size(fort_wrappers)*size(c_wrappers)*size(cpp_wrappers)<=0) then
        call fatal_error(error,"cannot find MPI wrappers for "//compiler%name()//" compiler")
        return
    end if

    !> Return an MPI wrapper that matches the current compiler
    ifort = mpi_compiler_match(fort_wrappers,compiler,error)
    if (allocated(error)) return

    !C, C++ not available yet
    !ic    = mpi_compiler_match(c_wrappers,compiler,error)
    !icpp  = mpi_compiler_match(cpp_wrappers,compiler,error)

    !> Build MPI dependency
    if (ifort>0) then

         ! Get linking flags
         this%link_flags = mpi_wrapper_query(fort_wrappers(ifort),'link',verbose,error)
         if (allocated(error)) return
         this%has_link_flags = len_trim(this%link_flags)>0

         ! Add heading space
         this%link_flags = string_t(' '//this%link_flags%s)

         ! Get build flags
         this%flags = mpi_wrapper_query(fort_wrappers(ifort),'flags',verbose,error)
         if (allocated(error)) return
         this%has_build_flags = len_trim(this%flags)>0

         ! Add heading space
         this%flags = string_t(' '//this%flags%s)

    else

         ! None of the available wrappers matched the current Fortran compiler
         write(msg_out,1) size(fort_wrappers),compiler%fc
         call fatal_error(error,trim(msg_out))
         return

    endif


    1 format('MPI wrappers found: fortran=',i0,' c=',i0,' c++=',i0)
    2 format('<ERROR> None out of ',i0,' valid MPI wrappers matches compiler ',a)

end subroutine init_mpi

!> Match one of the available compiler wrappers with the current compiler
integer function mpi_compiler_match(wrappers,compiler,error)
    type(string_t), intent(in) :: wrappers(:)
    type(compiler_t), intent(in) :: compiler
    type(error_t), allocatable, intent(out) :: error

    integer :: i
    type(string_t) :: screen
    character(128) :: msg_out
    type(compiler_t) :: mpi_compiler

    mpi_compiler_match = 0

    do i=1,size(wrappers)

        screen = mpi_wrapper_query(wrappers(i),'compiler',.false.,error)
        if (allocated(error)) return

        ! Build compiler type
        call new_compiler(mpi_compiler,screen%s,'','',echo=.true.,verbose=.true.)

        ! Match found!
        if (mpi_compiler%id == compiler%id) then

            mpi_compiler_match = i
            return

        end if

    end do

    ! None of the available wrappers matched the current Fortran compiler
    write(msg_out,1) size(wrappers),compiler%fc
    call fatal_error(error,trim(msg_out))
    1 format('<ERROR> None out of ',i0,' valid MPI wrappers matches compiler ',a)

end function mpi_compiler_match

!> Return several mpi wrappers, and return
subroutine mpi_wrappers(compiler,fort_wrappers,c_wrappers,cpp_wrappers)
    type(compiler_t), intent(in) :: compiler
    type(string_t), allocatable, intent(out) :: c_wrappers(:),cpp_wrappers(:),fort_wrappers(:)

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

       case (id_intel_classic_windows,id_intel_llvm_windows,&
             id_intel_classic_nix,id_intel_classic_mac,id_intel_llvm_nix,id_intel_llvm_unknown)

            c_wrappers = [c_wrappers,string_t(get_env('I_MPI_CC','mpiicc')),string_t('mpicl.bat')]
          cpp_wrappers = [cpp_wrappers,string_t(get_env('I_MPI_CXX','mpiicpc')),string_t('mpicl.bat')]
         fort_wrappers = [fort_wrappers,string_t(get_env('I_MPI_F90','mpiifort')),string_t('mpif77'),&
                          string_t('mpif90')]

       case (id_pgi,id_nvhpc)

            c_wrappers = [c_wrappers,string_t('mpipgicc'),string_t('mpgcc')]
          cpp_wrappers = [cpp_wrappers,string_t('mpipgic++')]
         fort_wrappers = [fort_wrappers,string_t('mpipgifort'),string_t('mpipgf90')]

       case (id_cray)

            c_wrappers = [c_wrappers,string_t('cc')]
          cpp_wrappers = [cpp_wrappers,string_t('CC')]
         fort_wrappers = [fort_wrappers,string_t('ftn')]

    end select compiler_specific

    call assert_mpi_wrappers(fort_wrappers)
    call assert_mpi_wrappers(c_wrappers)
    call assert_mpi_wrappers(cpp_wrappers)

end subroutine mpi_wrappers

!> Filter out invalid/unavailable mpi wrappers
subroutine assert_mpi_wrappers(wrappers,verbose)
    type(string_t), allocatable, intent(inout) :: wrappers(:)
    logical, optional, intent(in) :: verbose

    integer :: i
    integer, allocatable :: works(:)

    allocate(works(size(wrappers)))

    do i=1,size(wrappers)
        works(i) = which_mpi_library(wrappers(i),verbose)
    end do

    ! Filter out non-working wrappers
    wrappers = pack(wrappers,works/=MPI_TYPE_NONE)

end subroutine assert_mpi_wrappers

!> Simple call to execute_command_line involving one mpi* wrapper
subroutine run_mpi_wrapper(wrapper,args,verbose,exitcode,cmd_success,screen_output)
    type(string_t), intent(in) :: wrapper
    type(string_t), intent(in), optional :: args(:)
    logical, intent(in), optional :: verbose
    integer, intent(out), optional :: exitcode
    logical, intent(out), optional :: cmd_success
    type(string_t), intent(out), optional :: screen_output

    logical :: echo_local
    character(:), allocatable :: redirect_str,command,redirect,line
    integer :: iunit,iarg,stat,cmdstat


    if(present(verbose))then
       echo_local=verbose
    else
       echo_local=.true.
    end if

    ! No redirection and non-verbose output
    if (present(screen_output)) then
        redirect = get_temp_filename()
        redirect_str =  ">"//redirect//" 2>&1"
    else
        if (os_is_unix()) then
            redirect_str = " >/dev/null 2>&1"
        else
            redirect_str = " >NUL 2>&1"
        end if
    end if

    ! Init command
    command = wrapper%s

    add_arguments: if (present(args)) then
        do iarg=1,size(args)
            if (len_trim(args(iarg))<=0) cycle
            command = trim(command)//' '//args(iarg)%s
        end do
    endif add_arguments


    if (echo_local) print *, '+ ', command

    ! Test command
    call execute_command_line(command//redirect_str,exitstat=stat,cmdstat=cmdstat)

    ! Command successful?
    if (present(cmd_success)) cmd_success = cmdstat==0

    ! Program exit code?
    if (present(exitcode)) exitcode = stat

    ! Want screen output?
    if (present(screen_output) .and. cmdstat==0) then

        allocate(character(len=0) :: screen_output%s)

        open(newunit=iunit,file=redirect,status='old',iostat=stat)
        if (stat == 0)then
           do
               call getline(iunit, line, stat)
               if (stat /= 0) exit

               screen_output%s = screen_output%s//new_line('a')//line

               write(*,'(A)') trim(line)
           end do

           ! Close and delete file
           close(iunit,status='delete')

        else
           call fpm_stop(1,'cannot read temporary file from successful MPI wrapper')
        endif

    end if

end subroutine run_mpi_wrapper

!> Get MPI library type from the wrapper command. Currently, only OpenMPI is supported
integer function which_mpi_library(wrapper,verbose)
    type(string_t), intent(in) :: wrapper
    logical, intent(in), optional :: verbose

    logical :: is_mpi_wrapper
    integer :: stat

    ! Run mpi wrapper first
    call run_mpi_wrapper(wrapper,verbose=verbose,cmd_success=is_mpi_wrapper)

    if (is_mpi_wrapper) then

        ! Attempt to decipher which library this wrapper comes from.

        ! OpenMPI responds to '--showme' calls
        call run_mpi_wrapper(wrapper,[string_t('--showme')],verbose,&
                             exitcode=stat,cmd_success=is_mpi_wrapper)

        if (stat==0 .and. is_mpi_wrapper) then

            which_mpi_library = MPI_TYPE_OPENMPI

        else

            ! This MPI wrapper is of a currently unsupported library
            which_mpi_library = MPI_TYPE_NONE

        end if

    else

        which_mpi_library = MPI_TYPE_NONE

    end if

end function which_mpi_library

!> Test if an MPI wrapper works
type(string_t) function mpi_wrapper_query(wrapper,command,verbose,error) result(screen)
    type(string_t), intent(in) :: wrapper
    character(*), intent(in) :: command
    logical, intent(in), optional :: verbose
    type(error_t), allocatable, intent(out) :: error

    logical :: success
    character(:), allocatable :: redirect_str
    integer :: stat,cmdstat,mpi

    ! Get mpi type
    mpi = which_mpi_library(wrapper,verbose)

    select case (command)

       ! Get MPI compiler name
       case ('compiler')

           select case (mpi)
              case (MPI_TYPE_OPENMPI)

                 ! --showme:command returns the build command of this wrapper
                 call run_mpi_wrapper(wrapper,[string_t('--showme:command')],verbose=.true., &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local OpenMPI library does not support --showme:command')
                    return
                 end if

              case default

                 call fatal_error(error,'the MPI library of wrapper '//wrapper%s//' is not currently supported')
                 return

           end select


       ! Get a list of additional compiler flags
       case ('flags')

           select case (mpi)
              case (MPI_TYPE_OPENMPI)

                 ! --showme:command returns the build command of this wrapper
                 call run_mpi_wrapper(wrapper,[string_t('--showme:compile')],verbose=.true., &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local OpenMPI library does not support --showme:compile')
                    return
                 end if

                 call remove_new_lines(screen)

              case default

                 call fatal_error(error,'the MPI library of wrapper '//wrapper%s//' is not currently supported')
                 return

           end select

       ! Get a list of additional linker flags
       case ('link')

           select case (mpi)
              case (MPI_TYPE_OPENMPI)

                 ! --showme:command returns the build command of this wrapper
                 call run_mpi_wrapper(wrapper,[string_t('--showme:link')],verbose=.true., &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local OpenMPI library does not support --showme:link')
                    return
                 end if

                 call remove_new_lines(screen)

              case default

                 call fatal_error(error,'the MPI library of wrapper '//wrapper%s//' is not currently supported')
                 return

           end select

       ! Get a list of MPI library directories
       case ('link_dirs')

           select case (mpi)
              case (MPI_TYPE_OPENMPI)

                 ! --showme:command returns the build command of this wrapper
                 call run_mpi_wrapper(wrapper,[string_t('--showme:libdirs')],verbose=.true., &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local OpenMPI library does not support --showme:libdirs')
                    return
                 end if

              case default

                 call fatal_error(error,'the MPI library of wrapper '//wrapper%s//' is not currently supported')
                 return

           end select

       ! Get a list of include directories for the MPI headers/modules
       case ('incl_dirs')

           select case (mpi)
              case (MPI_TYPE_OPENMPI)

                 ! --showme:command returns the build command of this wrapper
                 call run_mpi_wrapper(wrapper,[string_t('--showme:incdirs')],verbose=.true., &
                                      exitcode=stat,cmd_success=success,screen_output=screen)

                 if (stat/=0 .or. .not.success) then
                    call syntax_error(error,'local OpenMPI library does not support --showme:incdirs')
                    return
                 end if

              case default

                 call fatal_error(error,'the MPI library of wrapper '//wrapper%s//' is not currently supported')
                 return

           end select

       case default;
           call fatal_error(error,'an invalid MPI wrapper command ('//command//&
                                  ') was invoked for wrapper <'//wrapper%s//'>.')
           return
    end select


end function mpi_wrapper_query

! Remove all new line characters from the current string
subroutine remove_new_lines(string)
    type(string_t), intent(inout) :: string

    integer :: feed,length

    if (.not.allocated(string%s)) return


    length = len(string%s)
    feed   = scan(string%s,new_line('a'))

    do while (length>0 .and. feed>0)

        if (length==1) then
            string = string_t("")
        elseif (feed==1) then
            string%s = string%s(2:length)
        elseif (feed==length) then
            string%s = string%s(1:length-1)
        else
            string%s = string%s(1:feed-1)//string%s(feed+1:length)
        end if

        length = len(string%s)
        feed   = scan(string%s,new_line('a'))

    end do

end subroutine remove_new_lines

end module fpm_meta
