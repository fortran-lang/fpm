!> Implementation of the meta data for compiler flag profiles.
!>
!> A profiles table can currently have the following subtables:
!> Profile names - any string, if omitted, flags are appended to all matching profiles
!> Compiler - any from the following list, if omitted, `DEFAULT_COMPILER` is used
!> - "gfortran"
!> - "ifort"
!> - "ifx"
!> - "pgfortran"
!> - "nvfrotran"
!> - "flang"
!> - "caf"
!> - "f95"
!> - "lfortran"
!> - "lfc"
!> - "nagfor"
!> - "crayftn"
!> - "xlf90"
!> - "ftn95"
!> OS - any from the following list, if omitted, the profile is used if and only
!> if there is no profile perfectly matching the current configuration
!> - "linux"
!> - "macos"
!> - "windows"
!> - "cygwin"
!> - "solaris"
!> - "freebsd"
!> - "openbsd"
!> - "unknown"
!> - "UNKNOWN"
!>
!> Each of the subtables currently supports the following fields:
!>```toml
!>[profile.debug.gfortran.linux]
!> flags="-Wall -g -Og"
!> c_flags="-g O1"
!> link_time_flags="-xlinkopt"
!> files={"hello_world.f90"="-Wall -O3"}
!>```
!>
module fpm_manifest_profile
    use fpm_error, only : error_t, syntax_error, fatal_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    use fpm_strings, only: lower
    use fpm_environment, only: get_os_type, OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
    use fpm_filesystem, only: join_path
    implicit none
    private
    public :: profile_config_t, new_profile, new_profiles, get_default_profiles, &
            & info_profile, find_profile, DEFAULT_COMPILER

    character(len=*), parameter :: DEFAULT_COMPILER = 'gfortran' 
    integer, parameter :: OS_ALL = -1
    character(len=:), allocatable :: path

    !> Type storing file name - file scope compiler flags pairs
    type :: file_scope_flag

      !> Name of the file
      character(len=:), allocatable :: file_name

      !> File scope flags
      character(len=:), allocatable :: flags
    end type file_scope_flag

    !> Configuration meta data for a profile
    type :: profile_config_t
      !> Name of the profile
      character(len=:), allocatable :: profile_name

      !> Name of the compiler
      character(len=:), allocatable :: compiler

      !> Value repesenting OS
      integer :: os_type
      
      !> Fortran compiler flags
      character(len=:), allocatable :: flags

      !> C compiler flags
      character(len=:), allocatable :: c_flags

      !> Link time compiler flags
      character(len=:), allocatable :: link_time_flags

      !> File scope flags
      type(file_scope_flag), allocatable :: file_scope_flags(:)

      contains

        !> Print information on this instance
        procedure :: info

    end type profile_config_t

    contains

      !> Construct a new profile configuration from a TOML data structure
      function new_profile(profile_name, compiler, os_type, flags, c_flags, link_time_flags, file_scope_flags) result(profile)
        
        !> Name of the profile
        character(len=*), intent(in) :: profile_name
        
        !> Name of the compiler
        character(len=*), intent(in) :: compiler
        
        !> Type of the OS
        integer, intent(in) :: os_type
        
        !> Fortran compiler flags
        character(len=*), optional, intent(in) :: flags

        !> C compiler flags
        character(len=*), optional, intent(in) :: c_flags

        !> Link time compiler flags
        character(len=*), optional, intent(in) :: link_time_flags

        !> File scope flags
        type(file_scope_flag), optional, intent(in) :: file_scope_flags(:)

        type(profile_config_t) :: profile

        profile%profile_name = profile_name
        profile%compiler = compiler
        profile%os_type = os_type
        if (present(flags)) then
          profile%flags = flags
        else
          profile%flags = ""
        end if
        if (present(c_flags)) then
          profile%c_flags = c_flags
        else
          profile%c_flags = ""
        end if
        if (present(link_time_flags)) then
          profile%link_time_flags = link_time_flags
        else
          profile%link_time_flags = ""
        end if
        if (present(file_scope_flags)) then
           profile%file_scope_flags = file_scope_flags
        end if
      end function new_profile

      !> Check if compiler name is a valid compiler name
      subroutine validate_compiler_name(compiler_name, is_valid)
        character(len=:), allocatable, intent(in) :: compiler_name
        logical, intent(out) :: is_valid
        select case(compiler_name)
          case("gfortran", "ifort", "ifx", "pgfortran", "nvfrotran", "flang", "caf", &
                        & "f95", "lfortran", "lfc", "nagfor", "crayftn", "xlf90", "ftn95")
            is_valid = .true.
          case default
            is_valid = .false.
        end select
      end subroutine validate_compiler_name
        
      subroutine validate_os_name(os_name, is_valid)
        character(len=:), allocatable, intent(in) :: os_name
        logical, intent(out) :: is_valid
        select case (os_name)
          case ("linux", "macos", "windows", "cygwin", "solaris", "freebsd", &
                          & "openbsd", "unknown", "UNKNOWN")
            is_valid = .true.
          case default
            is_valid = .false.
        end select
      end subroutine validate_os_name

      subroutine match_os_type(os_name, os_type)
        character(len=:), allocatable, intent(in) :: os_name
        integer, intent(out) :: os_type
        select case (os_name)
          case ("linux");   os_type = OS_LINUX
          case ("macos");   os_type = OS_WINDOWS
          case ("cygwin");  os_type = OS_CYGWIN
          case ("solaris"); os_type = OS_SOLARIS
          case ("freebsd"); os_type = OS_FREEBSD
          case ("openbsd"); os_type = OS_OPENBSD
          case ("all");     os_type = OS_ALL
          case default;     os_type = OS_UNKNOWN
        end select
      end subroutine match_os_type

      subroutine get_flags(profile_name, compiler_name, os_type, key_list, table, profiles, profindex, error)

        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> OS type
        integer, intent(in) :: os_type

        !> List of keys in the table
        type(toml_key), allocatable, intent(in) :: key_list(:)

        !> Table containing OS tables
        type(toml_table), pointer, intent(in) :: table

        !> List of profiles
        type(profile_config_t), allocatable, intent(inout) :: profiles(:)

        !> Index in the list of profiles
        integer, intent(inout) :: profindex

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: flags, c_flags, link_time_flags, key_name, file_name, file_flags
        type(toml_table), pointer :: files
        type(toml_key), allocatable :: file_list(:)
        type(file_scope_flag), allocatable :: file_scope_flags(:)
        integer :: ikey, ifile, stat
        logical :: is_valid

        if (size(key_list).ge.1) then
          do ikey=1,size(key_list)
            key_name = key_list(ikey)%key
            if (key_name.eq.'flags') then
              call get_value(table, 'flags', flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'c_flags') then
              call get_value(table, 'c_flags', c_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "c_flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'link_time_flags') then
              call get_value(table, 'link_time_flags', link_time_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "link_time_flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'files') then
              call get_value(table, 'files', files, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "files has to be a table")
                return
              end if
              call files%get_keys(file_list)
              allocate(file_scope_flags(size(file_list)))
              do ifile=1,size(file_list)
                file_name = file_list(ifile)%key
                call get_value(files, file_name, file_flags, stat=stat)
                if (stat /= toml_stat%success) then
                  call syntax_error(error, "file scope flags has to be a key-value pair")
                  return
                end if
                associate(cur_file=>file_scope_flags(ifile))
                  if (.not.(path.eq."")) file_name = join_path(path, file_name)
                  cur_file%file_name = file_name
                  cur_file%flags = file_flags
                end associate
              end do
            end if
          end do
        end if

        if (.not.allocated(flags)) flags=''
        if (.not.allocated(c_flags)) c_flags=''
        if (.not.allocated(link_time_flags)) link_time_flags=''

        if (allocated(file_scope_flags)) then
          profiles(profindex) = new_profile(profile_name, compiler_name, os_type, &
                 & flags, c_flags, link_time_flags, file_scope_flags)
        else
          profiles(profindex) = new_profile(profile_name, compiler_name, os_type, flags, c_flags, link_time_flags)
        end if
        profindex = profindex + 1
      end subroutine get_flags

      !> Traverse operating system tables
      subroutine traverse_oss(profile_name, compiler_name, os_list, table, error, profiles_size, profiles, profindex)
        
        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> List of OSs in table with profile name and compiler name given
        type(toml_key), allocatable, intent(in) :: os_list(:)

        !> Table containing OS tables
        type(toml_table), pointer, intent(in) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Number of profiles in list of profiles
        integer, intent(inout), optional :: profiles_size

        !> List of profiles
        type(profile_config_t), allocatable, intent(inout), optional :: profiles(:)

        !> Index in the list of profiles
        integer, intent(inout), optional :: profindex
        
        type(toml_key), allocatable :: key_list(:)
        character(len=:), allocatable :: os_name
        type(toml_table), pointer :: os_node
        character(len=:), allocatable :: flags
        integer :: ios, stat, os_type
        logical :: is_valid, key_val_added, is_key_val

        if (size(os_list)<1) return
        key_val_added = .false.
        do ios = 1, size(os_list)
          os_name = lower(os_list(ios)%key)
          call validate_os_name(os_name, is_valid)
          if (is_valid) then
            if (present(profiles_size)) then
              profiles_size = profiles_size + 1
            else
              if (.not.(present(profiles).and.present(profindex))) then
                call fatal_error(error, "Both profiles and profindex have to be present")
                return
              end if
              os_name = os_list(ios)%key
              call get_value(table, os_name, os_node, stat=stat)
              os_name = lower(os_name)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "os "//os_name//" has to be a table")
                return
              end if
              call match_os_type(os_name, os_type)
              call os_node%get_keys(key_list)
              call get_flags(profile_name, compiler_name, os_type, key_list, os_node, profiles, profindex, error)
              if (allocated(error)) return
            end if
          else
            is_key_val = .false.
            os_name = os_list(ios)%key
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              is_key_val = .true.
            end if
            if (present(profiles_size)) then
              if (is_key_val.and..not.key_val_added) then
                key_val_added = .true.
                is_key_val = .false.
                profiles_size = profiles_size + 1
              else if (.not.is_key_val) then
                profiles_size = profiles_size + 1
              end if
            else
              if (.not.(present(profiles).and.present(profindex))) then
                call fatal_error(error, "Both profiles and profindex have to be present")
                return
              end if
              os_type = OS_ALL
              os_node=>table
              call get_flags(profile_name, compiler_name, os_type, os_list, os_node, profiles, profindex, error)
              if (allocated(error)) return
            end if
          end if
        end do
      end subroutine traverse_oss

      !> Traverse compiler tables
      subroutine traverse_compilers(profile_name, comp_list, table, error, profiles_size, profiles, profindex)
        
        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> List of OSs in table with profile name given
        type(toml_key), allocatable, intent(in) :: comp_list(:)

        !> Table containing compiler tables
        type(toml_table), pointer, intent(in) :: table
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        !> Number of profiles in list of profiles
        integer, intent(inout), optional :: profiles_size

        !> List of profiles
        type(profile_config_t), allocatable, intent(inout), optional :: profiles(:)

        !> Index in the list of profiles
        integer, intent(inout), optional :: profindex
        
        character(len=:), allocatable :: compiler_name        
        type(toml_table), pointer :: comp_node
        type(toml_key), allocatable :: os_list(:)
        integer :: icomp, stat
        logical :: is_valid

        if (size(comp_list)<1) return
        do icomp = 1, size(comp_list)
          call validate_compiler_name(comp_list(icomp)%key, is_valid)
          if (is_valid) then  
            compiler_name = comp_list(icomp)%key
            call get_value(table, compiler_name, comp_node, stat=stat)
            if (stat /= toml_stat%success) then
              call syntax_error(error, "Compiler "//comp_list(icomp)%key//" must be a table entry")
              exit
            end if
            call comp_node%get_keys(os_list)
            if (present(profiles_size)) then
              call traverse_oss(profile_name, compiler_name, os_list, comp_node, error, profiles_size=profiles_size)
              if (allocated(error)) return
            else
              if (.not.(present(profiles).and.present(profindex))) then
                call fatal_error(error, "Both profiles and profindex have to be present")
                return
              end if
              call traverse_oss(profile_name, compiler_name, os_list, comp_node, &
                                & error, profiles=profiles, profindex=profindex)
              if (allocated(error)) return
            end if
          else
            os_list = comp_list(icomp:icomp)
            compiler_name = DEFAULT_COMPILER

            if (present(profiles_size)) then
              call traverse_oss(profile_name, compiler_name, os_list, table, error, profiles_size=profiles_size)
              if (allocated(error)) return
            else
              if (.not.(present(profiles).and.present(profindex))) then
                call fatal_error(error, "Both profiles and profindex have to be present")
                return
              end if
              call traverse_oss(profile_name, compiler_name, os_list, table, &
                                & error, profiles=profiles, profindex=profindex)
              if (allocated(error)) return
            end if
          end if
        end do        
      end subroutine traverse_compilers

      !> Construct new profiles array from a TOML data structure
      subroutine new_profiles(profiles, table, error, file_scope_path)

        !> Instance of the dependency configuration
        type(profile_config_t), allocatable, intent(out) :: profiles(:)

        !> Instance of the TOML data structure
        type(toml_table), target, intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Path to project directory of the current package
        character(len=*), intent(in), optional :: file_scope_path

        type(toml_table), pointer :: prof_node
        type(toml_key), allocatable :: prof_list(:)
        type(toml_key), allocatable :: comp_list(:)
        type(toml_key), allocatable :: os_list(:)
        character(len=:), allocatable :: profile_name, compiler_name
        integer :: profiles_size, iprof, stat, profindex
        logical :: is_valid
        type(profile_config_t), allocatable :: default_profiles(:)

        if (present(file_scope_path)) then
          path = file_scope_path
        else
          path = ''
        end if
        default_profiles = get_default_profiles(error)
        if (allocated(error)) return
        call table%get_keys(prof_list)
        
        if (size(prof_list) < 1) return
        
        profiles_size = 0

        do iprof = 1, size(prof_list)
          profile_name = prof_list(iprof)%key
          call validate_compiler_name(profile_name, is_valid)
          if (is_valid) then
            profile_name = "all"
            comp_list = prof_list(iprof:iprof)
            prof_node=>table
            call traverse_compilers(profile_name, comp_list, prof_node, error, profiles_size=profiles_size)
            if (allocated(error)) return
          else
            call validate_os_name(profile_name, is_valid)
            if (is_valid) then
              os_list = prof_list(iprof:iprof)
              profile_name = 'all'
              compiler_name = DEFAULT_COMPILER
              call traverse_oss(profile_name, compiler_name, os_list, table, error, profiles_size=profiles_size)
              if (allocated(error)) return
            else
              call get_value(table, profile_name, prof_node, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "Profile "//prof_list(iprof)%key//" must be a table entry")
                exit
              end if
              call prof_node%get_keys(comp_list)
              call traverse_compilers(profile_name, comp_list, prof_node, error, profiles_size=profiles_size)
              if (allocated(error)) return
            end if
          end if
        end do

        profiles_size=profiles_size+size(default_profiles)
        allocate(profiles(profiles_size))
        
        do profindex=1, size(default_profiles)
          profiles(profindex) = default_profiles(profindex)
        end do

        do iprof = 1, size(prof_list)
          profile_name = prof_list(iprof)%key
          call validate_compiler_name(profile_name, is_valid)
          if (is_valid) then
            profile_name = "all"
            comp_list = prof_list(iprof:iprof)
            prof_node=>table
            call traverse_compilers(profile_name, comp_list, prof_node, error, profiles=profiles, profindex=profindex)
            if (allocated(error)) return
          else
            call validate_os_name(profile_name, is_valid)
            if (is_valid) then
              os_list = prof_list(iprof:iprof)
              profile_name = 'all'
              compiler_name = DEFAULT_COMPILER
              prof_node=>table
              call traverse_oss(profile_name, compiler_name, os_list, prof_node, error, profiles=profiles, profindex=profindex)
              if (allocated(error)) return
            else
              call get_value(table, profile_name, prof_node, stat=stat)
              call prof_node%get_keys(comp_list)
              call traverse_compilers(profile_name, comp_list, prof_node, error, profiles=profiles, profindex=profindex)
              if (allocated(error)) return
            end if
          end if
        end do

        ! Apply profiles with profile name 'all' to matching profiles
        do iprof = 1,size(profiles)
          if (profiles(iprof)%profile_name.eq.'all') then
            do profindex = 1,size(profiles)
              if (.not.(profiles(profindex)%profile_name.eq.'all') &
                      & .and.(profiles(profindex)%compiler.eq.profiles(iprof)%compiler) &
                      & .and.(profiles(profindex)%os_type.eq.profiles(iprof)%os_type)) then
                profiles(profindex)%flags=profiles(profindex)%flags// &
                        & " "//profiles(iprof)%flags
                profiles(profindex)%c_flags=profiles(profindex)%c_flags// &
                        & " "//profiles(iprof)%c_flags
                profiles(profindex)%link_time_flags=profiles(profindex)%link_time_flags// &
                        & " "//profiles(iprof)%link_time_flags
              end if
            end do
          end if
        end do
      end subroutine new_profiles

      function get_default_profiles(error) result(default_profiles)
        type(error_t), allocatable, intent(out) :: error
        type(profile_config_t), allocatable :: default_profiles(:)
        default_profiles = [ &
              & new_profile('release', 'caf', OS_ALL, flags=' -O3 -Wimplicit-interface&
                                                & -fPIC -fmax-errors=1 -funroll-loops'), &
              & new_profile('release', 'gfortran', OS_ALL, flags=' -O3 -Wimplicit-interface -fPIC&
                                                & -fmax-errors=1 -funroll-loops -fcoarray=single'), &
              & new_profile('release', 'f95', OS_ALL, flags=' -O3 -Wimplicit-interface -fPIC&
                                                & -fmax-errors=1 -ffast-math -funroll-loops'), &
              & new_profile('release', 'nvfortran', OS_ALL, flags = ' -Mbackslash'), &
              & new_profile('release', 'ifort', OS_ALL, flags = ' -fp-model precise -pc64 -align all&
                                                & -error-limit 1 -reentrancy threaded&
                                                & -nogen-interfaces -assume byterecl'), &
              & new_profile('release', 'ifort', OS_WINDOWS, flags = ' /fp:precise /align:all&
                                                & /error-limit:1 /reentrancy:threaded&
                                                & /nogen-interfaces /assume:byterecl'), &
              & new_profile('release', 'ifx', OS_ALL, flags = ' -fp-model=precise -pc64&
                                                & -align all -error-limit 1 -reentrancy threaded&
                                                & -nogen-interfaces -assume byterecl'), &
              & new_profile('release', 'ifx', OS_WINDOWS, flags = ' /fp:precise /align:all&
                                                & /error-limit:1 /reentrancy:threaded&
                                                & /nogen-interfaces /assume:byterecl'), &
              & new_profile('release', 'nagfor', OS_ALL, flags = ' -O4 -coarray=single -PIC'), &
              & new_profile('debug', 'caf', OS_ALL, flags = ' -Wall -Wextra -Wimplicit-interface&
                                                & -fPIC -fmax-errors=1  -g -fcheck=bounds&
                                                & -fcheck=array-temps -fbacktrace'), &
              & new_profile('debug', 'gfortran', OS_ALL, flags = ' -Wall -Wextra -Wimplicit-interface&
                                                & -fPIC -fmax-errors=1 -g -fcheck=bounds&
                                                & -fcheck=array-temps -fbacktrace -fcoarray=single'), &
              & new_profile('debug', 'f95', OS_ALL, flags = ' -Wall -Wextra -Wimplicit-interface&
                                                & -fPIC -fmax-errors=1 -g -fcheck=bounds&
                                                & -fcheck=array-temps -Wno-maybe-uninitialized&
                                                & -Wno-uninitialized -fbacktrace'), &
              & new_profile('debug', 'nvfortran', OS_ALL, flags = ' -Minform=inform -Mbackslash -g&
                                                & -Mbounds -Mchkptr -Mchkstk -traceback'), &
              & new_profile('debug', 'ifort', OS_ALL, flags = ' -warn all -check all -error-limit 1&
                                                & -O0 -g -assume byterecl -traceback'), &
              & new_profile('debug', 'ifort', OS_WINDOWS, flags = ' /warn:all /check:all /error-limit:1&
                                                & /Od /Z7 /assume:byterecl /traceback'), &
              & new_profile('debug', 'ifx', OS_ALL, flags = ' -warn all -check all -error-limit 1&
                                                & -O0 -g -assume byterecl -traceback'), &
              & new_profile('debug', 'ifx', OS_WINDOWS, flags = ' /warn:all /check:all /error-limit:1&
                                                & /Od /Z7 /assume:byterecl'), &
              & new_profile('debug', 'nagfor', OS_ALL, flags = ' -g -C=all -O0 -gline -coarray=single -PIC') &
              &]
      end function get_default_profiles

      !> Write information on instance
      subroutine info(self, unit, verbosity)

        !> Instance of the profile configuration
        class(profile_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        write(unit, fmt) "Profile"
        if (allocated(self%profile_name)) then
            write(unit, fmt) "- profile name", self%profile_name
        end if

        if (allocated(self%compiler)) then
            write(unit, fmt) "- compiler", self%compiler
        end if

        write(unit, fmt) "- os", self%os_type

        if (allocated(self%flags)) then
            write(unit, fmt) "- compiler flags", self%flags
        end if

      end subroutine info

      function info_profile(profile) result(s)
        ! Prints a representation of profile_config_t
        type(profile_config_t), intent(in) :: profile
        character(:), allocatable :: s
        integer :: i
        s = "profile_config_t("
        s = s // 'profile_name="' // profile%profile_name // '"'
        s = s // ', compiler="' // profile%compiler // '"'
        s = s // ", os_type="
        select case(profile%os_type)
        case (OS_UNKNOWN)
          s = s // "OS_UNKNOWN"
        case (OS_LINUX)
          s = s // "OS_LINUX"
        case (OS_MACOS)
          s = s // "OS_MACOS"
        case (OS_WINDOWS)
          s = s // "OS_WINDOWS"
        case (OS_CYGWIN)
          s = s // "OS_CYGWIN"
        case (OS_SOLARIS)
          s = s // "OS_SOLARIS"
        case (OS_FREEBSD)
          s = s // "OS_FREEBSD"
        case (OS_OPENBSD)
          s = s // "OS_OPENBSD"
        case (OS_ALL)
          s = s // "OS_ALL"
        case default
          s = s // "INVALID"
        end select
        if (allocated(profile%flags)) s = s // ', flags="' // profile%flags // '"'
        if (allocated(profile%c_flags)) s = s // ', c_flags="' // profile%c_flags // '"'
        if (allocated(profile%link_time_flags)) s = s // ', link_time_flags="' // profile%link_time_flags // '"'
        if (allocated(profile%file_scope_flags)) then
          do i=1,size(profile%file_scope_flags)
            s = s // ', flags for '//profile%file_scope_flags(i)%file_name// &
                    & ' ="' // profile%file_scope_flags(i)%flags // '"'
          end do
        end if
        s = s // ")"
      end function info_profile

      subroutine find_profile(profiles, profile_name, compiler, os_type, found_matching, chosen_profile)
        type(profile_config_t), allocatable, intent(in) :: profiles(:)
        character(:), allocatable, intent(in) :: profile_name
        character(:), allocatable, intent(in) :: compiler
        integer, intent(in) :: os_type
        logical, intent(out) :: found_matching
        type(profile_config_t), intent(out) :: chosen_profile
        character(:), allocatable :: curr_profile_name
        character(:), allocatable :: curr_compiler
        integer :: curr_os
        integer :: i, priority, curr_priority

        found_matching = .false.
        do i=1,size(profiles)
          curr_profile_name = profiles(i)%profile_name
          curr_compiler = profiles(i)%compiler
          curr_os = profiles(i)%os_type
          if (curr_profile_name.eq.profile_name) then
            if (curr_compiler.eq.compiler) then
              if (curr_os.eq.os_type) then
                chosen_profile = profiles(i)
                found_matching = .true.
              end if
            end if
          end if
        end do
        if (.not. found_matching) then
          do i=1,size(profiles)
            curr_profile_name = profiles(i)%profile_name
            curr_compiler = profiles(i)%compiler
            curr_os = profiles(i)%os_type
            if (curr_profile_name.eq.profile_name) then
              if (curr_compiler.eq.compiler) then
                if (curr_os.eq.OS_ALL) then
                  chosen_profile = profiles(i)
                  found_matching = .true.
                end if
              end if
            end if
          end do
        end if
      end subroutine find_profile
end module fpm_manifest_profile
