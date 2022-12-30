!> Implementation of the meta data for compiler flag profiles.
!>
!> A profiles table can currently have the following subtables:
!> Profile names - any string, if omitted, flags are appended to all matching profiles
!> Compiler - any from the following list, omitting it yields an error
!>
!> - "gfortran"
!> - "ifort"
!> - "ifx"
!> - "pgfortran"
!> - "nvfortran"
!> - "flang"
!> - "caf"
!> - "f95"
!> - "lfortran"
!> - "lfc"
!> - "nagfor"
!> - "crayftn"
!> - "xlf90"
!> - "ftn95"
!>
!> OS - any from the following list, if omitted, the profile is used if and only
!> if there is no profile perfectly matching the current configuration
!>
!> - "linux"
!> - "macos"
!> - "windows"
!> - "cygwin"
!> - "solaris"
!> - "freebsd"
!> - "openbsd"
!> - "unknown"
!>
!> Each of the subtables currently supports the following fields:
!>```toml
!>[profiles.debug.gfortran.linux]
!> flags="-Wall -g -Og"
!> c-flags="-g O1"
!> cxx-flags="-g O1"
!> link-time-flags="-xlinkopt"
!> files={"hello_world.f90"="-Wall -O3"}
!>```
!>
module fpm_manifest_profile
    use fpm_error, only : error_t, syntax_error, fatal_error, fpm_stop
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    use fpm_strings, only: lower
    use fpm_environment, only: get_os_type, OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
    use fpm_filesystem, only: join_path
    implicit none
    public :: profile_config_t, new_profile, new_profiles, get_default_profiles, &
            & info_profile, find_profile, DEFAULT_COMPILER

    !> Name of the default compiler
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

      !> C++ compiler flags
      character(len=:), allocatable :: cxx_flags

      !> Link time compiler flags
      character(len=:), allocatable :: link_time_flags

      !> File scope flags
      type(file_scope_flag), allocatable :: file_scope_flags(:)

      !> Is this profile one of the built-in ones?
      logical :: is_built_in

      contains

        !> Print information on this instance
        procedure :: info

    end type profile_config_t

    contains

      !> Construct a new profile configuration from a TOML data structure
      function new_profile(profile_name, compiler, os_type, flags, c_flags, cxx_flags, &
                           link_time_flags, file_scope_flags, is_built_in) &
                      & result(profile)
        
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

        !> C++ compiler flags
        character(len=*), optional, intent(in) :: cxx_flags

        !> Link time compiler flags
        character(len=*), optional, intent(in) :: link_time_flags

        !> File scope flags
        type(file_scope_flag), optional, intent(in) :: file_scope_flags(:)

        !> Is this profile one of the built-in ones?
        logical, optional, intent(in) :: is_built_in

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
        if (present(cxx_flags)) then
          profile%cxx_flags = cxx_flags
        else
          profile%cxx_flags = ""
        end if
        if (present(link_time_flags)) then
          profile%link_time_flags = link_time_flags
        else
          profile%link_time_flags = ""
        end if
        if (present(file_scope_flags)) then
           profile%file_scope_flags = file_scope_flags
        end if
        if (present(is_built_in)) then
           profile%is_built_in = is_built_in
        else
           profile%is_built_in = .false.
        end if

      end function new_profile

      !> Check if compiler name is a valid compiler name
      subroutine validate_compiler_name(compiler_name, is_valid)

        !> Name of a compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> Boolean value of whether compiler_name is valid or not
        logical, intent(out) :: is_valid
        select case(compiler_name)
          case("gfortran", "ifort", "ifx", "pgfortran", "nvfortran", "flang", "caf", &
                        & "f95", "lfortran", "lfc", "nagfor", "crayftn", "xlf90", "ftn95")
            is_valid = .true.
          case default
            is_valid = .false.
        end select
      end subroutine validate_compiler_name
        
      !> Check if os_name is a valid name of a supported OS
      subroutine validate_os_name(os_name, is_valid)

        !> Name of an operating system
        character(len=:), allocatable, intent(in) :: os_name

        !> Boolean value of whether os_name is valid or not
        logical, intent(out) :: is_valid

        select case (os_name)
          case ("linux", "macos", "windows", "cygwin", "solaris", "freebsd", &
                          & "openbsd", "unknown")
            is_valid = .true.
          case default
            is_valid = .false.
        end select

      end subroutine validate_os_name

      !> Match os_type enum to a lowercase string with name of OS
      subroutine match_os_type(os_name, os_type)

        !> Name of operating system
        character(len=:), allocatable, intent(in) :: os_name

        !> Enum representing type of OS
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

      subroutine validate_profile_table(profile_name, compiler_name, key_list, table, error, os_valid)

        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> List of keys in the table
        type(toml_key), allocatable, intent(in) :: key_list(:)

        !> Table containing OS tables
        type(toml_table), pointer, intent(in) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Was called with valid operating system
        logical, intent(in) :: os_valid

        character(len=:), allocatable :: flags, c_flags, cxx_flags, link_time_flags, key_name, file_name, file_flags, err_message
        type(toml_table), pointer :: files
        type(toml_key), allocatable :: file_list(:)
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
            else if (key_name.eq.'c-flags') then
              call get_value(table, 'c-flags', c_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "c-flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'cxx-flags') then
              call get_value(table, 'cxx-flags', cxx_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "cxx-flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'link-time-flags') then
              call get_value(table, 'link-time-flags', link_time_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "link-time-flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'files') then
              call get_value(table, 'files', files, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "files has to be a table")
                return
              end if
              call files%get_keys(file_list)
              do ifile=1,size(file_list)
                file_name = file_list(ifile)%key
                call get_value(files, file_name, file_flags, stat=stat)
                if (stat /= toml_stat%success) then
                  call syntax_error(error, "file scope flags has to be a key-value pair")
                  return
                end if
              end do
            else if (.not. os_valid) then
                call validate_os_name(key_name, is_valid)
                err_message = "Unexpected key " // key_name // " found in profile table "//profile_name//" "//compiler_name//"."
                if (.not. is_valid) call syntax_error(error, err_message)
            else
                err_message = "Unexpected key " // key_name // " found in profile table "//profile_name//" "//compiler_name//"."
                call syntax_error(error, err_message)
            end if
          end do
        end if

        if (allocated(error)) return

      end subroutine validate_profile_table

      !> Look for flags, c-flags, link-time-flags key-val pairs
      !> and files table in a given table and create new profiles
      subroutine get_flags(profile_name, compiler_name, os_type, key_list, table, profiles, profindex, os_valid)

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

        !> Was called with valid operating system
        logical, intent(in) :: os_valid

        character(len=:), allocatable :: flags, c_flags, cxx_flags, link_time_flags, key_name, file_name, file_flags, err_message
        type(toml_table), pointer :: files
        type(toml_key), allocatable :: file_list(:)
        type(file_scope_flag), allocatable :: file_scope_flags(:)
        integer :: ikey, ifile, stat
        logical :: is_valid

        call get_value(table, 'flags', flags)
        call get_value(table, 'c-flags', c_flags)
        call get_value(table, 'cxx-flags', cxx_flags)
        call get_value(table, 'link-time-flags', link_time_flags)
        call get_value(table, 'files', files)
        if (associated(files)) then
          call files%get_keys(file_list)
          allocate(file_scope_flags(size(file_list)))
          do ifile=1,size(file_list)
            file_name = file_list(ifile)%key
            call get_value(files, file_name, file_flags)
            associate(cur_file=>file_scope_flags(ifile))
              if (.not.(path.eq."")) file_name = join_path(path, file_name)
              cur_file%file_name = file_name
              cur_file%flags = file_flags
            end associate
          end do
        end if

        profiles(profindex) = new_profile(profile_name, compiler_name, os_type, &
                 & flags, c_flags, cxx_flags, link_time_flags, file_scope_flags)
        profindex = profindex + 1
      end subroutine get_flags
      
      !> Traverse operating system tables to obtain number of profiles
      subroutine traverse_oss_for_size(profile_name, compiler_name, os_list, table, profiles_size, error)
        
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
        integer, intent(inout) :: profiles_size

        type(toml_key), allocatable :: key_list(:)
        character(len=:), allocatable :: os_name, l_os_name
        type(toml_table), pointer :: os_node
        integer :: ios, stat
        logical :: is_valid, key_val_added, is_key_val

        if (size(os_list)<1) return
        key_val_added = .false.
        do ios = 1, size(os_list)
          os_name = os_list(ios)%key
          call validate_os_name(os_name, is_valid)
          if (is_valid) then
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              call syntax_error(error, "os "//os_name//" has to be a table")
              return
            end if
            call os_node%get_keys(key_list)
            profiles_size = profiles_size + 1
            call validate_profile_table(profile_name, compiler_name, key_list, os_node, error, .true.)
          else
            ! Not lowercase OS name
            l_os_name = lower(os_name)
            call validate_os_name(l_os_name, is_valid)
            if (is_valid) then
              call fatal_error(error,'*traverse_oss*:Error: Name of the operating system must be a lowercase string.')
            end if
            if (allocated(error)) return

            ! Missing OS name
            is_key_val = .false.
            os_name = os_list(ios)%key
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              is_key_val = .true.
            end if
            os_node=>table
            if (is_key_val.and..not.key_val_added) then
              key_val_added = .true.
              is_key_val = .false.
              profiles_size = profiles_size + 1
            else if (.not.is_key_val) then
              profiles_size = profiles_size + 1
            end if
            call validate_profile_table(profile_name, compiler_name, os_list, os_node, error, .false.)
          end if
        end do
      end subroutine traverse_oss_for_size


      !> Traverse operating system tables to obtain profiles
      subroutine traverse_oss(profile_name, compiler_name, os_list, table, profiles, profindex, error)
        
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

        !> List of profiles
        type(profile_config_t), allocatable, intent(inout) :: profiles(:)

        !> Index in the list of profiles
        integer, intent(inout) :: profindex
        
        type(toml_key), allocatable :: key_list(:)
        character(len=:), allocatable :: os_name, l_os_name
        type(toml_table), pointer :: os_node
        integer :: ios, stat, os_type
        logical :: is_valid, is_key_val

        if (size(os_list)<1) return
        do ios = 1, size(os_list)
          os_name = os_list(ios)%key
          call validate_os_name(os_name, is_valid)
          if (is_valid) then
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              call syntax_error(error, "os "//os_name//" has to be a table")
              return
            end if
            call os_node%get_keys(key_list)
            call match_os_type(os_name, os_type)
            call get_flags(profile_name, compiler_name, os_type, key_list, os_node, profiles, profindex, .true.)
          else
            ! Not lowercase OS name
            l_os_name = lower(os_name)
            call validate_os_name(l_os_name, is_valid)
            if (is_valid) then
              call fatal_error(error,'*traverse_oss*:Error: Name of the operating system must be a lowercase string.')
            end if
            if (allocated(error)) return

            ! Missing OS name
            is_key_val = .false.
            os_name = os_list(ios)%key
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              is_key_val = .true.
            end if
            os_node=>table
            os_type = OS_ALL
            call get_flags(profile_name, compiler_name, os_type, os_list, os_node, profiles, profindex, .false.)
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
              call traverse_oss_for_size(profile_name, compiler_name, os_list, comp_node, profiles_size, error)
              if (allocated(error)) return
            else
              if (.not.(present(profiles).and.present(profindex))) then
                call fatal_error(error, "Both profiles and profindex have to be present")
                return
              end if
              call traverse_oss(profile_name, compiler_name, os_list, comp_node, &
                                & profiles, profindex, error)
              if (allocated(error)) return
            end if
          else
            call fatal_error(error,'*traverse_compilers*:Error: Compiler name not specified or invalid.')
          end if
        end do        
      end subroutine traverse_compilers

      !> Construct new profiles array from a TOML data structure
      subroutine new_profiles(profiles, table, error)

        !> Instance of the dependency configuration
        type(profile_config_t), allocatable, intent(out) :: profiles(:)

        !> Instance of the TOML data structure
        type(toml_table), target, intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: prof_node
        type(toml_key), allocatable :: prof_list(:)
        type(toml_key), allocatable :: comp_list(:)
        type(toml_key), allocatable :: os_list(:)
        character(len=:), allocatable :: profile_name, compiler_name
        integer :: profiles_size, iprof, stat, profindex
        logical :: is_valid
        type(profile_config_t), allocatable :: default_profiles(:)

        path = ''

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
              call traverse_oss_for_size(profile_name, compiler_name, os_list, table, profiles_size, error)
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

        profiles_size = profiles_size + size(default_profiles)
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
              call traverse_oss(profile_name, compiler_name, os_list, prof_node, profiles, profindex, error)
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
                profiles(profindex)%cxx_flags=profiles(profindex)%cxx_flags// &
                        & " "//profiles(iprof)%cxx_flags
                profiles(profindex)%link_time_flags=profiles(profindex)%link_time_flags// &
                        & " "//profiles(iprof)%link_time_flags
              end if
            end do
          end if
        end do
      end subroutine new_profiles

      !> Construct an array of built-in profiles
      function get_default_profiles(error) result(default_profiles)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(profile_config_t), allocatable :: default_profiles(:)

        default_profiles = [ &
              & new_profile('release', &
                & 'caf', &
                & OS_ALL, &
                & flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'gfortran', &
                & OS_ALL, &
                & flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops -fcoarray=single', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'f95', &
                & OS_ALL, &
                & flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -ffast-math -funroll-loops', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'nvfortran', &
                & OS_ALL, &
                & flags = ' -Mbackslash', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'ifort', &
                & OS_ALL, &
                & flags = ' -fp-model precise -pc64 -align all -error-limit 1 -reentrancy&
                          & threaded -nogen-interfaces -assume byterecl', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'ifort', &
                & OS_WINDOWS, &
                & flags = ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
                          & /nogen-interfaces /assume:byterecl', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'ifx', &
                & OS_ALL, &
                & flags = ' -fp-model=precise -pc64 -align all -error-limit 1 -reentrancy&
                          & threaded -nogen-interfaces -assume byterecl', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'ifx', &
                & OS_WINDOWS, &
                & flags = ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
                          & /nogen-interfaces /assume:byterecl', &
                & is_built_in=.true.), &
              & new_profile('release', &
                &'nagfor', &
                & OS_ALL, &
                & flags = ' -O4 -coarray=single -PIC', &
                & is_built_in=.true.), &
              & new_profile('release', &
                &'lfortran', &
                & OS_ALL, &
                & flags = ' flag_lfortran_opt', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'caf', &
                & OS_ALL, &
                & flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -fbacktrace', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'gfortran', &
                & OS_ALL, &
                & flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -fbacktrace -fcoarray=single', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'f95', &
                & OS_ALL, &
                & flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -Wno-maybe-uninitialized -Wno-uninitialized -fbacktrace', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'nvfortran', &
                & OS_ALL, &
                & flags = ' -Minform=inform -Mbackslash -g -Mbounds -Mchkptr -Mchkstk -traceback', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifort', &
                & OS_ALL, &
                & flags = ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifort', &
                & OS_WINDOWS, &
                & flags = ' /warn:all /check:all /error-limit:1&
                          & /Od /Z7 /assume:byterecl /traceback', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifx', &
                & OS_ALL, &
                & flags = ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifx', &
                & OS_WINDOWS, &
                & flags = ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifx', &
                & OS_WINDOWS, &
                & flags = ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'lfortran', &
                & OS_ALL, &
                & flags = '', &
                & is_built_in=.true.) &
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

      !> Print a representation of profile_config_t
      function info_profile(profile) result(s)

        !> Profile to be represented
        type(profile_config_t), intent(in) :: profile

        !> String representation of given profile
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
        if (allocated(profile%cxx_flags)) s = s // ', cxx_flags="' // profile%cxx_flags // '"'
        if (allocated(profile%link_time_flags)) s = s // ', link_time_flags="' // profile%link_time_flags // '"'
        if (allocated(profile%file_scope_flags)) then
          do i=1,size(profile%file_scope_flags)
            s = s // ', flags for '//profile%file_scope_flags(i)%file_name// &
                    & ' ="' // profile%file_scope_flags(i)%flags // '"'
          end do
        end if
        s = s // ")"

      end function info_profile

      !> Look for profile with given configuration in array profiles
      subroutine find_profile(profiles, profile_name, compiler, os_type, found_matching, chosen_profile)

        !> Array of profiles
        type(profile_config_t), allocatable, intent(in) :: profiles(:)

        !> Name of profile
        character(:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(:), allocatable, intent(in) :: compiler

        !> Type of operating system (enum)
        integer, intent(in) :: os_type

        !> Boolean value containing true if matching profile was found
        logical, intent(out) :: found_matching

        !> Last matching profile in the profiles array
        type(profile_config_t), intent(out) :: chosen_profile

        character(:), allocatable :: curr_profile_name
        character(:), allocatable :: curr_compiler
        integer :: curr_os
        integer :: i, priority, curr_priority

        found_matching = .false.
        if (size(profiles) < 1) return
        ! Try to find profile with matching OS type
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
        ! Try to find profile with OS type 'all'
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
