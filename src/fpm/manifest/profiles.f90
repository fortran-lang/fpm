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
    use fpm_manifest_feature, only: feature_config_t, new_feature, find_feature
    use fpm_error, only : error_t, syntax_error, fatal_error, fpm_stop
    use tomlf, only : toml_table, toml_key, toml_stat
    use fpm_toml, only : get_value, serializable_t, set_value, &
                         set_string, add_table
    use fpm_strings, only: lower
    use fpm_manifest_platform, only: platform_config_t
    use fpm_environment, only: get_os_type, OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD, OS_NAME, OS_ALL, &
                             validate_os_name, match_os_type
    use fpm_compiler, only: compiler_enum, compiler_id_name, match_compiler_type, &
                            validate_compiler_name
    use fpm_filesystem, only: join_path
    implicit none
    public :: profile_config_t, new_profile, new_profiles, find_profile, DEFAULT_COMPILER

    !> Name of the default compiler
    character(len=*), parameter :: DEFAULT_COMPILER = 'gfortran'    
    character(len=:), allocatable :: path

    !> Type storing file name - file scope compiler flags pairs
    type, extends(serializable_t) :: file_scope_flag

      !> Name of the file
      character(len=:), allocatable :: file_name

      !> File scope flags
      character(len=:), allocatable :: flags

      contains

          !> Serialization interface
          procedure :: serializable_is_same => file_scope_same
          procedure :: dump_to_toml => file_scope_dump
          procedure :: load_from_toml => file_scope_load

    end type file_scope_flag

    !> Configuration meta data for a profile (now based on features)
    type, extends(serializable_t) :: profile_config_t
        
      !> Profile feature - contains all profile configuration
      type(feature_config_t) :: profile_feature

      !> File scope flags (maintained for backwards compatibility)
      type(file_scope_flag), allocatable :: file_scope_flags(:)

      contains

        !> Print information on this instance
        procedure :: info

        !> Serialization interface
        procedure :: serializable_is_same => profile_same
        procedure :: dump_to_toml => profile_dump
        procedure :: load_from_toml => profile_load

        !> Convenience accessors for backward compatibility
        procedure :: profile_name => get_profile_name
        procedure :: compiler => get_profile_compiler
        procedure :: os_type => get_profile_os_type  
        procedure :: flags => get_profile_flags
        procedure :: c_flags => get_profile_c_flags
        procedure :: cxx_flags => get_profile_cxx_flags
        procedure :: link_time_flags => get_profile_link_time_flags
        procedure :: is_built_in => get_profile_is_built_in

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
        integer(compiler_enum) :: compiler_id

        ! Initialize the profile feature
        profile%profile_feature%name = profile_name
        profile%profile_feature%platform = platform_config_t(compiler, os_type)
        if (present(is_built_in)) then
           profile%profile_feature%default = is_built_in
        else
           profile%profile_feature%default = .false.
        end if
        
        ! Set flags
        if (present(flags)) then
          profile%profile_feature%flags = flags
        else
          profile%profile_feature%flags = ""
        end if
        if (present(c_flags)) then
          profile%profile_feature%c_flags = c_flags
        else
          profile%profile_feature%c_flags = ""
        end if
        if (present(cxx_flags)) then
          profile%profile_feature%cxx_flags = cxx_flags
        else
          profile%profile_feature%cxx_flags = ""
        end if
        if (present(link_time_flags)) then
          profile%profile_feature%link_time_flags = link_time_flags
        else
          profile%profile_feature%link_time_flags = ""
        end if
        
        ! Set file scope flags (maintained for backward compatibility)
        if (present(file_scope_flags)) then
           profile%file_scope_flags = file_scope_flags
        end if

      end function new_profile

      !> Match lowercase string with name of OS to os_type enum
      function os_type_name(os_type)

        !> Name of operating system
        character(len=:), allocatable :: os_type_name

        !> Enum representing type of OS
        integer, intent(in) :: os_type

        select case (os_type)
          case (OS_ALL); os_type_name = "all"
          case default; os_type_name = lower(OS_NAME(os_type))
        end select

      end function os_type_name

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
            os_type = match_os_type(os_name)
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

        ! Default profiles are now features - no longer used
        allocate(default_profiles(0))
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
          if (profiles(iprof)%profile_feature%name == 'all') then
            do profindex = 1,size(profiles)
              if (.not.(profiles(profindex)%profile_feature%name == 'all') &
                      & .and.(profiles(profindex)%profile_feature%platform == profiles(iprof)%profile_feature%platform)) then
                profiles(profindex)%profile_feature%flags = profiles(profindex)%profile_feature%flags // &
                        & " " // profiles(iprof)%profile_feature%flags
                profiles(profindex)%profile_feature%c_flags = profiles(profindex)%profile_feature%c_flags // &
                        & " " // profiles(iprof)%profile_feature%c_flags
                profiles(profindex)%profile_feature%cxx_flags = profiles(profindex)%profile_feature%cxx_flags // &
                        & " " // profiles(iprof)%profile_feature%cxx_flags
                profiles(profindex)%profile_feature%link_time_flags = profiles(profindex)%profile_feature%link_time_flags // &
                        & " " // profiles(iprof)%profile_feature%link_time_flags
              end if
            end do
          end if
        end do
      end subroutine new_profiles

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
        if (allocated(self%profile_feature%name)) then
            write(unit, fmt) "- profile name", self%profile_feature%name
        end if
        
        call self%profile_feature%platform%info(unit, verbosity)

        if (allocated(self%profile_feature%flags)) then
            write(unit, fmt) "- compiler flags", self%profile_feature%flags
        end if

      end subroutine info

      !> Look for profile with given configuration in array profiles
      subroutine find_profile(profiles, profile_name, target, found_matching, chosen_profile)

        !> Array of profiles
        type(profile_config_t), allocatable, intent(in) :: profiles(:)

        !> Name of profile
        character(:), allocatable, intent(in) :: profile_name

        ! Target platform
        type(platform_config_t), intent(in) :: target

        !> Boolean value containing true if matching profile was found
        logical, intent(out) :: found_matching

        !> Last matching profile in the profiles array
        type(profile_config_t), intent(out) :: chosen_profile

        integer :: i

        found_matching = .false.
        if (size(profiles) < 1) return
        
       
        ! Try to find profile with matching OS type
        do i=1,size(profiles)
            
          associate (feat => profiles(i)%profile_feature)  
            
          if (profiles(i)%profile_feature%name == profile_name) then
            if (profiles(i)%profile_feature%platform%matches(target)) then
                chosen_profile = profiles(i)
                found_matching = .true.
                return
            end if
          end if
          
          endassociate
          
        end do
        
      end subroutine find_profile


      logical function file_scope_same(this,that)
          class(file_scope_flag), intent(in) :: this
          class(serializable_t), intent(in) :: that

          file_scope_same = .false.

          select type (other=>that)
             type is (file_scope_flag)
                if (allocated(this%file_name).neqv.allocated(other%file_name)) return
                if (allocated(this%file_name)) then
                    if (.not.(this%file_name==other%file_name)) return
                endif
                if (allocated(this%flags).neqv.allocated(other%flags)) return
                if (allocated(this%flags)) then
                    if (.not.(this%flags==other%flags)) return
                endif

             class default
                ! Not the same type
                return
          end select

          !> All checks passed!
          file_scope_same = .true.

    end function file_scope_same

    !> Dump to toml table
    subroutine file_scope_dump(self, table, error)

       !> Instance of the serializable object
       class(file_scope_flag), intent(inout) :: self

       !> Data structure
       type(toml_table), intent(inout) :: table

       !> Error handling
       type(error_t), allocatable, intent(out) :: error

       call set_string(table, "file-name", self%file_name, error)
       if (allocated(error)) return
       call set_string(table, "flags", self%flags, error)
       if (allocated(error)) return

     end subroutine file_scope_dump

     !> Read from toml table (no checks made at this stage)
     subroutine file_scope_load(self, table, error)

        !> Instance of the serializable object
        class(file_scope_flag), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call get_value(table, "file-name", self%file_name)
        call get_value(table, "flags", self%flags)

     end subroutine file_scope_load

      logical function profile_same(this,that)
          class(profile_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that

          integer :: ii

          profile_same = .false.

          select type (other=>that)
             type is (profile_config_t)
                
                ! Compare the underlying features
                if (.not.(this%profile_feature==other%profile_feature)) return

                ! Compare file scope flags (maintained for backward compatibility)
                if (allocated(this%file_scope_flags).neqv.allocated(other%file_scope_flags)) return
                if (allocated(this%file_scope_flags)) then
                    if (.not.size(this%file_scope_flags)==size(other%file_scope_flags)) return
                    do ii=1,size(this%file_scope_flags)
                       if (.not.this%file_scope_flags(ii)==other%file_scope_flags(ii)) return
                    end do
                endif

             class default
                ! Not the same type
                return
          end select

          !> All checks passed!
          profile_same = .true.

    end function profile_same

    !> Dump to toml table
    subroutine profile_dump(self, table, error)

       !> Instance of the serializable object
       class(profile_config_t), intent(inout) :: self

       !> Data structure
       type(toml_table), intent(inout) :: table

       !> Error handling
       type(error_t), allocatable, intent(out) :: error

       !> Local variables
       integer :: ierr, ii
       type(toml_table), pointer :: ptr_deps, ptr
       character(len=30) :: unnamed

       ! Dump the underlying feature data
       call self%profile_feature%dump_to_toml(table, error)
       if (allocated(error)) return
       
       if (allocated(self%file_scope_flags)) then

           ! Create file scope flags table
           call add_table(table, "file-scope-flags", ptr_deps)
           if (.not. associated(ptr_deps)) then
              call fatal_error(error, "profile_config_t cannot create file scope table ")
              return
           end if

           do ii = 1, size(self%file_scope_flags)
              associate (dep => self%file_scope_flags(ii))

                 !> Because files need a name, fallback if this has no name
                 if (len_trim(dep%file_name)==0) then
                    write(unnamed,1) ii
                    call add_table(ptr_deps, trim(unnamed), ptr)
                 else
                    call add_table(ptr_deps, dep%file_name, ptr)
                 end if
                 if (.not. associated(ptr)) then
                    call fatal_error(error, "profile_config_t cannot create entry for file "//dep%file_name)
                    return
                 end if
                 call dep%dump_to_toml(ptr, error)
                 if (allocated(error)) return
              end associate
           end do

       endif

       1 format('UNNAMED_FILE_',i0)

     end subroutine profile_dump

     !> Read from toml table (no checks made at this stage)
     subroutine profile_load(self, table, error)

        !> Instance of the serializable object
        class(profile_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Local variables
        character(len=:), allocatable :: flag, compiler_name
        integer :: ii, jj
        type(toml_table), pointer :: ptr_dep, ptr
        type(toml_key), allocatable :: keys(:),dep_keys(:)

        call table%get_keys(keys)

        ! Load into feature structure
       ! Dump the underlying feature data
       call self%profile_feature%load_from_toml(table, error)
       if (allocated(error)) return

        if (allocated(self%file_scope_flags)) deallocate(self%file_scope_flags)
        sub_deps: do ii = 1, size(keys)

           select case (keys(ii)%key)
              case ("file-scope-flags")

               call get_value(table, keys(ii), ptr)
               if (.not.associated(ptr)) then
                  call fatal_error(error,'profile_config_t: error retrieving file_scope_flags table')
                  return
               end if

               !> Read all file scope flags
               call ptr%get_keys(dep_keys)
               allocate(self%file_scope_flags(size(dep_keys)))

               do jj = 1, size(dep_keys)

                   call get_value(ptr, dep_keys(jj), ptr_dep)
                   call self%file_scope_flags(jj)%load_from_toml(ptr_dep, error)
                   if (allocated(error)) return

               end do

           end select
        end do sub_deps

     end subroutine profile_load

      !> Convenience accessor procedures for backward compatibility

      !> Get profile name
      function get_profile_name(self) result(name)
        class(profile_config_t), intent(in) :: self
        character(len=:), allocatable :: name
        name = self%profile_feature%name
      end function get_profile_name

      !> Get compiler name
      function get_profile_compiler(self) result(compiler)
        class(profile_config_t), intent(in) :: self
        character(len=:), allocatable :: compiler
        compiler = compiler_id_name(self%profile_feature%platform%compiler)
      end function get_profile_compiler

      !> Get OS type
      function get_profile_os_type(self) result(os_type)
        class(profile_config_t), intent(in) :: self
        integer :: os_type
        os_type = self%profile_feature%platform%os_type
      end function get_profile_os_type

      !> Get flags
      function get_profile_flags(self) result(flags)
        class(profile_config_t), intent(in) :: self
        character(len=:), allocatable :: flags
        flags = self%profile_feature%flags
      end function get_profile_flags

      !> Get C flags
      function get_profile_c_flags(self) result(c_flags)
        class(profile_config_t), intent(in) :: self
        character(len=:), allocatable :: c_flags  
        c_flags = self%profile_feature%c_flags
      end function get_profile_c_flags

      !> Get C++ flags
      function get_profile_cxx_flags(self) result(cxx_flags)
        class(profile_config_t), intent(in) :: self
        character(len=:), allocatable :: cxx_flags
        cxx_flags = self%profile_feature%cxx_flags
      end function get_profile_cxx_flags

      !> Get link time flags
      function get_profile_link_time_flags(self) result(link_time_flags)
        class(profile_config_t), intent(in) :: self
        character(len=:), allocatable :: link_time_flags
        link_time_flags = self%profile_feature%link_time_flags
      end function get_profile_link_time_flags

      !> Get is_built_in flag (maps to feature default flag)
      function get_profile_is_built_in(self) result(is_built_in)
        class(profile_config_t), intent(in) :: self
        logical :: is_built_in
        is_built_in = self%profile_feature%default
      end function get_profile_is_built_in


end module fpm_manifest_profile
