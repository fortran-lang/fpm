module fpm_manifest_profile
    use fpm_error, only : error_t, syntax_error, fatal_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    use fpm_strings, only: lower
    use fpm_environment, only: get_os_type, OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
    implicit none
    private
    public :: profile_config_t, new_profile, new_profiles, get_default_profiles, &
            & find_profile, DEFAULT_COMPILER, NO_DEF_PROF

    character(len=*), parameter :: DEFAULT_COMPILER = 'gfortran' 
    integer, parameter :: NO_DEF_PROF = 18 ! Number of default profiles
    integer, parameter :: OS_ALL = -1
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

      contains

        !> Print information on this instance
        procedure :: info
    end type profile_config_t

    contains

      !> Construct a new profile configuration from a TOML data structure
      subroutine new_profile(self, profile_name, compiler, os_type, flags, c_flags, link_time_flags)
        type(profile_config_t), intent(out) :: self
        
        !> Name of the profile
        character(len=:), allocatable, intent(in) :: profile_name
        
        !> Name of the compiler
        character(len=:), allocatable, intent(in) :: compiler
        
        !> Type of the OS
        integer, intent(in) :: os_type
        
        !> Fortran compiler flags
        character(len=:), allocatable, optional, intent(in) :: flags

        !> C compiler flags
        character(len=:), allocatable, optional, intent(in) :: c_flags

        !> Link time compiler flags
        character(len=:), allocatable, optional, intent(in) :: link_time_flags
       
        self%profile_name = profile_name
        self%compiler = compiler
        self%os_type = os_type
        if (present(flags)) then
          self%flags = flags
        else
          self%flags = ""
        end if
        if (present(c_flags)) then
          self%c_flags = c_flags
        else
          self%c_flags = ""
        end if
        if (present(link_time_flags)) then
          self%link_time_flags = link_time_flags
        else
          self%link_time_flags = ""
        end if
!        print *,profile_name," ",compiler," ",os_type," ",flags
!        print *,profile_name," ",compiler," ",os_type," ",flags, " ",c_flags," ", link_time_flags
      end subroutine new_profile

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

      subroutine validate_key_name(key_name, is_valid)
        character(len=:), allocatable, intent(in) :: key_name
        logical, intent(out) :: is_valid
        select case (key_name)
          case ("flags", "c_flags", "link_time_flags")
            is_valid = .true.
          case default
            is_valid = .false.
        end select
      end subroutine validate_key_name

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

        character(len=:), allocatable :: flags, c_flags, link_time_flags, key_name
        integer :: ikey, stat
        logical :: is_valid

        if (size(key_list).ge.1) then
          do ikey=1,size(key_list)
            key_name = key_list(ikey)%key
!            call validate_key_name(key_name, is_valid)
!            if (.not. is_valid) then
!              call syntax_error(error, "Only flags, c_flags and link_time_flags are valid keys in profiles table")
!              return
!            end if
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
            else
              call get_value(table, 'link_time_flags', link_time_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "link_time_flags has to be a key-value pair")
                return
              end if
            end if
          end do
        end if

        if (.not.allocated(flags)) flags=''
        if (.not.allocated(c_flags)) c_flags=''
        if (.not.allocated(link_time_flags)) link_time_flags=''

        call new_profile(profiles(profindex), profile_name, compiler_name, os_type, flags, c_flags, link_time_flags)
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

        ! call get defaults
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

        profiles_size=profiles_size+NO_DEF_PROF
        allocate(profiles(profiles_size))
        
        call get_default_profiles(profiles)
        profindex = 1 + NO_DEF_PROF

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
      
      subroutine get_default_profiles(profiles)
        type(profile_config_t), allocatable, intent(inout) :: profiles(:)
        character(len=:), allocatable :: profile_name
        character(len=:), allocatable :: compiler
        character(len=:), allocatable :: flags
        integer :: os_type, i

        do i=1,NO_DEF_PROF
          if (i.le.(9)) then
            profile_name = 'release'
          else
            profile_name = 'debug'
          end if

          os_type = OS_ALL
          select case(i)
          ! release profiles
          case(1) !caf
            compiler='caf'
            flags='&
                & -O3&
                & -Wimplicit-interface&
                & -fPIC&
                & -fmax-errors=1&
                & -funroll-loops&
                &'
          case(2) !gcc
            compiler='gfortran'
            flags='&
                & -O3&
                & -Wimplicit-interface&
                & -fPIC&
                & -fmax-errors=1&
                & -funroll-loops&
                & -fcoarray=single&
                &'
          case(3) !f95
            compiler='f95'
            flags='&
                & -O3&
                & -Wimplicit-interface&
                & -fPIC&
                & -fmax-errors=1&
                & -ffast-math&
                & -funroll-loops&
                &'
          case(4) !nvhpc
            compiler='nvfrotran'
            flags = '&
                & -Mbackslash&
                &'
          case(5) !intel_classic
            compiler='ifort'
            flags = '&
                & -fp-model precise&
                & -pc64&
                & -align all&
                & -error-limit 1&
                & -reentrancy threaded&
                & -nogen-interfaces&
                & -assume byterecl&
                &'
          case(6) !intel_classic_windows
            compiler='ifort'
            os_type=OS_WINDOWS
            flags = '&
                & /fp:precise&
                & /align:all&
                & /error-limit:1&
                & /reentrancy:threaded&
                & /nogen-interfaces&
                & /assume:byterecl&
                &'
          case(7) !intel_llvm
            compiler='ifx'
            flags = '&
                & -fp-model=precise&
                & -pc64&
                & -align all&
                & -error-limit 1&
                & -reentrancy threaded&
                & -nogen-interfaces&
                & -assume byterecl&
                &'
          case(8) !intel_llvm_windows
            compiler='ifx'
            os_type = OS_WINDOWS
            flags = '&
                & /fp:precise&
                & /align:all&
                & /error-limit:1&
                & /reentrancy:threaded&
                & /nogen-interfaces&
                & /assume:byterecl&
                &'
          case(9) !nag
            compiler='nagfor'
            flags = ' &
                & -O4&
                & -coarray=single&
                & -PIC&
                &'

          ! debug profiles
          case(10) !caf
            compiler='caf'
            flags = '&
                & -Wall&
                & -Wextra&
                & -Wimplicit-interface&
                & -fPIC -fmax-errors=1&
                & -g&
                & -fcheck=bounds&
                & -fcheck=array-temps&
                & -fbacktrace&
                &'
          case(11) !gcc
            compiler='gfortran'
            flags = '&
                & -Wall&
                & -Wextra&
                & -Wimplicit-interface&
                & -fPIC -fmax-errors=1&
                & -g&
                & -fcheck=bounds&
                & -fcheck=array-temps&
                & -fbacktrace&
                & -fcoarray=single&
                &'
          case(12) !f95
            compiler='f95'
            flags = '&
                & -Wall&
                & -Wextra&
                & -Wimplicit-interface&
                & -fPIC -fmax-errors=1&
                & -g&
                & -fcheck=bounds&
                & -fcheck=array-temps&
                & -Wno-maybe-uninitialized -Wno-uninitialized&
                & -fbacktrace&
                &'
          case(13) !nvhpc
            compiler='nvfrotran'
            flags = '&
                & -Minform=inform&
                & -Mbackslash&
                & -g&
                & -Mbounds&
                & -Mchkptr&
                & -Mchkstk&
                & -traceback&
                &'
          case(14) !intel_classic
            compiler='ifort'
            flags = '&
                & -warn all&
                & -check all&
                & -error-limit 1&
                & -O0&
                & -g&
                & -assume byterecl&
                & -traceback&
                &'
          case(15) !intel_classic_windows
            compiler='ifort'
            os_type=OS_WINDOWS
            flags = '&
                & /warn:all&
                & /check:all&
                & /error-limit:1&
                & /Od&
                & /Z7&
                & /assume:byterecl&
                & /traceback&
                &'
          case(16) !intel_llvm
            compiler='ifx'
            flags = '&
                & -warn all&
                & -check all&
                & -error-limit 1&
                & -O0&
                & -g&
                & -assume byterecl&
                & -traceback&
                &'
          case(17) !intel_llvm_windows
            compiler='ifx'
            os_type=OS_WINDOWS
            flags = '&
                & /warn:all&
                & /check:all&
                & /error-limit:1&
                & /Od&
                & /Z7&
                & /assume:byterecl&
                &'
          case(18) !nag
            compiler='nagfor'
            flags = '&
                & -g&
                & -C=all&
                & -O0&
                & -gline&
                & -coarray=single&
                & -PIC&
                &'
          end select
          call new_profile(profiles(i), profile_name, compiler, os_type, flags)
        end do
      end subroutine get_default_profiles

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

      subroutine find_profile(profiles, profile_name, compiler, os_type, flags, c_flags, link_time_flags)
        type(profile_config_t), allocatable, intent(in) :: profiles(:)
        character(:), allocatable, intent(in) :: profile_name
        character(:), allocatable, intent(in) :: compiler
        integer, intent(in) :: os_type
        character(:), allocatable, intent(out), optional :: flags
        character(:), allocatable, intent(out), optional :: c_flags
        character(:), allocatable, intent(out), optional :: link_time_flags
        character(:), allocatable :: curr_profile_name
        character(:), allocatable :: curr_compiler
        integer :: curr_os
        type(profile_config_t) :: chosen_profile
        integer :: i, priority, curr_priority
        logical :: found_matching
        
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
        if (present(flags)) flags = chosen_profile%flags
        if (present(c_flags)) c_flags = chosen_profile%c_flags
        if (present(link_time_flags)) link_time_flags = chosen_profile%link_time_flags
      end subroutine find_profile
end module fpm_manifest_profile
