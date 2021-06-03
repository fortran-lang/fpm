module fpm_manifest_profile
    use fpm_error, only : error_t, syntax_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    use fpm_strings, only: lower
    use fpm_environment, only: get_os_type, OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
    implicit none
    private

    public :: profile_config_t, new_profile, new_profiles, find_profile

    !> Configuration meta data for a profile
    type :: profile_config_t
      !> Name of the profile
      character(len=:), allocatable :: profile_name

      !> Name of the compiler
      character(len=:), allocatable :: compiler
      
      !> Name of the OS
      character(len=:), allocatable :: os
      
      !> Compiler flags
      character(len=:), allocatable :: compiler_flags

      contains

        !> Print information on this instance
        procedure :: info
    end type profile_config_t

    contains

      !> Construct a new profile configuration from a TOML data structure
      subroutine new_profile(self, profile_name, compiler, os, compiler_flags, error)
        type(profile_config_t), intent(out) :: self
        
        !> Name of the profile
        character(len=:), allocatable, intent(in) :: profile_name
        
        !> Name of the compiler
        character(len=:), allocatable, intent(in) :: compiler
        
        !> Name of the OS
        character(len=:), allocatable, intent(in) :: os
        
        !> Compiler flags
        character(len=:), allocatable, intent(in) :: compiler_flags
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
       
        self%profile_name = profile_name
        self%compiler = compiler
        self%os = os
        self%compiler_flags = compiler_flags
      end subroutine new_profile

      !> Check if compiler name is a valid compiler name
      subroutine validate_compiler_name(compiler_name, is_valid)
        character(len=:), allocatable, intent(in) :: compiler_name
        logical, intent(out) :: is_valid
        select case(compiler_name)
          case("gfortran", "ifort", "ifx", "pgfortran", "nvfrotran", "flang", &
                        &"lfortran", "lfc", "nagfor", "crayftn", "xlf90", "ftn95")
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
        
        character(len=:), allocatable :: os_name
        type(toml_table), pointer :: os_node
        character(len=:), allocatable :: compiler_flags
        integer :: ios, stat

        if (size(os_list)<1) return
        do ios = 1, size(os_list)
          if (present(profiles_size)) then
            profiles_size = profiles_size + 1
          else
            if (.not.(present(profiles).and.present(profindex))) then
              print *,"Error in traverse_oss"
              return
            end if
            os_name = os_list(ios)%key
            call get_value(table, os_name, os_node, stat=stat)
            os_name = lower(os_name)
            if (stat /= toml_stat%success) then
              call get_value(table, 'flags', compiler_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "Compiler flags "//compiler_flags//" must be a table entry")
                exit
              end if
              os_name = "all"
              call new_profile(profiles(profindex), profile_name, compiler_name, os_name, compiler_flags, error)
              profindex = profindex + 1
            else
              call get_value(os_node, 'flags', compiler_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "Compiler flags "//compiler_flags//" must be a table entry")
                compiler_flags="did not work"
              end if
              call new_profile(profiles(profindex), profile_name, compiler_name, os_name, compiler_flags, error)
              profindex = profindex + 1
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
            else
              if (.not.(present(profiles).and.present(profindex))) then
                print *,"Error in traverse_compilers"
                return
              end if
              call traverse_oss(profile_name, compiler_name, os_list, comp_node, &
                                & error, profiles=profiles, profindex=profindex)
            end if
          else
            os_list = comp_list(icomp:icomp)
            compiler_name = "default"

            if (present(profiles_size)) then
              call traverse_oss(profile_name, compiler_name, os_list, table, error, profiles_size=profiles_size)
            else
              if (.not.(present(profiles).and.present(profindex))) then
                print *,"Error in traverse_compilers"
                return
              end if
              call traverse_oss(profile_name, compiler_name, os_list, table, &
                                & error, profiles=profiles, profindex=profindex)
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
        character(len=:), allocatable :: profile_name
        integer :: profiles_size, iprof, stat, profindex
        logical :: is_valid

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
          else
            call get_value(table, profile_name, prof_node, stat=stat)
            if (stat /= toml_stat%success) then
              call syntax_error(error, "Profile "//prof_list(iprof)%key//" must be a table entry")
              exit
            end if
            call prof_node%get_keys(comp_list)
            call traverse_compilers(profile_name, comp_list, prof_node, error, profiles_size=profiles_size)
          end if
        end do
       
        allocate(profiles(profiles_size))
        
        profindex = 1

        do iprof = 1, size(prof_list)
          profile_name = prof_list(iprof)%key
          call validate_compiler_name(profile_name, is_valid)
          if (is_valid) then
            profile_name = "all"
            comp_list = prof_list(iprof:iprof)
            prof_node=>table
            call traverse_compilers(profile_name, comp_list, prof_node, error, profiles=profiles, profindex=profindex)
          else
            call get_value(table, profile_name, prof_node, stat=stat)
            call prof_node%get_keys(comp_list)
            call traverse_compilers(profile_name, comp_list, prof_node, error, profiles=profiles, profindex=profindex)
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
        if (allocated(self%profile_name)) then
            write(unit, fmt) "- profile name", self%profile_name
        end if

        if (allocated(self%compiler)) then
            write(unit, fmt) "- compiler", self%compiler
        end if

        if (allocated(self%os)) then
            write(unit, fmt) "- os", self%os
        end if

        if (allocated(self%compiler_flags)) then
            write(unit, fmt) "- compiler flags", self%compiler_flags
        end if

      end subroutine info

      subroutine find_profile(profiles, profile_name, compiler, compiler_flags)
        type(profile_config_t), allocatable, intent(in) :: profiles(:)
        character(:), allocatable, intent(in) :: profile_name
        character(:), allocatable, intent(in) :: compiler
        character(:), allocatable, intent(out) :: compiler_flags
        character(:), allocatable :: curr_profile_name
        character(:), allocatable :: curr_compiler
        character(:), allocatable :: curr_os
        character(len=:),allocatable :: os_type
        type(profile_config_t) :: chosen_profile
        integer :: i, priority, curr_priority

        select case (get_os_type())
          case (OS_LINUX);   os_type =  "linux"
          case (OS_MACOS);   os_type =  "macos"
          case (OS_WINDOWS); os_type =  "windows"
          case (OS_CYGWIN);  os_type =  "cygwin"
          case (OS_SOLARIS); os_type =  "solaris"
          case (OS_FREEBSD); os_type =  "freebsd"
          case (OS_OPENBSD); os_type =  "openbsd"
          case (OS_UNKNOWN); os_type =  "unknown"
          case default     ; os_type =  "UNKNOWN"
        end select
        
        print *, os_type
        priority = 0
        print *,profile_name,compiler,os_type
        do i=1,size(profiles)
          curr_priority = 0
          curr_profile_name = profiles(i)%profile_name
          curr_compiler = profiles(i)%compiler
          curr_os = profiles(i)%os
          if (curr_profile_name.eq.profile_name.or.curr_profile_name.eq.'all') then
            if (curr_profile_name.eq.'all') then
              curr_priority= curr_priority + 4
            end if
            if (curr_compiler.eq.compiler.or.curr_compiler.eq.'default') then
              if (curr_compiler.eq.'default') then
                curr_priority = curr_priority + 2
              end if
              if (curr_os.eq.os_type.or.curr_os.eq.'all') then
                if (curr_os.eq.'all') then
                  curr_priority = curr_priority + 1
                end if
                print *,"found matching profile with priority ",curr_priority, curr_profile_name//" "//curr_compiler &
                        &//" "//curr_os//" "//profiles(i)%compiler_flags
                if (curr_priority > priority) then
                  chosen_profile = profiles(i)
                  priority = curr_priority
                  print *, priority
                end if
              end if
            end if
          end if
        end do
        compiler_flags = chosen_profile%compiler_flags
      end subroutine find_profile
end module fpm_manifest_profile
