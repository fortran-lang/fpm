!> Platform configuration type.
!>
!> This type captures only the target compiler and operating-system
!> selector, and implements the standard serialization interface
!> (serializable_t) used across FPM manifest classes.
!>
!> TOML representation:
!>   compiler = "<name>"   # e.g., "gfortran", "ifx", "all"
!>   os       = "<name>"   # e.g., "linux", "macos", "windows", "all"
module fpm_manifest_platform
    use fpm_error,      only : error_t, fatal_error
    use tomlf,          only : toml_table
    use fpm_toml,       only : serializable_t, set_string, get_value
    use fpm_environment,only : OS_ALL, OS_NAME, match_os_type, OS_UNKNOWN, validate_os_name, &
        OS_WINDOWS, OS_LINUX, OS_MACOS
    use fpm_compiler,   only : compiler_enum, compiler_id_name, match_compiler_type, id_all, &
        id_unknown, validate_compiler_name, id_intel_classic_nix, id_intel_classic_mac, &
        id_intel_classic_windows, id_intel_llvm_nix, id_intel_llvm_windows
    use fpm_strings,    only : lower
    implicit none
    private

    public :: platform_config_t
    public :: is_platform_key

    !> Serializable platform configuration (compiler + OS only)
    type, extends(serializable_t) :: platform_config_t
        
        integer(compiler_enum) :: compiler = id_all
        integer                :: os_type  = OS_ALL
        
    contains
    
        procedure :: serializable_is_same => platform_is_same
        procedure :: dump_to_toml
        procedure :: load_from_toml
                
        !> Print information
        procedure :: info
        
        !> Return .true. if THIS platform selector is compatible with CURRENT (wildcards allowed)
        procedure :: matches => platform_is_suitable        
        
        !> Get compiler name as string
        procedure :: compiler_name => platform_compiler_name
        
        !> Get OS name as string  
        procedure :: os_name => platform_os_name
        
        !> Get configuration name as it appears in the manifest
        procedure :: name => platform_config_name
        
        !> Validation
        procedure :: is_valid => platform_is_valid
        
        !> Properties
        procedure, non_overridable :: any_compiler
        procedure, non_overridable :: any_os
        procedure, non_overridable :: any_platform
        
    end type platform_config_t
    
    ! Overloaded initializer
    interface platform_config_t
        module procedure new_platform
        module procedure new_platform_id
    end interface

    character(len=*), parameter, private :: class_name = 'platform_config_t'

contains

    !> Initialize a new platform config from compiler name
    !> Automatically selects correct Intel compiler version based on OS
    type(platform_config_t) function new_platform(compiler, os_type)
        character(*), intent(in) :: compiler
        integer, intent(in) :: os_type
        
        new_platform%compiler = match_compiler_type(compiler)
        new_platform%os_type  = os_type
        
        ! Correct Intel compiler ID based on OS (fallback to unix version for OS_ALL)
        new_platform%compiler = correct_compiler_for_os(new_platform%compiler, os_type)
        
    end function new_platform

    !> Initialize a new platform config from compiler enum ID  
    !> Automatically selects correct Intel compiler version based on OS
    type(platform_config_t) function new_platform_id(compiler_id, os_type)
        integer(compiler_enum), intent(in) :: compiler_id
        integer, intent(in) :: os_type
        
        new_platform_id%compiler = compiler_id
        new_platform_id%os_type  = os_type
        
        ! Correct Intel compiler ID based on OS (fallback to unix version for OS_ALL)
        new_platform_id%compiler = correct_compiler_for_os(new_platform_id%compiler, os_type)
        
    end function new_platform_id

    !> Correct Intel compiler ID to match the target OS
    !> Returns the appropriate OS-specific Intel compiler variant
    function correct_compiler_for_os(compiler_id, os_type) result(corrected_id)
        integer(compiler_enum), intent(in) :: compiler_id
        integer, intent(in) :: os_type
        integer(compiler_enum) :: corrected_id
        
        corrected_id = compiler_id  ! Default: no change
        
        ! Intel classic compilers: map to OS-specific version
        select case (compiler_id)
        case (id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows)
            select case (os_type)
            case (OS_WINDOWS)
                corrected_id = id_intel_classic_windows
            case (OS_MACOS)
                corrected_id = id_intel_classic_mac
            case default
                corrected_id = id_intel_classic_nix  ! Fallback to unix version
            end select
            
        case (id_intel_llvm_nix, id_intel_llvm_windows)
            select case (os_type)
            case (OS_WINDOWS)
                corrected_id = id_intel_llvm_windows
            case default
                corrected_id = id_intel_llvm_nix  ! Fallback to unix version
            end select
        end select
        
    end function correct_compiler_for_os

    !> Check if two Intel compiler IDs are equivalent (same family, different OS versions)
    logical function intel_compilers_equivalent(compiler1, compiler2) result(equivalent)
        integer(compiler_enum), intent(in) :: compiler1, compiler2
        
        equivalent = .false.
        
        ! Intel classic compilers are equivalent across OS variants
        if (any(compiler1 == [id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows]) .and. &
            any(compiler2 == [id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows])) then
            equivalent = .true.
            return
        end if
        
        ! Intel LLVM compilers are equivalent across OS variants
        if (any(compiler1 == [id_intel_llvm_nix, id_intel_llvm_windows]) .and. &
            any(compiler2 == [id_intel_llvm_nix, id_intel_llvm_windows])) then
            equivalent = .true.
            return
        end if
        
    end function intel_compilers_equivalent

    !> Compare two platform_config_t (semantic equality)
    logical function platform_is_same(this, that)
        class(platform_config_t), intent(in) :: this
        class(serializable_t),     intent(in) :: that

        platform_is_same = .false.
        select type (other => that)
        type is (platform_config_t)
            if (this%compiler /= other%compiler) return
            if (this%os_type  /= other%os_type ) return
        class default
            return
        end select
        platform_is_same = .true.
    end function platform_is_same

    !> Dump to TOML table
    subroutine dump_to_toml(self, table, error)
        class(platform_config_t), intent(inout) :: self
        type(toml_table),         intent(inout) :: table
        type(error_t), allocatable, intent(out) :: error

        ! Compiler as canonical name (e.g., "gfortran", "ifx", "all")
        call set_string(table, "compiler", compiler_id_name(self%compiler), error, class_name)
        if (allocated(error)) return

        ! OS as canonical name (e.g., "linux", "macos", "windows", "all")
        call set_string(table, "os", OS_NAME(self%os_type), error, class_name)
        if (allocated(error)) return
    end subroutine dump_to_toml


    !> Load from TOML table
    subroutine load_from_toml(self, table, error)
        class(platform_config_t), intent(inout) :: self
        type(toml_table),         intent(inout) :: table
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: s

        ! Compiler (default "all")
        call get_value(table, "compiler", s, "all")
        self%compiler = match_compiler_type(s)
        if (self%compiler == id_unknown) then
            call fatal_error(error, class_name//": unsupported compiler '"//s//"'")
            return
        end if

        ! OS (default "all")
        call get_value(table, "os", s, "all")
        self%os_type = match_os_type(s)
        ! match_os_type should map unknowns to a sentinel; rely on it for validation.
        ! If you prefer a hard failure on unknown here, uncomment the next block:
        ! if (self%os_type == OS_UNKNOWN) then
        !     call fatal_error(error, class_name//": unsupported os '"//s//"'")
        !     return
        ! end if
    end subroutine load_from_toml

    !> Write information on instance (similar style to profile_config_t%info)
    subroutine info(self, unit, verbosity)
        class(platform_config_t), intent(in) :: self
        integer,                  intent(in) :: unit
        integer,        optional, intent(in) :: verbosity

        integer :: pr
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        pr = merge(verbosity, 1, present(verbosity))

        write(unit, fmt) "Platform"
        write(unit, fmt) "- compiler", compiler_id_name(self%compiler)
        write(unit, fmt) "- os",       OS_NAME(self%os_type)

        ! Currently 'verbosity' does not expand output; reserved for future fields.
    end subroutine info

    !> Return .true. if SELF is suitable for a given target platform
    !>
    !> Rules:
    !>   - compiler matches if SELF%compiler == id_all OR == target%compiler
    !>   - os matches        if SELF%os_type  == OS_ALL  OR == target%os_type
    !>   - id_unknown / OS_UNKNOWN in SELF are treated as "no match" (conservative)
    !>   - Intel compilers must match OS (ifort unix/windows versions use different flags)
    logical function platform_is_suitable(self, target) result(ok)
        class(platform_config_t), intent(in) :: self
        type(platform_config_t),  intent(in) :: target

        logical :: compiler_ok, os_ok

        ! Check that both platforms are valid
        if (.not. self%is_valid() .or. .not. target%is_valid()) then
            ok = .false.
            return
        end if

        compiler_ok = any(self%compiler == [id_all,target%compiler])
        os_ok       = any(self%os_type  == [OS_ALL,target%os_type])

        ! Basic matching
        ok = compiler_ok .and. os_ok
        
        if (.not. ok) return

        ! Additional validation: Intel compilers must have compatible OS
        ! ifort on Unix/Mac should not match ifort on Windows and vice versa
        if (self%compiler /= id_all .and. self%os_type /= OS_ALL) then
            ok = compiler_os_compatible(self%compiler, self%os_type) .and. &
                 compiler_os_compatible(target%compiler, target%os_type)
        end if
        
    end function platform_is_suitable

    !> Check if a platform configuration is valid (no unknowns, compatible compiler+OS)
    logical function platform_is_valid(self) result(valid)
        class(platform_config_t), intent(in) :: self
                
        ! Check compiler+OS compatibility
        valid = compiler_os_compatible(self%compiler, self%os_type)
        
    end function platform_is_valid

    !> Check if a compiler ID is compatible with an OS type
    elemental logical function compiler_os_compatible(compiler_id, os_type) result(compatible)
        integer(compiler_enum), intent(in) :: compiler_id
        integer, intent(in) :: os_type
        
        ! Check for unknowns
        if (compiler_id == id_unknown .or. os_type == OS_UNKNOWN) then
            compatible = .false.
            return
        end if
        
        ! Intel classic compilers: OS-specific variants
        select case (compiler_id)
            case (id_intel_classic_windows)
                compatible = any(os_type == [OS_ALL,OS_WINDOWS])
            case (id_intel_classic_nix)
                compatible = any(os_type == [OS_ALL,OS_LINUX])
            case (id_intel_classic_mac) 
                compatible = any(os_type == [OS_ALL,OS_MACOS])
            case (id_intel_llvm_windows)
                compatible = any(os_type == [OS_ALL,OS_WINDOWS])
            case (id_intel_llvm_nix)
                compatible = any(os_type == [OS_ALL,OS_LINUX,OS_MACOS])
            case default
                ! Other compilers are compatible with any OS
                compatible = os_type/=OS_UNKNOWN .and. compiler_id/=id_unknown
        end select
        
    end function compiler_os_compatible

    !> Check if a key (os or compiler) can be used for platform setting
    elemental logical function is_platform_key(key)
        character(*), intent(in) :: key
        
        call validate_compiler_name(key, is_platform_key)
        if (is_platform_key) return

        call validate_os_name(key, is_platform_key)
        if (is_platform_key) return        
        
    end function is_platform_key

    !> Get compiler name as string
    function platform_compiler_name(self) result(name)
        class(platform_config_t), intent(in) :: self
        character(len=:), allocatable :: name
        
        name = compiler_id_name(self%compiler)
    end function platform_compiler_name
    
    !> Get OS name as string
    function platform_os_name(self) result(name)
        class(platform_config_t), intent(in) :: self
        character(len=:), allocatable :: name
        
        name = OS_NAME(self%os_type)
    end function platform_os_name
    
    !> Get configuration name
    function platform_config_name(self) result(name)
        class(platform_config_t), intent(in) :: self
        character(len=:), allocatable :: name
        
        if (self%os_type==OS_ALL .and. self%compiler==id_all) then 
            name = ""
        elseif (self%os_type==OS_ALL) then 
            name = self%compiler_name()
        elseif (self%compiler==id_all) then 
            name = self%os_name()
        else
            name = self%os_name()//'.'//self%compiler_name()
        end if
        
    end function platform_config_name
    
    !> Whether the configuration is generic
    elemental logical function any_compiler(self)
        class(platform_config_t), intent(in) :: self
        any_compiler = self%compiler == id_all
    end function any_compiler
    elemental logical function any_os(self)
        class(platform_config_t), intent(in) :: self
        any_os = self%os_type == OS_ALL
    end function any_os
    elemental logical function any_platform(self)
        class(platform_config_t), intent(in) :: self
        any_platform = any_os(self) .and. any_compiler(self)
    end function any_platform

end module fpm_manifest_platform
