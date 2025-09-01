
module fpm_manifest_feature_collection
    use fpm_manifest_feature, only: feature_config_t, new_feature
    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_environment, only: OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN, OS_SOLARIS, &
                             OS_FREEBSD, OS_OPENBSD, OS_ALL, OS_NAME, match_os_type
    use fpm_compiler, only: compiler_enum, compiler_id_name, match_compiler_type, &
                          id_unknown, id_gcc, id_f95, id_caf, &
                          id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows, &
                          id_intel_llvm_nix, id_intel_llvm_windows, id_intel_llvm_unknown, &
                          id_pgi, id_nvhpc, id_nag, id_flang, id_flang_new, id_f18, &
                          id_ibmxl, id_cray, id_lahey, id_lfortran, id_all
    use fpm_strings, only: string_t, lower, operator(==), split
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat
    use fpm_toml, only: get_value, len, serializable_t, set_value, set_string, set_list, add_table, &
                        get_list
    implicit none
    private
    
    public :: feature_collection_t, new_collection, get_default_features, get_default_features_as_features, default_debug_feature, default_release_feature
    
    !> Feature configuration data
    type, public, extends(serializable_t) :: feature_collection_t
        
        ! Features shared by all platforms, all compilers
        type(feature_config_t) :: base
        
        ! Features shared by specific platform/compiler configurations
        type(feature_config_t), allocatable :: variants(:)
        
        contains
        
            procedure :: serializable_is_same => feature_collection_same
            procedure :: dump_to_toml        => feature_collection_dump
            procedure :: load_from_toml      => feature_collection_load
                        
            procedure :: push_variant

    end type feature_collection_t

    contains
    
    !> Equality (semantic): base and variants (size + element-wise)
    logical function feature_collection_same(this, that)
        class(feature_collection_t), intent(in) :: this
        class(serializable_t),       intent(in) :: that
        integer :: i

        feature_collection_same = .false.
        select type (other => that)
        type is (feature_collection_t)
            if (.not.(this%base == other%base)) return
            if (allocated(this%variants) .neqv. allocated(other%variants)) return
            if (allocated(this%variants)) then
                if (size(this%variants) /= size(other%variants)) return
                do i = 1, size(this%variants)
                    if (.not.(this%variants(i) == other%variants(i))) return
                end do
            end if
        class default
            return
        end select
        feature_collection_same = .true.
    end function feature_collection_same
        
    !> Serialize: base as a subtable; variants under a "variants" table
    subroutine feature_collection_dump(self, table, error)
        class(feature_collection_t), intent(inout) :: self
        type(toml_table),           intent(inout) :: table
        type(error_t), allocatable, intent(out)   :: error

        type(toml_table), pointer :: ptr_base, ptr_vars, ptr
        integer :: i
        character(len=32) :: key

        ! base
        call add_table(table, "base", ptr_base)
        if (.not. associated(ptr_base)) then
            call fatal_error(error, "feature_collection_t: cannot create 'base' table"); return
        end if
        call self%base%dump_to_toml(ptr_base, error); if (allocated(error)) return

        ! variants (optional)
        if (allocated(self%variants)) then
            call add_table(table, "variants", ptr_vars)
            if (.not. associated(ptr_vars)) then
                call fatal_error(error, "feature_collection_t: cannot create 'variants' table"); return
            end if
            do i = 1, size(self%variants)
                write(key, '("variant_", i0)') i
                call add_table(ptr_vars, trim(key), ptr)
                if (.not. associated(ptr)) then
                    call fatal_error(error, "feature_collection_t: cannot create entry for "//trim(key)); return
                end if
                call self%variants(i)%dump_to_toml(ptr, error); if (allocated(error)) return
            end do
        end if
    end subroutine feature_collection_dump
        
    !> Deserialize: read base; then any number of variants under "variants"
    subroutine feature_collection_load(self, table, error)
        class(feature_collection_t), intent(inout) :: self
        type(toml_table),           intent(inout) :: table
        type(error_t), allocatable, intent(out)   :: error

        type(toml_table), pointer :: ptr_base, ptr_vars, ptr
        type(toml_key),  allocatable :: keys(:)
        integer :: i

        ! base (required)
        call get_value(table, "base", ptr_base)
        if (.not. associated(ptr_base)) then
            call fatal_error(error, "feature_collection_t: missing 'base' table"); return
        end if
        call self%base%load_from_toml(ptr_base, error); if (allocated(error)) return

        ! variants (optional)
        call get_value(table, "variants", ptr_vars)
        if (.not. associated(ptr_vars)) then
            if (allocated(self%variants)) deallocate(self%variants)
            return
        end if

        call ptr_vars%get_keys(keys)
        if (allocated(self%variants)) deallocate(self%variants)
        allocate(self%variants(size(keys)))

        do i = 1, size(keys)
            call get_value(ptr_vars, keys(i), ptr)
            if (.not. associated(ptr)) then
                call fatal_error(error, "feature_collection_t: invalid variant entry '"//keys(i)%key//"'"); return
            end if
            call self%variants(i)%load_from_toml(ptr, error); if (allocated(error)) return
        end do
    end subroutine feature_collection_load
        
    ! helper: append a variant to self%variants
    elemental subroutine push_variant(self,variant)
        class(feature_collection_t), intent(inout) :: self
        type(feature_config_t), intent(in) :: variant
        
        
        type(feature_config_t), allocatable :: tmp(:)
        
        integer :: n
        
        if (.not. allocated(self%variants)) then
            allocate(self%variants(1), source=variant)
        else
            n = size(self%variants)
            allocate(tmp(n+1))
            if (n>0) tmp(1:n) = self%variants
            tmp(n+1) = variant
            call move_alloc(tmp, self%variants)
        end if
    end subroutine push_variant

    logical function is_compiler_key(s)
       character(*), intent(in) :: s
       is_compiler_key = match_compiler_type(s) /= id_unknown
    end function is_compiler_key

    logical function is_os_key(s)
       character(*), intent(in) :: s
       is_os_key = match_os_type(s) /= OS_UNKNOWN
    end function is_os_key

    !> Initialize feature collection from manifest features table
    !> Supports flexible configurations like:
    !> [features]
    !> name.os.compiler.* = ...  # specific compiler + OS
    !> name.compiler.* = ...     # all OS, specific compiler  
    !> name.os.* = ...           # specific OS, all compilers
    !> name.* = ...              # base feature (all OS, all compilers)
    subroutine new_collection(self, table, error)
        type(feature_collection_t), intent(out) :: self
        type(toml_table), intent(inout) :: table
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: keys(:)
        type(toml_table), pointer :: node
        character(len=:), allocatable :: key_str, base_name, os_name, compiler_name
        character(len=:), allocatable :: remaining_key
        integer :: i, stat
        integer :: os_type, compiler_type
        type(feature_config_t) :: feature_variant
        logical :: is_base_feature

        ! Get all keys from the features table
        call table%get_keys(keys)
        if (size(keys) == 0) return

        ! Initialize base feature with defaults
        self%base%platform%compiler = id_all
        self%base%platform%os_type = OS_ALL

        do i = 1, size(keys)
            key_str = keys(i)%key
            
            ! Parse the key to extract base name, OS, compiler, and feature type
            call parse_feature_key(key_str, base_name, os_name, compiler_name, remaining_key, is_base_feature)
            
            ! Get the feature configuration table
            call get_value(table, key_str, node, stat=stat)
            if (stat /= toml_stat%success) then
                call fatal_error(error, "Could not retrieve feature '"//key_str//"' table")
                return
            end if

            ! Create feature configuration
            call new_feature(feature_variant, node, error=error)
            if (allocated(error)) return

            ! Set the name to base name (without OS/compiler suffixes)
            feature_variant%name = base_name

            ! Set platform constraints based on parsed key
            if (is_base_feature) then
                ! This is a base feature - merge with existing base
                call merge_feature_configs(self%base, feature_variant, error)
                if (allocated(error)) return
            else
                ! This is a variant - set platform constraints
                if (len(os_name) > 0) then
                    os_type = match_os_type(os_name)
                    if (os_type == OS_UNKNOWN) then
                        call fatal_error(error, "Unknown OS type: "//os_name)
                        return
                    end if
                    feature_variant%platform%os_type = os_type
                else
                    feature_variant%platform%os_type = OS_ALL
                end if

                if (len(compiler_name) > 0) then
                    compiler_type = match_compiler_type(compiler_name)
                    if (compiler_type == id_unknown) then
                        call fatal_error(error, "Unknown compiler type: "//compiler_name)
                        return
                    end if
                    feature_variant%platform%compiler = compiler_type
                else
                    feature_variant%platform%compiler = id_all
                end if

                ! Add to variants
                call self%push_variant(feature_variant)
            end if
        end do

    end subroutine new_collection

    !> Parse a feature key like "name.os.compiler.field" into components
    subroutine parse_feature_key(key_str, base_name, os_name, compiler_name, remaining_key, is_base_feature)
        character(len=*), intent(in) :: key_str
        character(len=:), allocatable, intent(out) :: base_name, os_name, compiler_name, remaining_key
        logical, intent(out) :: is_base_feature

        character(len=:), allocatable :: parts(:)
        integer :: n_parts, i
        logical :: found_os, found_compiler

        ! Split key by dots
        call split(key_str, parts, '.')
        n_parts = size(parts)
        
        if (n_parts == 1) then
            ! Simple case: just "name"
            base_name = parts(1)
            os_name = ""
            compiler_name = ""
            remaining_key = ""
            is_base_feature = .true.
            return
        end if

        ! First part is always the base name
        base_name = parts(1)
        os_name = ""
        compiler_name = ""
        remaining_key = ""
        found_os = .false.
        found_compiler = .false.
        is_base_feature = .false.

        ! Check remaining parts for OS and compiler
        do i = 2, n_parts
            if (.not. found_os .and. is_os_key(parts(i))) then
                os_name = parts(i)
                found_os = .true.
            else if (.not. found_compiler .and. is_compiler_key(parts(i))) then
                compiler_name = parts(i) 
                found_compiler = .true.
            else
                ! This is part of the feature specification
                if (len(remaining_key) == 0) then
                    remaining_key = parts(i)
                else
                    remaining_key = remaining_key // "." // parts(i)
                end if
            end if
        end do

        ! If no OS or compiler constraints found, treat as base feature
        if (.not. found_os .and. .not. found_compiler) then
            is_base_feature = .true.
        end if

    end subroutine parse_feature_key


    !> Merge two feature configurations (for base feature merging)
    subroutine merge_feature_configs(target, source, error)
        type(feature_config_t), intent(inout) :: target
        type(feature_config_t), intent(in) :: source  
        type(error_t), allocatable, intent(out) :: error
        
        ! Currently no errors are generated in this routine
        ! The error parameter is for future extensibility

        ! Merge simple fields
        if (allocated(source%description) .and. .not. allocated(target%description)) then
            target%description = source%description
        end if
        
        if (allocated(source%flags) .and. .not. allocated(target%flags)) then
            target%flags = source%flags
        end if
        
        if (allocated(source%c_flags) .and. .not. allocated(target%c_flags)) then
            target%c_flags = source%c_flags
        end if
        
        if (allocated(source%cxx_flags) .and. .not. allocated(target%cxx_flags)) then
            target%cxx_flags = source%cxx_flags
        end if
        
        if (allocated(source%link_time_flags) .and. .not. allocated(target%link_time_flags)) then
            target%link_time_flags = source%link_time_flags
        end if

        ! Merge build config
        target%build = source%build
        
        ! Merge install config  
        target%install = source%install
        
        ! Merge fortran config
        target%fortran = source%fortran

        ! Merge library config
        if (allocated(source%library) .and. .not. allocated(target%library)) then
            allocate(target%library)
            target%library = source%library
        end if

        ! TODO: Merge arrays (executable, dependency, etc.) - for now just take from source
        if (allocated(source%executable) .and. .not. allocated(target%executable)) then
            allocate(target%executable(size(source%executable)))
            target%executable = source%executable
        end if

        if (allocated(source%dependency) .and. .not. allocated(target%dependency)) then
            allocate(target%dependency(size(source%dependency)))  
            target%dependency = source%dependency
        end if

        if (allocated(source%dev_dependency) .and. .not. allocated(target%dev_dependency)) then
            allocate(target%dev_dependency(size(source%dev_dependency)))
            target%dev_dependency = source%dev_dependency
        end if

        if (allocated(source%example) .and. .not. allocated(target%example)) then
            allocate(target%example(size(source%example)))
            target%example = source%example
        end if

        if (allocated(source%test) .and. .not. allocated(target%test)) then
            allocate(target%test(size(source%test)))
            target%test = source%test
        end if

        if (allocated(source%preprocess) .and. .not. allocated(target%preprocess)) then
            allocate(target%preprocess(size(source%preprocess)))
            target%preprocess = source%preprocess
        end if

        ! Merge metapackage config
        target%meta = source%meta

    end subroutine merge_feature_configs

    !> Create default debug feature collection  
    function default_debug_feature() result(collection)
        type(feature_collection_t) :: collection
        
        ! Initialize base feature with debug settings
        collection%base%name              = 'debug'
        collection%base%platform%compiler = id_all
        collection%base%platform%os_type  = OS_ALL
        collection%base%default           = .true.
        
        ! Add debug variants for different compilers
        call collection%push_variant(default_variant('debug', id_caf, OS_ALL, &
            ' -Wall -Wextra -Wimplicit-interface -Wno-external-argument-mismatch&
            & -fPIC -fmax-errors=1 -g -fcheck=bounds&
            & -fcheck=array-temps -fbacktrace'))
            
        call collection%push_variant(default_variant('debug', id_gcc, OS_ALL, &
            ' -Wall -Wextra -Wimplicit-interface -Wno-external-argument-mismatch&
            & -fPIC -fmax-errors=1 -g -fcheck=bounds&
            & -fcheck=array-temps -fbacktrace -fcoarray=single'))
            
        call collection%push_variant(default_variant('debug', id_f95, OS_ALL, &
            ' -Wall -Wextra -Wimplicit-interface -Wno-external-argument-mismatch&
            & -fPIC -fmax-errors=1 -g -fcheck=bounds&
            & -fcheck=array-temps -Wno-maybe-uninitialized -Wno-uninitialized -fbacktrace'))
            
        call collection%push_variant(default_variant('debug', id_nvhpc, OS_ALL, &
            ' -Minform=inform -Mbackslash -g -Mbounds -Mchkptr -Mchkstk -traceback'))
            
        call collection%push_variant(default_variant('debug', id_intel_classic_nix, OS_ALL, &
            ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback'))
            
        call collection%push_variant(default_variant('debug', id_intel_classic_nix, OS_WINDOWS, &
            ' /warn:all /check:all /error-limit:1&
            & /Od /Z7 /assume:byterecl /traceback'))
            
        call collection%push_variant(default_variant('debug', id_intel_llvm_nix, OS_ALL, &
            ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback'))
            
        call collection%push_variant(default_variant('debug', id_intel_llvm_nix, OS_WINDOWS, &
            ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl'))
            
        call collection%push_variant(default_variant('debug', id_lfortran, OS_ALL, ''))
            
    end function default_debug_feature

    !> Create default release feature collection
    function default_release_feature() result(collection)
        type(feature_collection_t) :: collection
        
        ! Initialize base feature with release settings
        collection%base%name = 'release'
        collection%base%platform%compiler = id_all
        collection%base%platform%os_type = OS_ALL
        collection%base%default = .true.
        
        ! Add release variants for different compilers
        call collection%push_variant(default_variant('release-caf', id_caf, OS_ALL, &
            ' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops'))
            
        call collection%push_variant(default_variant('release-gfortran', id_gcc, OS_ALL, &
            ' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops -fcoarray=single'))
            
        call collection%push_variant(default_variant('release-f95', id_f95, OS_ALL, &
            ' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -ffast-math -funroll-loops'))
            
        call collection%push_variant(default_variant('release-nvfortran', id_nvhpc, OS_ALL, &
            ' -Mbackslash'))
            
        call collection%push_variant(default_variant('release-ifort', id_intel_classic_nix, OS_ALL, &
            ' -fp-model precise -pc64 -align all -error-limit 1 -reentrancy&
            & threaded -nogen-interfaces -assume byterecl'))
            
        call collection%push_variant(default_variant('release-ifort-windows', id_intel_classic_nix, OS_WINDOWS, &
            ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
            & /nogen-interfaces /assume:byterecl'))
            
        call collection%push_variant(default_variant('release-ifx', id_intel_llvm_nix, OS_ALL, &
            ' -fp-model=precise -pc64 -align all -error-limit 1 -reentrancy&
            & threaded -nogen-interfaces -assume byterecl'))
            
        call collection%push_variant(default_variant('release-ifx-windows', id_intel_llvm_nix, OS_WINDOWS, &
            ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
            & /nogen-interfaces /assume:byterecl'))
            
        call collection%push_variant(default_variant('release-nagfor', id_nag, OS_ALL, &
            ' -O4 -coarray=single -PIC'))
            
        call collection%push_variant(default_variant('release-lfortran', id_lfortran, OS_ALL, &
            ' flag_lfortran_opt'))
            
    end function default_release_feature

    !> Get default feature collections (debug and release)
    subroutine get_default_features(collections, error)
        
        !> Feature collections array to populate (debug and release)
        type(feature_collection_t), allocatable, intent(out) :: collections(:)
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        ! Allocate array for debug and release collections
        allocate(collections(2))
        
        ! Create debug and release collections
        collections(1) = default_debug_feature()
        collections(2) = default_release_feature()
        
    end subroutine get_default_features

    !> Convert feature collections to individual features (for backward compatibility)
    subroutine get_default_features_as_features(features, error)
        
        !> Features array to populate (backward compatible)
        type(feature_config_t), allocatable, intent(out) :: features(:)
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        type(feature_collection_t), allocatable :: collections(:)
        integer :: total_features, ifeature, icol, ivar
        
        ! Get the feature collections
        call get_default_features(collections, error)
        if (allocated(error)) return
        
        ! Count total features needed
        total_features = 0
        do icol = 1, size(collections)
            total_features = total_features + 1  ! base feature
            if (allocated(collections(icol)%variants)) then
                total_features = total_features + size(collections(icol)%variants)
            end if
        end do
        
        ! Allocate features array
        allocate(features(total_features))
        
        ! Copy features from collections
        ifeature = 1
        do icol = 1, size(collections)
            ! Add base feature
            features(ifeature) = collections(icol)%base
            ifeature = ifeature + 1
            
            ! Add variants
            if (allocated(collections(icol)%variants)) then
                do ivar = 1, size(collections(icol)%variants)
                    features(ifeature) = collections(icol)%variants(ivar)
                    ifeature = ifeature + 1
                end do
            end if
        end do
        
    end subroutine get_default_features_as_features

    !> Helper to create a feature variant
    function default_variant(name, compiler_id, os_type, flags) result(feature)
        character(len=*), intent(in) :: name
        integer(compiler_enum), intent(in) :: compiler_id
        integer, intent(in) :: os_type
        character(len=*), intent(in) :: flags
        type(feature_config_t) :: feature

        feature%name = name
        feature%platform%compiler = compiler_id
        feature%platform%os_type = os_type
        feature%flags = flags
        feature%default = .true.  ! These are built-in features
    end function default_variant

end module fpm_manifest_feature_collection
