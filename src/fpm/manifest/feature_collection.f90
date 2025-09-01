
module fpm_manifest_feature_collection
    use fpm_manifest_feature, only: feature_config_t, new_feature
    use fpm_manifest_platform, only: platform_config_t
    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_environment, only: OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN, OS_SOLARIS, &
                             OS_FREEBSD, OS_OPENBSD, OS_ALL, OS_NAME, match_os_type
    use fpm_compiler, only: compiler_enum, compiler_id_name, match_compiler_type, &
                          id_unknown, id_gcc, id_f95, id_caf, &
                          id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows, &
                          id_intel_llvm_nix, id_intel_llvm_windows, id_intel_llvm_unknown, &
                          id_pgi, id_nvhpc, id_nag, id_flang, id_flang_new, id_f18, &
                          id_ibmxl, id_cray, id_lahey, id_lfortran, id_all
    use fpm_strings, only: string_t, lower, operator(==), split, str
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat
    use fpm_toml, only: get_value, len, serializable_t, set_value, set_string, set_list, add_table, &
                        get_list
    implicit none
    private
    
    public :: new_collections, get_default_features, &
              get_default_features_as_features, default_debug_feature, default_release_feature
    
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
            procedure :: extract_for_target
            procedure :: check => check_collection

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

    !> Initialize multiple feature collections from manifest features table
    subroutine new_collections(collections, table, error)
        type(feature_collection_t), allocatable, intent(out) :: collections(:)
        type(toml_table), intent(inout) :: table
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: keys(:)
        type(toml_table), pointer :: feature_table
        integer :: i, stat
        
        ! Get all top-level feature names from the features table
        call table%get_keys(keys)

        if (size(keys) == 0) then
            ! No features defined, return default collections
            call get_default_features(collections, error)
            return
        end if

        ! Create one collection per top-level feature name
        allocate(collections(size(keys)))
        
        do i = 1, size(keys)
            ! Get the subtable for this feature
            call get_value(table, keys(i)%key, feature_table, stat=stat)
            if (stat /= toml_stat%success) then
                call fatal_error(error, "Could not retrieve feature table for '"//keys(i)%key//"'")
                return
            end if
            
            ! Create collection from this feature's subtable
            call new_collection_from_subtable(collections(i), feature_table, keys(i)%key, error)
            if (allocated(error)) return
            
        end do
                

        
    end subroutine new_collections
    
    !> Create a feature collection from a TOML subtable by traversing the hierarchy
    !> Supports flexible configurations like:
    !> [features]
    !> name.os.compiler.* = ...  # specific compiler + OS
    !> name.compiler.* = ...     # all OS, specific compiler  
    !> name.os.* = ...           # specific OS, all compilers
    !> name.* = ...              # base feature (all OS, all compilers)
    subroutine new_collection_from_subtable(self, table, name, error)
        type(feature_collection_t), intent(out) :: self
        type(toml_table), intent(inout) :: table
        character(*), intent(in) :: name
        type(error_t), allocatable, intent(out) :: error
        
        ! Initialize base feature
        self%base%name = name
        self%base%platform%compiler = id_all
        self%base%platform%os_type = OS_ALL
        
        ! Traverse the table hierarchy to find variants
        call traverse_feature_table(self, table, name, OS_ALL, id_all, error)
        
        ! Check collection
        call self%check(error)        
        
    end subroutine new_collection_from_subtable
    
    !> Recursively traverse a feature table to find variants
    recursive subroutine traverse_feature_table(collection, table, feature_name, os_constraint, compiler_constraint, error)
        type(feature_collection_t), intent(inout) :: collection
        type(toml_table), intent(inout) :: table
        character(*), intent(in) :: feature_name
        integer, intent(in) :: os_constraint, compiler_constraint
        type(error_t), allocatable, intent(out) :: error
        
        type(toml_key), allocatable :: keys(:)
        type(toml_table), pointer :: subtable
        character(len=:), allocatable :: value_str
        integer :: i, stat, os_type, compiler_type
        type(feature_config_t) :: feature_variant
        logical :: has_feature_data
        
        call table%get_keys(keys)
        has_feature_data = .false.
        
        do i = 1, size(keys)
            ! Check if this key is an OS name
            os_type = match_os_type(keys(i)%key)
            if (os_type /= OS_UNKNOWN) then
                ! This is an OS constraint - get subtable and recurse
                call get_value(table, keys(i)%key, subtable, stat=stat)
                if (stat == toml_stat%success) then
                    call traverse_feature_table(collection, subtable, feature_name, os_type, compiler_constraint, error)
                    if (allocated(error)) return
                end if
                cycle
            end if
            
            ! Check if this key is a compiler name  
            compiler_type = match_compiler_type(keys(i)%key)
            if (compiler_type /= id_unknown) then
                ! This is a compiler constraint - get subtable and recurse
                call get_value(table, keys(i)%key, subtable, stat=stat)
                if (stat == toml_stat%success) then
                    call traverse_feature_table(collection, subtable, feature_name, os_constraint, compiler_type, error)
                    if (allocated(error)) return
                end if
                cycle
            end if
            
            ! Check if this looks like it should be an OS or compiler but isn't valid
            if (is_potential_platform_key(keys(i)%key)) then
                call fatal_error(error, "Key '"//keys(i)%key//"' is not allowed in feature table")
                return
            end if
            
            ! This is a feature specification (like "flags" or "preprocess")
            has_feature_data = .true.
        end do
        
        ! If we found feature data at this level, create a feature config
        if (has_feature_data) then
            ! Create feature from the current table
            call new_feature(feature_variant, table, error=error, name=feature_name)
            if (allocated(error)) return
            
            feature_variant%platform%os_type = os_constraint
            feature_variant%platform%compiler = compiler_constraint
            
            if (os_constraint == OS_ALL .and. compiler_constraint == id_all) then
                ! This is a base feature specification
                call merge_feature_configs(collection%base, feature_variant, error)
                if (allocated(error)) return
            else
                ! This is a constrained variant
                call collection%push_variant(feature_variant)
            end if
        end if
        
    end subroutine traverse_feature_table
    
    !> Check if a key looks like it should be a platform constraint but isn't valid
    logical function is_potential_platform_key(key)
        character(*), intent(in) :: key
        
        ! Simple heuristic: if it's not a known feature configuration key,
        ! and it looks like it could be a platform identifier, flag it as invalid
        
        ! Known feature configuration keys
        if (key == "flags" .or. key == "preprocess" .or. key == "link" .or. &
            key == "include-dir" .or. key == "source-dir" .or. key == "dependencies") then
            is_potential_platform_key = .false.
            return
        end if
        
        ! If it contains common OS or compiler-like patterns, it might be an invalid platform key
        if (index(key, "os") > 0 .or. index(key, "compiler") > 0 .or. &
            index(key, "win") > 0 .or. index(key, "linux") > 0 .or. &
            index(key, "mac") > 0 .or. index(key, "fort") > 0 .or. &
            index(key, "gcc") > 0 .or. index(key, "intel") > 0) then
            is_potential_platform_key = .true.
            return
        end if
        
        is_potential_platform_key = .false.
    end function is_potential_platform_key

    !> Merge two feature configurations (for base feature merging)
    subroutine merge_feature_configs(target, source, error)
        type(feature_config_t), intent(inout) :: target
        type(feature_config_t), intent(in) :: source  
        type(error_t), allocatable, intent(out) :: error
        
        ! Currently no errors are generated in this routine
        ! The error parameter is for future extensibility

        ! Merge simple fields - description is taken from source if target doesn't have one
        if (allocated(source%description) .and. .not. allocated(target%description)) then
            target%description = source%description
        end if
        
        ! For flags, we APPEND/ADD them together
        if (allocated(source%flags)) then
            if (allocated(target%flags)) then
                target%flags = trim(target%flags) // " " // trim(source%flags)
            else
                target%flags = source%flags
            end if
        end if
        
        if (allocated(source%c_flags)) then
            if (allocated(target%c_flags)) then
                target%c_flags = trim(target%c_flags) // " " // trim(source%c_flags)
            else
                target%c_flags = source%c_flags
            end if
        end if
        
        if (allocated(source%cxx_flags)) then
            if (allocated(target%cxx_flags)) then
                target%cxx_flags = trim(target%cxx_flags) // " " // trim(source%cxx_flags)
            else
                target%cxx_flags = source%cxx_flags
            end if
        end if
        
        if (allocated(source%link_time_flags)) then
            if (allocated(target%link_time_flags)) then
                target%link_time_flags = trim(target%link_time_flags) // " " // trim(source%link_time_flags)
            else
                target%link_time_flags = source%link_time_flags
            end if
        end if

        ! Merge build config
        if (allocated(source%build) .and. .not. allocated(target%build)) then
            allocate(target%build)
            target%build = source%build
        end if
        
        ! Merge install config  
        if (allocated(source%install) .and. .not. allocated(target%install)) then
            allocate(target%install)
            target%install = source%install
        end if
        
        ! Merge fortran config
        if (allocated(source%fortran) .and. .not. allocated(target%fortran)) then
            allocate(target%fortran)
            target%fortran = source%fortran
        end if

        ! Merge library config
        if (allocated(source%library) .and. .not. allocated(target%library)) then
            allocate(target%library)
            target%library = source%library
        end if

        ! Merge arrays by appending source to target
        call merge_executable_arrays(target%executable, source%executable)
        call merge_dependency_arrays(target%dependency, source%dependency)
        call merge_dependency_arrays(target%dev_dependency, source%dev_dependency)

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

    !> Merge executable arrays by appending source to target
    subroutine merge_executable_arrays(target, source)
        use fpm_manifest_executable, only: executable_config_t
        type(executable_config_t), allocatable, intent(inout) :: target(:)
        type(executable_config_t), allocatable, intent(in) :: source(:)
        
        type(executable_config_t), allocatable :: temp(:)
        integer :: target_size, source_size
        
        if (.not. allocated(source)) return
        
        source_size = size(source)
        if (source_size == 0) return
        
        if (.not. allocated(target)) then
            allocate(target(source_size))
            target = source
        else
            target_size = size(target)
            allocate(temp(target_size + source_size))
            temp(1:target_size) = target
            temp(target_size+1:target_size+source_size) = source
            call move_alloc(temp, target)
        end if
        
    end subroutine merge_executable_arrays

    !> Merge dependency arrays by appending source to target  
    subroutine merge_dependency_arrays(target, source)
        use fpm_manifest_dependency, only: dependency_config_t
        type(dependency_config_t), allocatable, intent(inout) :: target(:)
        type(dependency_config_t), allocatable, intent(in) :: source(:)
        
        type(dependency_config_t), allocatable :: temp(:)
        integer :: target_size, source_size
        
        if (.not. allocated(source)) return
        
        source_size = size(source)
        if (source_size == 0) return
        
        if (.not. allocated(target)) then
            allocate(target(source_size))
            target = source
        else
            target_size = size(target)
            allocate(temp(target_size + source_size))
            temp(1:target_size) = target
            temp(target_size+1:target_size+source_size) = source
            call move_alloc(temp, target)
        end if
        
    end subroutine merge_dependency_arrays

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


    !> Check that the collection has valid OS/compiler logic and no duplicate variants
    subroutine check_collection(self, error)
        class(feature_collection_t), intent(in) :: self
        type(error_t), allocatable, intent(out) :: error
        
        integer :: i, j
        
        ! Check base feature has valid platform settings
        if (self%base%platform%os_type == OS_UNKNOWN) then
            call fatal_error(error, "Base feature '"//self%base%name//"' has invalid OS type")
            return
        end if
        
        if (self%base%platform%compiler == id_unknown) then
            call fatal_error(error, "Base feature '"//self%base%name//"' has invalid compiler type")
            return
        end if
        
        ! Check all variants have valid platform settings and no duplicates
        if (allocated(self%variants)) then
            do i = 1, size(self%variants)
                ! Validate OS and compiler settings
                if (self%variants(i)%platform%os_type == OS_UNKNOWN) then
                    call fatal_error(error, "Variant "//trim(str(i))//" of feature '"//self%base%name//"' has invalid OS type")
                    return
                end if
                
                if (self%variants(i)%platform%compiler == id_unknown) then
                    call fatal_error(error, "Variant "//trim(str(i))//" of feature '"//self%base%name//"' has invalid compiler type")
                    return
                end if
                
                ! Check that variant name matches base name
                if (allocated(self%variants(i)%name) .and. allocated(self%base%name)) then
                    if (self%variants(i)%name /= self%base%name) then
                        call fatal_error(error, "Variant "//trim(str(i))//" name '"//self%variants(i)%name// &
                                              "' does not match base name '"//self%base%name//"'")
                        return
                    end if
                end if
                
                ! Check for duplicate platforms with other variants (exact match, not compatible match)
                do j = i + 1, size(self%variants)
                    if (self%variants(i)%platform == self%variants(j)%platform) then
                        call fatal_error(error, "Duplicate platform configuration found between variants "// &
                                              trim(str(i))//" and "//trim(str(j))//" of feature '"//self%base%name//"'")
                        return
                    end if
                end do
                
                ! Check that variant doesn't have identical platform to base (which would be redundant)
                if (self%variants(i)%platform == self%base%platform) then
                    call fatal_error(error, "Variant "//trim(str(i))//" of feature '"//self%base%name// &
                                          "' has identical platform as the base feature (redundant)")
                    return
                end if
            end do
        end if
        
    end subroutine check_collection

    !> Extract a merged feature configuration for the given target platform
    function extract_for_target(self, target) result(feature)
        class(feature_collection_t), intent(in) :: self
        type(platform_config_t), intent(in) :: target
        type(feature_config_t) :: feature
        
        integer :: i
        type(error_t), allocatable :: error
        
        ! Start with base feature as foundation
        feature = self%base
        
        ! Apply matching variants on top of base
        if (allocated(self%variants)) then
            do i = 1, size(self%variants)
                if (self%variants(i)%platform%matches(target)) then
                    ! Merge this variant into the feature
                    call merge_feature_configs(feature, self%variants(i), error)
                    if (allocated(error)) then
                        ! If merge fails, just continue with what we have
                        deallocate(error)
                    end if
                end if
            end do
        end if
        
    end function extract_for_target
    
end module fpm_manifest_feature_collection
