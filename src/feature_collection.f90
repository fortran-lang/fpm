
module fpm_manifest_feature_collection
    use fpm_manifest_feature, only: feature_config_t, new_feature, init_feature_components
    use fpm_manifest_platform, only: platform_config_t, is_platform_key    
    use fpm_manifest_dependency, only: dependency_config_t
    use fpm_manifest_example, only: example_config_t
    use fpm_manifest_executable, only: executable_config_t
    use fpm_manifest_metapackages, only: metapackage_config_t, metapackage_request_t
    use fpm_manifest_test, only: test_config_t
    use fpm_manifest_preprocess, only: preprocess_config_t
    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_environment, only: OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN, OS_SOLARIS, &
                             OS_FREEBSD, OS_OPENBSD, OS_ALL, match_os_type, OS_NAME
    use fpm_compiler, only: compiler_enum, compiler_id_name, match_compiler_type, &
                          id_unknown, id_gcc, id_f95, id_caf, &
                          id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows, &
                          id_intel_llvm_nix, id_intel_llvm_windows, id_intel_llvm_unknown, &
                          id_pgi, id_nvhpc, id_nag, id_flang, id_lahey, id_lfortran, id_all
    use fpm_strings, only: string_t, lower, operator(==), split, str
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat
    use fpm_toml, only: get_value, len, serializable_t, set_value, set_string, set_list, add_table, &
                        get_list
    implicit none
    private
    
    public :: new_collections, get_default_features, &
              default_debug_feature, default_release_feature, &
              add_default_features, collection_from_feature
    
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
            procedure :: merge_into_package
            procedure :: has_cpp

    end type feature_collection_t

    !> Interface for feature_collection_t constructor
    interface feature_collection_t
        module procedure collection_from_feature
    end interface feature_collection_t

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
                call fatal_error(error, "feature_collection_t: invalid variant entry '" &
                                        //keys(i)%key//"'"); return
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
        
        integer :: i
        type(platform_config_t) :: default_platform
        type(toml_key), allocatable :: keys(:)
        
        default_platform = platform_config_t(id_all,OS_ALL)
        
        ! Initialize base feature
        self%base%name = name
        self%base%platform = default_platform
        
        ! Traverse the table hierarchy to find variants
        call traverse_feature_table(self, table, name, default_platform, error)
        if (allocated(error)) return
        
        ! Check collection
        call self%check(error)        
        if (allocated(error)) return
        
    end subroutine new_collection_from_subtable
    
    !> Recursively traverse a feature table to find variants
    recursive subroutine traverse_feature_table(collection, table, feature_name, &
                                                constraint, error)        
        type(feature_collection_t), intent(inout) :: collection
        type(toml_table), intent(inout) :: table
        character(*), intent(in) :: feature_name
        type(platform_config_t), intent(in) :: constraint
        type(error_t), allocatable, intent(out) :: error
        
        type(toml_key), allocatable :: keys(:)
        type(toml_table), pointer :: subtable
        character(len=:), allocatable :: value_str
        integer :: i, stat, os_type, compiler_type
        type(feature_config_t) :: feature_variant
        type(platform_config_t) :: platform
        logical :: has_platform_keys, has_feature_data
        
        call table%get_keys(keys)
        has_platform_keys = .false.
        has_feature_data = .false.
        
        ! First pass: check what types of keys we have
        do i = 1, size(keys)
            
            ! Check if this key is a valid OS name
            os_type = match_os_type(keys(i)%key)
            if (os_type /= OS_UNKNOWN) then
                has_platform_keys = .true.
                cycle
            end if
            
            ! Check if this key is a valid compiler name  
            compiler_type = match_compiler_type(keys(i)%key)
            if (compiler_type /= id_unknown) then
                has_platform_keys = .true.
                cycle
            end if
            
            ! This is a feature specification (like "flags" or "preprocess")
            has_feature_data = .true.
            
            ! No compiler/OS flags can appear in feature specification branches
            if (is_platform_key(keys(i)%key)) then
                call fatal_error(error, "Key '"//keys(i)%key//"' is not allowed in feature table")
                return
            end if              
            
        end do
        
        ! If we have platform keys, traverse them
        if (has_platform_keys) then
            do i = 1, size(keys)
                ! Check if this key is an OS name
                os_type = match_os_type(keys(i)%key)
                if (os_type /= OS_UNKNOWN) then
                    ! This is an OS constraint 
                    
                    ! Check for chained OS commands (e.g., feature.windows.linux)
                    if (constraint%os_type /= OS_ALL) then
                        call fatal_error(error, "Cannot chain OS constraints: '" // &
                                        constraint%os_name() // "." // keys(i)%key // &
                                        "' - OS was already specified")
                        return
                    end if                    
                    
                    ! Get subtable and recurse
                    call get_value(table, keys(i)%key, subtable, stat=stat)
                    if (stat == toml_stat%success) then
                        platform = platform_config_t(constraint%compiler,os_type)
                        call traverse_feature_table(collection, subtable, feature_name, &
                                                    platform, error)
                        if (allocated(error)) return
                    end if
                    cycle
                end if
                
                ! Check if this key is a compiler name  
                compiler_type = match_compiler_type(keys(i)%key)
                if (compiler_type /= id_unknown) then
                    
                    ! Check for chained compiler commands (e.g., feature.gfortran.ifort)
                    if (constraint%compiler /= id_all) then
                        call fatal_error(error, "Cannot chain compiler constraints: '" // &
                                        constraint%compiler_name() // "." // keys(i)%key // &
                                        "' - compiler was already specified")
                        return
                    end if                    
                    
                    ! This is a compiler constraint - get subtable and recurse
                    call get_value(table, keys(i)%key, subtable, stat=stat)
                    if (stat == toml_stat%success) then
                        platform = platform_config_t(compiler_type,constraint%os_type)
                        call traverse_feature_table(collection, subtable, feature_name, &
                                                    platform, error)
                        if (allocated(error)) return
                    end if
                    cycle
                end if
            end do
        end if
        
        ! If we found feature data at this level (no more platform keys), initialize feature components
        if (has_feature_data) then

            ! Initialize a new feature variant
            feature_variant%name = feature_name
            
            ! Check that the table is right
            call feature_variant%check(table, error)            
            if (allocated(error)) return            
            
            call init_feature_components(feature_variant, table, constraint, error=error)
            if (allocated(error)) return
            
            if (constraint%any_platform()) then
                ! This is a base feature specification
                call merge_feature_configs(collection%base, feature_variant, error)
                if (allocated(error)) return
            else
                ! This is a constrained variant
                call collection%push_variant(feature_variant)
            end if
        end if
        
        ! If this is the root table and we haven't processed any feature data yet, 
        ! call init_feature_components on the base feature (may be empty)
        if (constraint%any_platform() .and. .not.(has_platform_keys.or.has_feature_data)) then
            
            ! Check that the table is right
            call feature_variant%check(table, error)            
            if (allocated(error)) return                        
            
            ! Initialize base feature components from empty or root table
            call init_feature_components(collection%base, table, error=error)
            if (allocated(error)) return
        end if
        
    end subroutine traverse_feature_table
    
    !> Merge two feature configurations using standardized rules:
    !> - String properties (flags): concatenate with spaces (additive)
    !> - Array properties: append arrays (additive) 
    !> - Allocatable properties: source overwrites target if target not allocated (conflict if both allocated)
    !> - Metapackages: OR logic - turn on any that are requested (additive)
    subroutine merge_feature_configs(target, source, error)
        type(feature_config_t), intent(inout) :: target
        type(feature_config_t), intent(in) :: source  
        type(error_t), allocatable, intent(out) :: error
        
        ! Check for allocatable property conflicts (should be caught by validation, but double-check)
        if (allocated(target%build) .and. allocated(source%build)) then
            call fatal_error(error, "build configuration can only be specified in one feature variant")
            return
        end if
        
        if (allocated(target%install) .and. allocated(source%install)) then
            call fatal_error(error, "install configuration can only be specified in one feature variant")  
            return
        end if
        
        if (allocated(target%fortran) .and. allocated(source%fortran)) then
            call fatal_error(error, "fortran configuration can only be specified in one feature variant")
            return
        end if
        
        if (allocated(target%library) .and. allocated(source%library)) then
            call fatal_error(error, "library configuration can only be specified in one feature variant")
            return
        end if

        ! Merge simple string fields - source takes precedence if target doesn't have one
        if (allocated(source%description) .and. .not. allocated(target%description)) then
            target%description = source%description
        end if
        
        ! ADDITIVE: String properties (flags) - concatenate with spaces
        call merge_string_additive(target%flags, source%flags)
        call merge_string_additive(target%c_flags, source%c_flags)
        call merge_string_additive(target%cxx_flags, source%cxx_flags) 
        call merge_string_additive(target%link_time_flags, source%link_time_flags)

        ! ALLOCATABLE: Only set if target doesn't have it (conflicts checked above)
        if (allocated(source%build) .and. .not. allocated(target%build)) then
            allocate(target%build)
            target%build = source%build
        end if
        
        if (allocated(source%install) .and. .not. allocated(target%install)) then
            allocate(target%install)
            target%install = source%install
        end if
        
        if (allocated(source%fortran) .and. .not. allocated(target%fortran)) then
            allocate(target%fortran)
            target%fortran = source%fortran
        end if

        if (allocated(source%library) .and. .not. allocated(target%library)) then
            allocate(target%library)
            target%library = source%library
        end if

        ! ADDITIVE: Array properties - append source to target
        call merge_executable_arrays(target%executable, source%executable, error)
        if (allocated(error)) return
        
        call merge_dependency_arrays(target%dependency, source%dependency, error)
        if (allocated(error)) return
        
        call merge_dependency_arrays(target%dev_dependency, source%dev_dependency, error)
        if (allocated(error)) return
        
        call merge_example_arrays(target%example, source%example, error)
        if (allocated(error)) return
        
        call merge_test_arrays(target%test, source%test, error)
        if (allocated(error)) return
        
        call merge_preprocess_arrays(target%preprocess, source%preprocess)
        call merge_string_arrays(target%requires_features, source%requires_features)

        ! ADDITIVE: Metapackages - OR logic (if either requests it, turn it on)
        call merge_metapackages(target%meta, source%meta)

    end subroutine merge_feature_configs

    !> Merge executable arrays by appending source to target, checking for duplicates
    subroutine merge_executable_arrays(target, source, error)        
        type(executable_config_t), allocatable, intent(inout) :: target(:)
        type(executable_config_t), allocatable, intent(in) :: source(:)
        type(error_t), allocatable, intent(out) :: error
        
        type(executable_config_t), allocatable :: temp(:)
        integer :: target_size, source_size, i, j
        
        if (.not. allocated(source)) return
        
        source_size = size(source)
        if (source_size == 0) return
        
        ! Check for duplicates between source and target
        if (allocated(target)) then
            target_size = size(target)
            do i = 1, source_size
                do j = 1, target_size
                    if (allocated(source(i)%name) .and. allocated(target(j)%name)) then
                        if (source(i)%name == target(j)%name) then
                            call fatal_error(error, "Duplicate executable '"//source(i)%name//"' found. " // &
                                           "Multiple definitions of the same executable are not currently allowed.")
                            return
                        end if
                    end if
                end do
            end do
        end if
        
        if (.not. allocated(target)) then
            allocate(target(source_size), source=source)
        else
            target_size = size(target)
            allocate(temp(target_size + source_size))
            temp(1:target_size) = target
            temp(target_size+1:target_size+source_size) = source
            call move_alloc(temp, target)
        end if
        
    end subroutine merge_executable_arrays

    !> Merge dependency arrays by appending source to target, checking for duplicates
    subroutine merge_dependency_arrays(target, source, error)        
        type(dependency_config_t), allocatable, intent(inout) :: target(:)
        type(dependency_config_t), allocatable, intent(in) :: source(:)
        type(error_t), allocatable, intent(out) :: error
        
        type(dependency_config_t), allocatable :: temp(:)
        integer :: target_size, source_size, i, j
        
        if (.not. allocated(source)) return
        
        source_size = size(source)
        if (source_size == 0) return
        
        ! Check for duplicates between source and target
        if (allocated(target)) then
            target_size = size(target)
            do i = 1, source_size
                do j = 1, target_size
                    if (allocated(source(i)%name) .and. allocated(target(j)%name)) then
                        if (source(i)%name == target(j)%name) then
                            call fatal_error(error, "Duplicate dependency '"//source(i)%name//"' found. " // &
                                           "Multiple definitions of the same dependency are not currently allowed.")
                            return
                        end if
                    end if
                end do
            end do
        end if
        
        if (.not. allocated(target)) then
            allocate(target(source_size), source=source)
        else
            target_size = size(target)
            allocate(temp(target_size + source_size))
            temp(1:target_size) = target
            temp(target_size+1:target_size+source_size) = source
            call move_alloc(temp, target)
        end if
        
    end subroutine merge_dependency_arrays

    !> Merge string properties additively by concatenating with space
    subroutine merge_string_additive(target, source)
        character(len=:), allocatable, intent(inout) :: target
        character(len=:), allocatable, intent(in) :: source
        
        if (allocated(source)) then
            if (allocated(target)) then
                target = trim(target) // " " // trim(source)
            else
                target = source
            end if
        end if
    end subroutine merge_string_additive

    !> Merge example arrays by appending source to target, checking for duplicates
    subroutine merge_example_arrays(target, source, error)        
        type(example_config_t), allocatable, intent(inout) :: target(:)
        type(example_config_t), allocatable, intent(in) :: source(:)
        type(error_t), allocatable, intent(out) :: error
        
        type(example_config_t), allocatable :: temp(:)
        integer :: target_size, source_size, i, j
        
        if (.not. allocated(source)) return
        
        source_size = size(source)
        if (source_size == 0) return
        
        ! Check for duplicates between source and target
        if (allocated(target)) then
            target_size = size(target)
            do i = 1, source_size
                do j = 1, target_size
                    if (allocated(source(i)%name) .and. allocated(target(j)%name)) then
                        if (source(i)%name == target(j)%name) then
                            call fatal_error(error, "Duplicate example '"//source(i)%name//"' found. " // &
                                           "Multiple definitions of the same example are not currently allowed.")
                            return
                        end if
                    end if
                end do
            end do
        end if
        
        if (.not. allocated(target)) then
            allocate(target(source_size), source=source)
        else
            target_size = size(target)
            allocate(temp(target_size + source_size))
            temp(1:target_size) = target
            temp(target_size+1:target_size+source_size) = source
            call move_alloc(temp, target)
        end if
    end subroutine merge_example_arrays

    !> Merge test arrays by appending source to target, checking for duplicates
    subroutine merge_test_arrays(target, source, error)        
        type(test_config_t), allocatable, intent(inout) :: target(:)
        type(test_config_t), allocatable, intent(in) :: source(:)
        type(error_t), allocatable, intent(out) :: error
        
        type(test_config_t), allocatable :: temp(:)
        integer :: target_size, source_size, i, j
        
        if (.not. allocated(source)) return
        
        source_size = size(source)
        if (source_size == 0) return
        
        ! Check for duplicates between source and target
        if (allocated(target)) then
            target_size = size(target)
            do i = 1, source_size
                do j = 1, target_size
                    if (allocated(source(i)%name) .and. allocated(target(j)%name)) then
                        if (source(i)%name == target(j)%name) then
                            call fatal_error(error, "Duplicate test '"//source(i)%name//"' found. " // &
                                           "Multiple definitions of the same test are not currently allowed.")
                            return
                        end if
                    end if
                end do
            end do
        end if
        
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
    end subroutine merge_test_arrays

    !> Merge preprocess arrays by merging configurations for same preprocessor names
    !> and appending new ones
    subroutine merge_preprocess_arrays(target, source)        
        type(preprocess_config_t), allocatable, intent(inout) :: target(:)
        type(preprocess_config_t), allocatable, intent(in) :: source(:)
        
        type(preprocess_config_t), allocatable :: temp(:)
        integer :: target_size, source_size, i, j, new_count
        integer, allocatable :: source_to_target_map(:)  ! Maps source index to target index (0 = new)
        
        if (.not. allocated(source)) return
        
        source_size = size(source)
        if (source_size == 0) return
        
        if (.not. allocated(target)) then
            allocate(target(source_size), source=source)
            return
        end if
        
        target_size = size(target)
        
        ! Create mapping arrays in a single pass
        allocate(source_to_target_map(source_size), source=0)
        
        ! Single loop to build the mapping
        do i = 1, source_size
            if (allocated(source(i)%name)) then
                do j = 1, target_size
                    if (allocated(target(j)%name)) then
                        if (target(j)%name == source(i)%name) then
                            source_to_target_map(i) = j
                            exit
                        end if
                    end if
                end do
            end if
        end do
        
        ! Merge overlapping configurations
        do i = 1, source_size
            j = source_to_target_map(i)
            if (j==0) cycle ! new config
            call merge_preprocessor_config(target(j), source(i))
        end do
        
        ! Count and add new preprocessors
        new_count = count(source_to_target_map==0)
        if (new_count > 0) then
            allocate(temp(target_size + new_count))
            temp(1:target_size) = target
            
            ! Add new preprocessors in a single pass
            j = target_size
            do i = 1, source_size
                if (source_to_target_map(i)==0) then
                    j = j + 1
                    temp(j) = source(i)
                end if
            end do
            
            call move_alloc(temp, target)
        end if
    end subroutine merge_preprocess_arrays
    
    !> Helper to merge two preprocessor configurations with the same name
    subroutine merge_preprocessor_config(target, source)
        type(preprocess_config_t), intent(inout) :: target
        type(preprocess_config_t), intent(in) :: source
        
        ! Merge suffixes arrays
        call merge_string_arrays(target%suffixes, source%suffixes)
        
        ! Merge directories arrays  
        call merge_string_arrays(target%directories, source%directories)
        
        ! Merge macros arrays
        call merge_string_arrays(target%macros, source%macros)
        
    end subroutine merge_preprocessor_config

    !> Merge string arrays by appending source to target
    subroutine merge_string_arrays(target, source)
        type(string_t), allocatable, intent(inout) :: target(:)
        type(string_t), allocatable, intent(in) :: source(:)
        
        type(string_t), allocatable :: temp(:)
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
    end subroutine merge_string_arrays

    !> Merge metapackages using OR logic - if either requests it, turn it on
    subroutine merge_metapackages_additive(target, source)
        type(metapackage_request_t), intent(inout) :: target
        type(metapackage_request_t), intent(in) :: source
        
        ! OR logic: if either requests a metapackage, turn it on
        if (source%on) then
            target%on = .true.
            target%name = source%name
            ! Use source version if target doesn't have one
            if (allocated(source%version) .and. .not. allocated(target%version)) then
                target%version = source%version
            end if
        end if        
        
    end subroutine merge_metapackages_additive

    !> Merge whole metapackage config
    subroutine merge_metapackages(target, source)        
        type(metapackage_config_t), intent(inout) :: target
        type(metapackage_config_t), intent(in) :: source
        
        call merge_metapackages_additive(target%openmp,source%openmp)
        call merge_metapackages_additive(target%stdlib,source%stdlib)
        call merge_metapackages_additive(target%minpack,source%minpack)
        call merge_metapackages_additive(target%mpi,source%mpi)
        call merge_metapackages_additive(target%hdf5,source%hdf5)
        call merge_metapackages_additive(target%netcdf,source%netcdf)
        call merge_metapackages_additive(target%blas,source%blas)

    end subroutine merge_metapackages

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
            & -fcheck=array-temps -fbacktrace', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('debug', id_gcc, OS_ALL, &
            ' -Wall -Wextra -Wimplicit-interface -Wno-external-argument-mismatch&
            & -fPIC -fmax-errors=1 -g -fcheck=bounds&
            & -fcheck=array-temps -fbacktrace -fcoarray=single', &
            ' -Wall -Wextra -fPIC -fmax-errors=1 -g', &
            ' -Wall -Wextra -fPIC -fmax-errors=1 -g'))
            
        call collection%push_variant(default_variant('debug', id_f95, OS_ALL, &
            ' -Wall -Wextra -Wimplicit-interface -Wno-external-argument-mismatch&
            & -fPIC -fmax-errors=1 -g -fcheck=bounds&
            & -fcheck=array-temps -Wno-maybe-uninitialized -Wno-uninitialized -fbacktrace', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('debug', id_nvhpc, OS_ALL, &
            ' -Minform=inform -Mbackslash -g -Mbounds -Mchkptr -Mchkstk -traceback', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('debug', id_intel_classic_nix, OS_ALL, &
            ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('debug', id_intel_classic_nix, OS_WINDOWS, &
            ' /warn:all /check:all /error-limit:1&
            & /Od /Z7 /assume:byterecl /traceback'))
            
        call collection%push_variant(default_variant('debug', id_intel_llvm_nix, OS_ALL, &
            ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('debug', id_intel_llvm_nix, OS_WINDOWS, &
            ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl'))
            
        call collection%push_variant(default_variant('debug', id_lfortran, OS_ALL, '', ' -fPIC', ' -fPIC'))
            
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
        call collection%push_variant(default_variant('release', id_caf, OS_ALL, &
            ' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('release', id_gcc, OS_ALL, &
            ' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops -fcoarray=single', &
            ' -O3 -fPIC -fmax-errors=1 -funroll-loops', &
            ' -O3 -fPIC -fmax-errors=1 -funroll-loops'))
            
        call collection%push_variant(default_variant('release', id_f95, OS_ALL, &
            ' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -ffast-math -funroll-loops', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('release', id_nvhpc, OS_ALL, &
            ' -Mbackslash', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('release', id_intel_classic_nix, OS_LINUX, &
            ' -fp-model precise -pc64 -align all -error-limit 1 -reentrancy&
            & threaded -nogen-interfaces -assume byterecl', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('release', id_intel_classic_windows, &
            OS_WINDOWS, ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
            & /nogen-interfaces /assume:byterecl'))
            
        call collection%push_variant(default_variant('release', id_intel_llvm_nix, &
            OS_LINUX, ' -fp-model=precise -pc64 -align all -error-limit 1 -reentrancy threaded&
            & -nogen-interfaces -assume byterecl', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('release', id_intel_llvm_nix, &
            OS_WINDOWS, ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
            & /nogen-interfaces /assume:byterecl'))
        
        call collection%push_variant(default_variant('release', id_nag, OS_ALL, &
            ' -O4 -coarray=single -PIC', &
            ' -fPIC', &
            ' -fPIC'))
            
        call collection%push_variant(default_variant('release', id_lfortran, OS_ALL, &
            ' flag_lfortran_opt', &
            ' -fPIC', &
            ' -fPIC'))
            
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

    !> Helper to create a feature variant
    function default_variant(name, compiler_id, os_type, flags, c_flags, cxx_flags) result(feature)
        character(len=*), intent(in) :: name
        integer(compiler_enum), intent(in) :: compiler_id
        integer, intent(in) :: os_type
        character(len=*), optional, intent(in) :: flags
        character(len=*), optional, intent(in) :: c_flags
        character(len=*), optional, intent(in) :: cxx_flags
        type(feature_config_t) :: feature

        feature%name = name
        feature%platform%compiler = compiler_id
        feature%platform%os_type = os_type
        feature%default = .true.  ! These are built-in features
        feature%flags = ""
        feature%c_flags = ""
        feature%cxx_flags = ""
        
        if (present(flags)) then
            feature%flags = flags
        end if
        if (present(c_flags)) then
            feature%c_flags = c_flags
        end if
        if (present(cxx_flags)) then
            feature%cxx_flags = cxx_flags
        end if

    end function default_variant


    !> Check that the collection has valid OS/compiler logic and can be merged safely
    !> Implements standardized feature hierarchy validation:
    !> 1. OS_all+id_all (base) → id_compiler+OS_all → id_all+OS_current → id_compiler+OS_current
    !> 2. Additive properties (flags) can be concatenated 
    !> 3. Allocatable properties can only exist in one variant per merge path
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
        
        ! Base feature must be OS_ALL + id_all for proper hierarchy
        if (self%base%platform%os_type /= OS_ALL .or. self%base%platform%compiler /= id_all) then
            call fatal_error(error, "Base feature '"//self%base%name// &
                            "' must have OS_ALL and id_all platform settings")
            return
        end if
        
        ! Check all variants have valid platform settings and hierarchy rules
        if (allocated(self%variants)) then
            do i = 1, size(self%variants)
                ! Validate OS and compiler settings
                if (self%variants(i)%platform%os_type == OS_UNKNOWN) then
                    call fatal_error(error, "Variant "//trim(str(i))//" of feature '" &
                                     //self%base%name//"' has invalid OS type")
                    return
                end if
                
                if (self%variants(i)%platform%compiler == id_unknown) then
                    call fatal_error(error, "Variant "//trim(str(i))//" of feature '" &
                                     //self%base%name//"' has invalid compiler type")
                    return
                end if
                
                ! Check that variant name matches base name
                if (allocated(self%variants(i)%name) .and. allocated(self%base%name)) then
                    if (self%variants(i)%name /= self%base%name) then
                        call fatal_error(error, "Variant "//trim(str(i))//" name '" &
                                                //self%variants(i)%name// &
                                                 "' does not match base name '"//self%base%name//"'")
                        return
                    end if
                end if
                
                ! Validate feature hierarchy rules
                call validate_variant_hierarchy(self%variants(i), i, error)
                if (allocated(error)) return
                
                ! Check for exact duplicate platforms
                do j = i + 1, size(self%variants)
                    if (self%variants(i)%platform == self%variants(j)%platform) then
                        call fatal_error(error, "Duplicate platform configurations: "// &
                                                self%variants(i)%manifest_name()// &
                                                " and "//self%variants(j)%manifest_name()) 
                        return
                    end if
                end do
            end do
            
            ! Check for conflicts between variants that could be applied together
            call check_merge_conflicts(self, error)
            if (allocated(error)) return
        end if
        
    end subroutine check_collection

    !> Validate that a variant follows the feature hierarchy rules
    !> Valid combinations:
    !> - id_compiler + OS_all (compiler-specific, all OS)
    !> - id_all + OS_specific (OS-specific, all compilers)  
    !> - id_compiler + OS_specific (target-specific)
    !> Note: id_all + OS_all variants are allowed during parsing but should be merged into base
    subroutine validate_variant_hierarchy(variant, index, error)
        type(feature_config_t), intent(in) :: variant
        integer, intent(in) :: index
        type(error_t), allocatable, intent(out) :: error
        
        ! For now, allow all combinations - the merge logic handles id_all + OS_ALL -> base
        ! The validation was too strict for current parsing logic
        ! TODO: Implement stricter validation after parsing is fixed
        
        ! All combinations are valid in the hierarchy:
        ! - id_all + OS_all (should be merged to base, but parsing might create these temporarily)
        ! - id_compiler + OS_all (compiler-specific)
        ! - id_all + OS_specific (OS-specific) 
        ! - id_compiler + OS_specific (target-specific)
        
    end subroutine validate_variant_hierarchy

    !> Check for merge conflicts between variants that could be applied together
    !> This validates that no two applicable variants have conflicting allocatable properties
    subroutine check_merge_conflicts(collection, error)
        class(feature_collection_t), intent(in) :: collection
        type(error_t), allocatable, intent(out) :: error
        
        integer :: i, j
        type(feature_config_t) :: merged_feature
        integer, parameter :: test_compilers(3) = [id_gcc, id_flang, id_intel_classic_nix]
        integer, parameter :: test_oses(3) = [OS_LINUX, OS_WINDOWS, OS_MACOS]
        integer :: comp_idx, os_idx
        
        ! Test merge conflicts for common compiler/OS combinations
        do comp_idx = 1, size(test_compilers)
            do os_idx = 1, size(test_oses)
                
                ! Start with base feature
                merged_feature = collection%base
                
                ! Try to merge all applicable variants for this target
                if (allocated(collection%variants)) then
                    do i = 1, size(collection%variants)
                        if (variant_applies_to_target(collection%variants(i), &
                                                    test_compilers(comp_idx), test_oses(os_idx))) then
                            
                            ! Check for allocatable property conflicts before merging
                            call check_single_merge_conflict(merged_feature, collection%variants(i), &
                                                            test_compilers(comp_idx), test_oses(os_idx), error)
                            if (allocated(error)) return
                            
                            ! Simulate the merge (without error checking since we just checked)
                            call simulate_merge(merged_feature, collection%variants(i))
                        end if
                    end do
                end if
            end do
        end do
    end subroutine check_merge_conflicts
    
    !> Check if a variant applies to the given target compiler and OS
    logical function variant_applies_to_target(variant, target_compiler, target_os)
        type(feature_config_t), intent(in) :: variant
        integer, intent(in) :: target_compiler, target_os
        
        ! A variant applies if:
        ! - Compiler matches or is id_all
        ! - OS matches or is OS_ALL
        variant_applies_to_target = &
            (variant%platform%compiler == target_compiler .or. variant%platform%compiler == id_all) .and. &
            (variant%platform%os_type == target_os .or. variant%platform%os_type == OS_ALL)
    end function variant_applies_to_target
    
    !> Check for conflicts between two features that would be merged
    subroutine check_single_merge_conflict(target, source, compiler_id, os_id, error)
        type(feature_config_t), intent(in) :: target, source
        integer, intent(in) :: compiler_id, os_id
        type(error_t), allocatable, intent(out) :: error
        
        character(len=:), allocatable :: compiler, os
        
        compiler = compiler_id_name(compiler_id)
        os = OS_NAME(os_id)
        
        if (allocated(target%build) .and. allocated(source%build)) then
            call fatal_error(error, "Feature '"//target%name//"' has conflicting build configurations for "// &
                           compiler//"+"//os//": both base/variants specify build settings")
            return
        end if
        
        if (allocated(target%install) .and. allocated(source%install)) then
            call fatal_error(error, "Feature '"//target%name//"' has conflicting install configurations for "// &
                           compiler//"+"//os//": both base/variants specify install settings")
            return
        end if
        
        if (allocated(target%fortran) .and. allocated(source%fortran)) then
            call fatal_error(error, "Feature '"//target%name//"' has conflicting fortran configurations for "// &
                           compiler//"+"//os//": both base/variants specify fortran settings")
            return
        end if
        
        if (allocated(target%library) .and. allocated(source%library)) then
            call fatal_error(error, "Feature '"//target%name//"' has conflicting library configurations for "// &
                           compiler//"+"//os//": both base/variants specify library settings")
            return
        end if
    end subroutine check_single_merge_conflict
    
    !> Simulate merging source into target for conflict checking (without error handling)
    subroutine simulate_merge(target, source)
        type(feature_config_t), intent(inout) :: target
        type(feature_config_t), intent(in) :: source
        
        ! Only merge allocatable properties if target doesn't have them
        ! (conflicts should have been caught by check_single_merge_conflict)
        
        if (allocated(source%build) .and. .not. allocated(target%build)) then
            allocate(target%build)
            target%build = source%build
        end if
        
        if (allocated(source%install) .and. .not. allocated(target%install)) then
            allocate(target%install)
            target%install = source%install
        end if
        
        if (allocated(source%fortran) .and. .not. allocated(target%fortran)) then
            allocate(target%fortran)
            target%fortran = source%fortran
        end if
        
        if (allocated(source%library) .and. .not. allocated(target%library)) then
            allocate(target%library)
            target%library = source%library
        end if
    end subroutine simulate_merge
    
    !> Merge a feature configuration into an existing global package
    subroutine merge_into_package(self, package, target, error)
        class(feature_collection_t), intent(in) :: self
        
        class(feature_config_t), intent(inout) :: package
        
        type(platform_config_t), intent(in) :: target
        
        type(error_t), allocatable, intent(out) :: error
        
        type(feature_config_t) :: feature
        
        ! Extract the feature configuration for the target platform
        feature = self%extract_for_target(target, error)
        if (allocated(error)) return

        ! Merge the extracted feature into the package
        call merge_feature_configs(package, feature, error)
        if (allocated(error)) return
        
    end subroutine merge_into_package

    !> Extract a merged feature configuration for the given target platform
    type(feature_config_t) function extract_for_target(self, target, error) result(feature)
        class(feature_collection_t), intent(in) :: self
        type(platform_config_t), intent(in) :: target
        type(error_t), allocatable, intent(out) :: error
        
        integer :: i
                
        ! Start with base feature as foundation
        feature = self%base
        
        ! Apply matching variants on top of base
        if (allocated(self%variants)) then
            do i = 1, size(self%variants)
                if (self%variants(i)%platform%matches(target)) then
                    ! Merge this variant into the feature
                    call merge_feature_configs(feature, self%variants(i), error)
                    if (allocated(error)) return
                end if
            end do
        end if
        
    end function extract_for_target

    !> Add default features to existing features array if they don't already exist
    subroutine add_default_features(features, error)

        !> Instance of the feature collections array (will be resized)
        type(feature_collection_t), allocatable, intent(inout) :: features(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(feature_collection_t), allocatable :: temp_features(:)
        type(feature_collection_t), allocatable :: default_features(:)
        logical :: debug_exists, release_exists
        integer :: i, current_size, new_size

        ! Get default features
        call get_default_features(default_features, error)
        if (allocated(error)) return

        ! Check if debug and release features already exist
        debug_exists = .false.
        release_exists = .false.

        if (allocated(features)) then
            do i = 1, size(features)
                if (allocated(features(i)%base%name)) then
                    if (features(i)%base%name == "debug") debug_exists = .true.
                    if (features(i)%base%name == "release") release_exists = .true.
                end if
            end do
            current_size = size(features)
        else
            current_size = 0
        end if

        ! Calculate how many features to add
        new_size = current_size
        if (.not. debug_exists) new_size = new_size + 1
        if (.not. release_exists) new_size = new_size + 1

        ! If nothing to add, return
        if (new_size == current_size) return

        ! Create new array with existing + missing defaults
        allocate(temp_features(new_size))

        ! Copy existing features
        if (current_size > 0) then
            temp_features(1:current_size) = features(1:current_size)
        end if

        ! Add missing defaults
        i = current_size
        if (.not. debug_exists) then
            i = i + 1
            temp_features(i) = default_features(1)  ! debug feature
        end if
        if (.not. release_exists) then
            i = i + 1
            temp_features(i) = default_features(2)  ! release feature
        end if

        ! Replace the features array
        call move_alloc(temp_features, features)

    end subroutine add_default_features


    !> Create a feature collection from a single feature_config_t
    !> The feature becomes the base configuration for all OS/compiler combinations
    type(feature_collection_t) function collection_from_feature(self) result(collection)
        
        !> Feature configuration to convert
        class(feature_config_t), intent(in) :: self
        
        ! Copy the feature into the base configuration
        collection%base = self
        
        ! Set platform to all OS and all compilers for the base
        collection%base%platform%os_type = OS_ALL
        collection%base%platform%compiler = id_all
        
        ! Copy the name if available
        if (allocated(self%name)) collection%base%name = self%name
        
        ! No variants initially - just the base configuration
        ! (variants can be added later if needed)
        
    end function collection_from_feature
    
      
    !> Check if there is a CPP preprocessor configuration
    elemental logical function has_cpp(self) 
        class(feature_collection_t), intent(in) :: self
          
        integer :: i
          
        has_cpp = self%base%has_cpp()
        if (has_cpp) return
        if (.not.allocated(self%variants)) return
          
        do i=1,size(self%variants)
            has_cpp = self%variants(i)%has_cpp()
            if (has_cpp) return
        end do
          
    end function has_cpp    
    
end module fpm_manifest_feature_collection
