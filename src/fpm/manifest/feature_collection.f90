
module fpm_manifest_feature_collection
    use fpm_manifest_feature, only: feature_config_t
    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_environment, only: OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN, OS_SOLARIS, &
                             OS_FREEBSD, OS_OPENBSD, OS_ALL, OS_NAME, match_os_type
    use fpm_compiler, only: compiler_enum, compiler_id_name, match_compiler_type, &
                          id_unknown, id_gcc, id_f95, id_caf, &
                          id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows, &
                          id_intel_llvm_nix, id_intel_llvm_windows, id_intel_llvm_unknown, &
                          id_pgi, id_nvhpc, id_nag, id_flang, id_flang_new, id_f18, &
                          id_ibmxl, id_cray, id_lahey, id_lfortran, id_all
    use fpm_strings, only: string_t, lower, operator(==)
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat
    use fpm_toml, only: get_value, len, serializable_t, set_value, set_string, set_list, add_table, &
                        get_list
    implicit none
    private
    
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
        

end module fpm_manifest_feature_collection
