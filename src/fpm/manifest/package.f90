!> Define the package data containing the meta data from the configuration file.
!>
!> The package data defines a Fortran type corresponding to the respective
!> TOML document, after creating it from a package file no more interaction
!> with the TOML document is required.
!>
!> Every configuration type provides it custom constructor (prefixed with `new_`)
!> and knows how to deserialize itself from a TOML document.
!> To ensure we find no untracked content in the package file all keywords are
!> checked and possible entries have to be explicitly allowed in the `check`
!> function.
!> If entries are mutally exclusive or interdependent inside the current table
!> the `check` function is required to enforce this schema on the data structure.
!>
!> The package file root allows the following keywords
!>
!>```toml
!>name = "string"
!>version = "string"
!>license = "string"
!>author = "string"
!>maintainer = "string"
!>copyright = "string"
!>[library]
!>[dependencies]
!>[dev-dependencies]
!>[profiles]
!>[build]
!>[install]
!>[fortran]
!>[[ executable ]]
!>[[ example ]]
!>[[ test ]]
!>[extra]
!>```
module fpm_manifest_package
    use fpm_manifest_build, only: build_config_t, new_build_config
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies
    use fpm_manifest_profile, only : profile_config_t, new_profiles, add_default_profiles
    use fpm_manifest_example, only : example_config_t, new_example
    use fpm_manifest_executable, only : executable_config_t, new_executable
    use fpm_manifest_fortran, only : fortran_config_t, new_fortran_config
    use fpm_manifest_library, only : library_config_t, new_library
    use fpm_manifest_install, only: install_config_t, new_install_config
    use fpm_manifest_test, only : test_config_t, new_test
    use fpm_manifest_preprocess, only : preprocess_config_t, new_preprocessors
    use fpm_manifest_feature, only: feature_config_t, init_feature_components
    use fpm_manifest_feature_collection, only: feature_collection_t, new_collections, add_default_features
    use fpm_manifest_platform, only: platform_config_t
    use fpm_strings, only: string_t
    use fpm_filesystem, only : exists, getline, join_path
    use fpm_error, only : error_t, fatal_error, syntax_error, bad_name_error
    use tomlf, only : toml_table, toml_array, toml_key, toml_stat
    use fpm_toml, only : get_value, len, serializable_t, set_value, set_string, set_list, add_table
    use fpm_versioning, only : version_t, new_version
    
    implicit none
    private

    public :: package_config_t, new_package

    !> Package meta data
    !> Package configuration data - extends a `feature_config_t` to represent the "default" 
    !> package feature. The following are now inherited from feature_config_t: name (but for package 
    !> it's the package name), description, compiler, os_type (defaults to id_all, OS_ALL for packages)
    !> library, executable(:), dependency(:), dev_dependency(:), example(:), test(:), preprocess(:)
    !> flags, c_flags, cxx_flags, link_time_flags, requires_features(:)

    type, extends(feature_config_t) :: package_config_t

        !> Package version (name is inherited from feature_config_t%name)
        type(version_t) :: version

        !> Package metadata (package-specific)  
        character(len=:), allocatable :: license
        character(len=:), allocatable :: author
        character(len=:), allocatable :: maintainer
        character(len=:), allocatable :: copyright

        !> Additional feature collections beyond the default package feature
        type(feature_collection_t), allocatable :: features(:)

        !> Profiles (collections of features)
        type(profile_config_t), allocatable :: profiles(:)

    contains

        !> Print information on this instance
        procedure :: info

        !> Serialization interface
        procedure :: serializable_is_same => manifest_is_same
        procedure :: dump_to_toml
        procedure :: load_from_toml
        
        !> Check if any features has a cpp configuration
        procedure :: has_cpp

        !> Export package configuration with features applied
        procedure :: export_config

        !> Find feature by name, returns index or 0 if not found
        procedure :: find_feature
        
        !> Find profile by name, returns index or 0 if not found
        procedure :: find_profile

    end type package_config_t

    character(len=*), parameter, private :: class_name = 'package_config_t'


contains


    !> Construct a new package configuration from a TOML data structure
    subroutine new_package(self, table, root, error)

        !> Instance of the package configuration
        type(package_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(len=*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        ! Backspace (8), tabulator (9), newline (10), formfeed (12) and carriage
        ! return (13) are invalid in package names
        character(len=*), parameter :: invalid_chars = &
           achar(8) // achar(9) // achar(10) // achar(12) // achar(13)
        type(toml_table), pointer :: child, node
        type(toml_array), pointer :: children
        character(len=:), allocatable :: version, version_file
        integer :: ii, nn, stat, io
        
        ! Ensure metapackage data is initialized although off
        call self%meta%reset()        

        call check(table, error)
        if (allocated(error)) return

        ! Get package name and perform validation
        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve package name")
           return
        end if
        if (bad_name_error(error,'package',self%name)) return

        if (len(self%name) <= 0) then
            call syntax_error(error, "Package name must be a non-empty string")
            return
        end if

        ii = scan(self%name, invalid_chars)
        if (ii > 0) then
            call syntax_error(error, "Package name contains invalid characters")
            return
        end if

        ! Get package-specific metadata
        call get_value(table, "license", self%license)
        call get_value(table, "author", self%author)
        call get_value(table, "maintainer", self%maintainer)
        call get_value(table, "copyright", self%copyright)

        ! Initialize shared feature components
        call init_feature_components(self%feature_config_t, table, root=root, error=error)
        if (allocated(error)) return

        call get_value(table, "version", version, "0")
        call new_version(self%version, version, error)
        if (allocated(error) .and. present(root)) then
            version_file = join_path(root, version)
            if (exists(version_file)) then
                deallocate(error)
                open(file=version_file, newunit=io, iostat=stat)
                if (stat == 0) then
                    call getline(io, version, iostat=stat)
                end if
                if (stat == 0) then
                    close(io, iostat=stat)
                end if
                if (stat == 0) then
                    call new_version(self%version, version, error)
                else
                    call fatal_error(error, "Reading version number from file '" &
                        & //version_file//"' failed")
                end if
            end if
        end if
        if (allocated(error)) return

        call get_value(table, "profiles", child, requested=.false.)
        if (associated(child)) then
            call new_profiles(self%profiles, child, error)
            if (allocated(error)) return
        else
            ! No profiles defined - start with empty array
            allocate(self%profiles(0))
        end if

        call get_value(table, "features", child, requested=.false.)
        if (associated(child)) then
            ! Parse features from manifest using new_collections
            call new_collections(self%features, child, error)
            if (allocated(error)) return
        else
            ! No features defined - start with empty array
            allocate(self%features(0))
        end if

        ! Add default features and profiles if they don't already exist
        call add_default_features(self%features, error)
        if (allocated(error)) return

        call add_default_profiles(self%profiles, error)
        if (allocated(error)) return

        ! Validate profiles after all features and profiles have been loaded
        call validate_profiles(self, error)
        if (allocated(error)) return
        
    end subroutine new_package


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        logical :: name_present
        integer :: ikey

        name_present = .false.

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Package file is empty")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in package file")
                exit

            case("name")
                name_present = .true.

            case("version", "license", "author", "maintainer", "copyright", &
                    & "description", "keywords", "categories", "homepage", "build", &
                    & "dependencies", "dev-dependencies", "profiles", "features", "test", "executable", &
                    & "example", "library", "install", "extra", "preprocess", "fortran")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Package name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the package configuration
        class(package_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ii
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)', &
            & fmti = '("#", 1x, a, t30, i0)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Package"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if

        call self%build%info(unit, pr - 1)

        call self%install%info(unit, pr - 1)

        if (allocated(self%library)) then
            write(unit, fmt) "- target", "archive"
            call self%library%info(unit, pr - 1)
        end if

        if (allocated(self%executable)) then
            if (size(self%executable) > 1 .or. pr > 2) then
                write(unit, fmti) "- executables", size(self%executable)
            end if
            do ii = 1, size(self%executable)
                call self%executable(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%dependency)) then
            if (size(self%dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- dependencies", size(self%dependency)
            end if
            do ii = 1, size(self%dependency)
                call self%dependency(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%example)) then
            if (size(self%example) > 1 .or. pr > 2) then
                write(unit, fmti) "- examples", size(self%example)
            end if
            do ii = 1, size(self%example)
                call self%example(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%test)) then
            if (size(self%test) > 1 .or. pr > 2) then
                write(unit, fmti) "- tests", size(self%test)
            end if
            do ii = 1, size(self%test)
                call self%test(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%dev_dependency)) then
            if (size(self%dev_dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- development deps.", size(self%dev_dependency)
            end if
            do ii = 1, size(self%dev_dependency)
                call self%dev_dependency(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%profiles)) then
            if (size(self%profiles) > 1 .or. pr > 2) then
                write(unit, fmti) "- profiles", size(self%profiles)
            end if
            do ii = 1, size(self%profiles)
                call self%profiles(ii)%info(unit, pr - 1)
            end do
        end if

    end subroutine info



   logical function manifest_is_same(this,that)
      class(package_config_t), intent(in) :: this
      class(serializable_t), intent(in) :: that

      integer :: ii

      manifest_is_same = .false.

      select type (other=>that)
         type is (package_config_t)
            
            ! Compare base fields
            if (.not.this%feature_config_t==other%feature_config_t) return            
            
            ! Manifest-specific fields
            if (.not.this%version==other%version) return
            if (allocated(this%license).neqv.allocated(other%license)) return
            if (allocated(this%license)) then
                if (.not.this%license==other%license) return
            end if
            if (allocated(this%author).neqv.allocated(other%author)) return
            if (allocated(this%author)) then
                if (.not.this%author==other%author) return
            end if
            if (allocated(this%maintainer).neqv.allocated(other%maintainer)) return
            if (allocated(this%maintainer)) then
                if (.not.this%maintainer==other%maintainer) return
            end if
            if (allocated(this%copyright).neqv.allocated(other%copyright)) return
            if (allocated(this%copyright)) then
                if (.not.this%copyright==other%copyright) return
            end if
            if (allocated(this%profiles).neqv.allocated(other%profiles)) return
            if (allocated(this%profiles)) then
                if (.not.size(this%profiles)==size(other%profiles)) return
                do ii=1,size(this%profiles)
                    if (.not.this%profiles(ii)==other%profiles(ii)) return
                end do
            end if

         class default
            ! Not the same type
            return
      end select

      !> All checks passed!
      manifest_is_same = .true.

    end function manifest_is_same

    !> Dump manifest to toml table
    subroutine dump_to_toml(self, table, error)

       !> Instance of the serializable object
       class(package_config_t), intent(inout) :: self

       !> Data structure
       type(toml_table), intent(inout) :: table

       !> Error handling
       type(error_t), allocatable, intent(out) :: error

       integer :: ii
       type(toml_table), pointer :: ptr,ptr_pkg
       character(30) :: unnamed
       character(128) :: profile_name
       
       ! Dump feature first
       call self%feature_config_t%dump_to_toml(table, error)       
       if (allocated(error)) return

       ! Package-specific fields
       call set_string(table, "version", self%version%s(), error, class_name)
       if (allocated(error)) return
       call set_string(table, "license", self%license, error, class_name)
       if (allocated(error)) return
       call set_string(table, "author", self%author, error, class_name)
       if (allocated(error)) return
       call set_string(table, "maintainer", self%maintainer, error, class_name)
       if (allocated(error)) return
       call set_string(table, "copyright", self%copyright, error, class_name)
       if (allocated(error)) return

       if (allocated(self%profiles)) then

           call add_table(table, "profiles", ptr_pkg)
           if (.not. associated(ptr_pkg)) then
              call fatal_error(error, class_name//" cannot create 'profiles' table ")
              return
           end if

           do ii = 1, size(self%profiles)

              associate (pkg => self%profiles(ii))

                 !> Duplicate profile names are possible, as multiple profiles are possible with the
                 !> same name, same compiler, etc. So, use a unique name here
                 write(profile_name,2) ii
                 call add_table(ptr_pkg, trim(profile_name), ptr, error, class_name//'(profiles)')
                 if (allocated(error)) return
                 call pkg%dump_to_toml(ptr, error)
                 if (allocated(error)) return

              end associate

           end do
       end if

       2 format('PROFILE_',i0)

     end subroutine dump_to_toml

     !> Read manifest from toml table (no checks made at this stage)
     subroutine load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(package_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: keys(:),pkg_keys(:)
        integer :: ii, jj
        character(len=:), allocatable :: flag
        type(toml_table), pointer :: ptr,ptr_pkg
        
        ! Clean state
        if (allocated(self%library)) deallocate(self%library)
        if (allocated(self%executable)) deallocate(self%executable)
        if (allocated(self%dependency)) deallocate(self%dependency)
        if (allocated(self%dev_dependency)) deallocate(self%dev_dependency)
        if (allocated(self%profiles)) deallocate(self%profiles)
        if (allocated(self%example)) deallocate(self%example)
        if (allocated(self%test)) deallocate(self%test)
        if (allocated(self%preprocess)) deallocate(self%preprocess)        
        
        !> Load base fields
        call self%feature_config_t%load_from_toml(table, error)
        if (allocated(error)) return

        call table%get_keys(keys)

        call get_value(table, "license", self%license)
        call get_value(table, "author", self%author)
        call get_value(table, "maintainer", self%maintainer)
        call get_value(table, "copyright", self%copyright)
        call get_value(table, "version", flag)
        call new_version(self%version, flag, error)
        if (allocated(error)) then
           error%message = class_name//': version error from TOML table - '//error%message
           return
        endif

        sub_deps: do ii = 1, size(keys)

           select case (keys(ii)%key)

              case ("profiles")

                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving profiles table')
                      return
                   end if

                   !> Read all packages
                   call ptr%get_keys(pkg_keys)
                   allocate(self%profiles(size(pkg_keys)))

                   do jj = 1, size(pkg_keys)
                      call get_value(ptr, pkg_keys(jj), ptr_pkg)
                      call self%profiles(jj)%load_from_toml(ptr_pkg, error)
                      if (allocated(error)) return
                   end do

              case default
                    cycle sub_deps
           end select

        end do sub_deps

     end subroutine load_from_toml

    !> Export package configuration for a given (OS+compiler) platform
    type(package_config_t) function export_config(self, platform, features, profile, verbose, error) result(cfg)

        !> Instance of the package configuration
        class(package_config_t), intent(in), target :: self

        !> Target platform
        type(platform_config_t), intent(in) :: platform

        !> Optional list of features to apply (cannot be used with profile)
        type(string_t), optional, intent(in), target :: features(:)

        !> Optional profile name to apply (cannot be used with features)
        character(len=*), optional, intent(in) :: profile

        !> Verbose output flag
        logical, optional, intent(in) :: verbose

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        integer :: i, idx
        type(string_t), pointer :: want_features(:)
        logical :: apply_default

        ! Validate that both profile and features are not specified simultaneously
        if (present(profile) .and. present(features)) then
            call syntax_error(error, "Cannot specify both 'profile' and 'features' parameters simultaneously")
            return
        end if

        ! Copy the entire package configuration
        cfg = self

        ! Determine which features to apply and whether to include the "default" profile.
        ! Default profile features are applied when:
        !   - No explicit profile/features are specified (implicit debug build)
        !   - The "debug" or "release" profile is requested
        ! Default profile features are NOT applied when:
        !   - An explicit features list is specified
        !   - A custom (non-debug/release) profile is requested
        apply_default = .false.

        if (present(features)) then
            want_features => features
        elseif (present(profile)) then
            idx = find_profile(self, profile)
            if (idx<=0) then
                call fatal_error(error, "Cannot find profile "//profile//" in package "//self%name)
                return
            end if
            want_features => self%profiles(idx)%features
            apply_default = (profile == "debug" .or. profile == "release")
        else
            nullify(want_features)
            apply_default = .true.
        endif

        ! Apply "default" profile features first (baseline configuration)
        if (apply_default) then
            idx = find_profile(self, "default")
            if (idx > 0) then
                call apply_default_features(self, self%profiles(idx), cfg, platform, verbose, error)
                if (allocated(error)) return
            end if
        end if

        ! Then apply the requested features on top
        apply_features: if (associated(want_features)) then
            do i=1,size(want_features)

                ! Find feature
                idx = self%find_feature(want_features(i)%s)
                if (idx<=0) then
                    call fatal_error(error, "Cannot find feature "//want_features(i)%s//&
                                            " in package "//self%name)
                    return
                end if

                ! Print feature collection info if verbose
                if (present(verbose)) then
                    if (verbose) then
                        call print_feature_collection(self%features(idx), platform)
                    end if
                end if

                ! Add it to the current configuration
                call self%features(idx)%merge_into_package(cfg, platform, error)
                if (allocated(error)) return

            end do
        end if apply_features
        
        ! Ensure allocatable fields are always allocated with default values if not already set
        if (.not. allocated(cfg%build)) then
            allocate(cfg%build)
            cfg%build%auto_executables = .true.
            cfg%build%auto_examples = .true.
            cfg%build%auto_tests = .true.
            cfg%build%module_naming = .false.
        end if
        
        if (.not. allocated(cfg%install)) then
            allocate(cfg%install)
            cfg%install%library = .false.
            cfg%install%test = .false.
        end if
        
        if (.not. allocated(cfg%fortran)) then
            allocate(cfg%fortran)
            cfg%fortran%implicit_typing = .false.
            cfg%fortran%implicit_external = .false.
            cfg%fortran%source_form = 'free'
        end if        
        
    end function export_config

    !> Find profile by name, returns index or 0 if not found
    function find_profile(self, profile_name) result(idx)
        
        !> Instance of the package configuration
        class(package_config_t), intent(in) :: self
        
        !> Name of the feature to find
        character(len=*), intent(in) :: profile_name
        
        !> Index of the feature (0 if not found)
        integer :: idx
        
        integer :: i
        
        idx = 0
        
        ! Check if features are allocated
        if (.not. allocated(self%profiles)) return
        
        ! Search through features array
        do i = 1, size(self%profiles)
            if (allocated(self%profiles(i)%name)) then
                if (self%profiles(i)%name == profile_name) then
                    idx = i
                    return
                end if
            end if
        end do
        
    end function find_profile

    !> Apply all features from a profile into the package configuration
    subroutine apply_default_features(self, prof, cfg, platform, verbose, error)

        !> Instance of the package configuration
        class(package_config_t), intent(in) :: self

        !> Profile whose features to apply
        type(profile_config_t), intent(in) :: prof

        !> Package configuration to merge into
        type(package_config_t), intent(inout) :: cfg

        !> Target platform
        type(platform_config_t), intent(in) :: platform

        !> Verbose output flag
        logical, optional, intent(in) :: verbose

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i, idx

        if (.not. allocated(prof%features)) return

        do i = 1, size(prof%features)

            idx = self%find_feature(prof%features(i)%s)
            if (idx <= 0) then
                call fatal_error(error, "Cannot find feature "//prof%features(i)%s//&
                                        " in package "//self%name)
                return
            end if

            if (present(verbose)) then
                if (verbose) then
                    call print_feature_collection(self%features(idx), platform)
                end if
            end if

            call self%features(idx)%merge_into_package(cfg, platform, error)
            if (allocated(error)) return

        end do

    end subroutine apply_default_features

    !> Find feature by name, returns index or 0 if not found
    function find_feature(self, feature_name) result(idx)
        
        !> Instance of the package configuration
        class(package_config_t), intent(in) :: self
        
        !> Name of the feature to find
        character(len=*), intent(in) :: feature_name
        
        !> Index of the feature (0 if not found)
        integer :: idx
        
        integer :: i
        
        idx = 0
        
        ! Check if features are allocated
        if (.not. allocated(self%features)) return
        
        ! Search through features array
        do i = 1, size(self%features)
            if (allocated(self%features(i)%base%name)) then
                if (self%features(i)%base%name == feature_name) then
                    idx = i
                    return
                end if
            end if
        end do
        
    end function find_feature


    !> Validate profiles - check for duplicate names and valid feature references
    subroutine validate_profiles(self, error)
        
        !> Instance of the package configuration
        class(package_config_t), intent(in) :: self
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        integer :: i, j
        
        ! Check if profiles are allocated
        if (.not. allocated(self%profiles)) return
        
        ! Check for duplicate profile names
        do i = 1, size(self%profiles)
            do j = i + 1, size(self%profiles)
                if (allocated(self%profiles(i)%name) .and. allocated(self%profiles(j)%name)) then
                    if (self%profiles(i)%name == self%profiles(j)%name) then
                        call syntax_error(error, "Duplicate profile name '" // self%profiles(i)%name // "'")
                        return
                    end if
                end if
            end do
        end do
        
        ! Check that all profile features reference valid features
        do i = 1, size(self%profiles)
            if (allocated(self%profiles(i)%features)) then
                do j = 1, size(self%profiles(i)%features)
                    ! Check if feature exists (case sensitive)
                    if (self%find_feature(self%profiles(i)%features(j)%s) == 0) then
                        call syntax_error(error, "Profile '" // self%profiles(i)%name // &
                            "' references undefined feature '" // self%profiles(i)%features(j)%s // "'")
                        return
                    end if
                end do
            end if
        end do
        
    end subroutine validate_profiles

    !> Check if there is a CPP preprocessor configuration
    elemental logical function has_cpp(self)
        class(package_config_t), intent(in) :: self

        integer :: i

        has_cpp = self%feature_config_t%has_cpp()
        if (has_cpp) return
        if (.not.allocated(self%features)) return

        do i=1,size(self%features)
            has_cpp = self%features(i)%has_cpp()
            if (has_cpp) return
        end do

    end function has_cpp

    !> Print feature collection information in verbose mode
    subroutine print_feature_collection(collection, platform)
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit
        use fpm_compiler, only: compiler_name
        use fpm_environment, only: os_name
        use fpm_strings, only: string_cat
        type(feature_collection_t), intent(in) :: collection
        type(platform_config_t), intent(in) :: platform

        type(feature_config_t) :: extracted
        type(error_t), allocatable :: error_tmp
        integer :: i, j, n_macros

        ! Extract the feature configuration for the target platform
        extracted = collection%extract_for_target(platform, error_tmp)
        if (allocated(error_tmp)) return

        ! Print header with feature name
        if (allocated(extracted%name)) then
            print *, '+ feature collection: ', trim(extracted%name)
        end if

        ! Print platform information
        print *, '+   platform: ', platform%compiler_name(), ' on ', platform%os_name()

        ! Print flags
        if (allocated(extracted%flags)) then
            print *, '+   flags: ', trim(extracted%flags)
        end if
        if (allocated(extracted%c_flags)) then
            print *, '+   c-flags: ', trim(extracted%c_flags)
        end if
        if (allocated(extracted%cxx_flags)) then
            print *, '+   cxx-flags: ', trim(extracted%cxx_flags)
        end if
        if (allocated(extracted%link_time_flags)) then
            print *, '+   link-flags: ', trim(extracted%link_time_flags)
        end if

        ! Print preprocessor macros
        if (allocated(extracted%preprocess)) then
            n_macros = 0
            do i = 1, size(extracted%preprocess)
                if (allocated(extracted%preprocess(i)%macros)) then
                    n_macros = n_macros + size(extracted%preprocess(i)%macros)
                end if
            end do

            if (n_macros > 0) then
                print *, '+   cpp-macros: yes (', n_macros, ' defined)'
                do i = 1, size(extracted%preprocess)
                    if (allocated(extracted%preprocess(i)%macros)) then
                        do j = 1, size(extracted%preprocess(i)%macros)
                            print *, '+     - ', trim(extracted%preprocess(i)%macros(j)%s)
                        end do
                    end if
                end do
            else
                print *, '+   cpp-macros: no'
            end if
        else
            print *, '+   cpp-macros: no'
        end if

        ! Print description if available
        if (allocated(extracted%description)) then
            print *, '+   description: ', trim(extracted%description)
        end if

        ! Print number of variants in collection
        if (allocated(collection%variants)) then
            print *, '+   variants: ', size(collection%variants)
        end if

    end subroutine print_feature_collection

end module fpm_manifest_package
