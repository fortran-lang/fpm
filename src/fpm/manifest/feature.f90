!> Implementation of the meta data for features.
!>
!> A feature is a configurable set of package properties that can be
!> conditionally enabled. Features allow fine-grained control over
!> dependencies, compiler flags, preprocessor definitions, and other
!> package components based on the target compiler and operating system.
!>
!> Features are similar to Rust Cargo features but adapted for Fortran
!> package management. Each feature can specify:
!> - Compiler-specific flags and settings
!> - Additional dependencies
!> - Preprocessor definitions
!> - Source files and build configurations
!>
!> A feature table can currently have the following fields:
!>
!>```toml
!>[features.mpi]
!>description = "Enable MPI parallel support"
!>compiler = "gfortran"
!>os = "linux" 
!>flags = "-fopenmp"
!>preprocessor = ["WITH_MPI"]
!>[features.mpi.dependencies]
!>mpi = { git = "https://github.com/fortran-lang/mpi" }
!>```
!>
module fpm_manifest_feature
    use fpm_manifest_build, only: build_config_t, new_build_config
    use fpm_manifest_dependency, only: dependency_config_t, new_dependencies
    use fpm_manifest_example, only: example_config_t, new_example
    use fpm_manifest_executable, only: executable_config_t, new_executable
    use fpm_manifest_fortran, only: fortran_config_t, new_fortran_config
    use fpm_manifest_library, only: library_config_t, new_library
    use fpm_manifest_install, only: install_config_t, new_install_config
    use fpm_manifest_test, only: test_config_t, new_test
    use fpm_manifest_preprocess, only: preprocess_config_t, new_preprocessors
    use fpm_manifest_metapackages, only: metapackage_config_t, new_meta_config
    use fpm_manifest_platform, only: platform_config_t
    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_environment, only: OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN, OS_SOLARIS, &
                             OS_FREEBSD, OS_OPENBSD, OS_ALL, OS_NAME, match_os_type
    use fpm_compiler, only: compiler_enum, compiler_id_name, match_compiler_type, id_all
    use fpm_strings, only: string_t, lower, operator(==)
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat
    use fpm_toml, only: get_value, len, serializable_t, set_value, set_string, set_list, add_table, &
                        get_list
    implicit none
    private

    public :: feature_config_t, new_feature, new_features, find_feature, init_feature_components, &
              unique_programs

    !> Feature configuration data
    type, extends(serializable_t) :: feature_config_t

        !> Feature identity  
        character(len=:), allocatable :: name
        character(len=:), allocatable :: description
        
        !> Compiler/OS targeting (consistent with profile_config_t pattern)
        type(platform_config_t) :: platform
        
        !> Build configuration
        type(build_config_t), allocatable :: build
        
        !> Installation configuration
        type(install_config_t), allocatable :: install
        
        !> Fortran configuration
        type(fortran_config_t), allocatable :: fortran
        
        !> Library configuration
        type(library_config_t), allocatable :: library
        
        !> Executable configurations
        type(executable_config_t), allocatable :: executable(:)
        
        !> Dependencies
        type(dependency_config_t), allocatable :: dependency(:)
        
        !> Development dependencies
        type(dependency_config_t), allocatable :: dev_dependency(:)
        
        !> Examples
        type(example_config_t), allocatable :: example(:)
        
        !> Tests
        type(test_config_t), allocatable :: test(:)
        
        !> Preprocessor configuration
        type(preprocess_config_t), allocatable :: preprocess(:)
        
        !> Metapackage data
        type(metapackage_config_t) :: meta        
        
        !> Compiler flags  
        character(len=:), allocatable :: flags
        character(len=:), allocatable :: c_flags  
        character(len=:), allocatable :: cxx_flags
        character(len=:), allocatable :: link_time_flags
        
        !> Feature dependencies (not active yet)
        type(string_t), allocatable :: requires_features(:)       

    contains

        !> Print information on this instance
        procedure :: info
        
        !> Check validity of the TOML table
        procedure, nopass :: check
        
        !> Get manifest name
        procedure :: manifest_name
        
        !> Check if there is a cpp configuration
        procedure :: has_cpp

        !> Serialization interface
        procedure :: serializable_is_same => feature_is_same
        procedure :: dump_to_toml
        procedure :: load_from_toml

    end type feature_config_t

    character(len=*), parameter, private :: class_name = 'feature_config_t'
    
    interface unique_programs
        module procedure :: unique_programs1
        module procedure :: unique_programs2
    end interface unique_programs

contains

    !> Construct a new feature configuration from a TOML data structure
    subroutine new_feature(self, table, root, error, name)

        !> Instance of the feature configuration
        type(feature_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(len=*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Optional name override (if not provided, gets from table key)
        character(len=*), intent(in), optional :: name

        type(toml_table), pointer :: child, node
        type(toml_array), pointer :: children
        character(len=:), allocatable :: compiler_name, os_name
        integer :: ii, nn, stat

        ! Only check schema for pure features (not when called from package)
        if (.not. present(name)) then
            call check(table, error)
            if (allocated(error)) return
        end if

        ! Get feature name from parameter or table key
        if (present(name)) then
            self%name = name
        else
            call table%get_key(self%name)
        end if

        ! Initialize common components
        call init_feature_components(self, table, root=root, error=error)
        if (allocated(error)) return

        ! For features, get platform configuration (optional for packages)
        if (.not. present(name)) then
            call get_value(table, "platform", child, requested=.false., stat=stat)
            if (stat == toml_stat%success .and. associated(child)) then
                call self%platform%load_from_toml(child, error)
                if (allocated(error)) return
            end if
        end if

    end subroutine new_feature

    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        integer :: ikey

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Feature table is empty")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in feature table")
                exit

            ! Keys
            case("description", "platform", "flags", "c-flags", &
                 "cxx-flags", "link-time-flags", "preprocessor", "requires", &
                 "build", "install", "fortran", "library", "dependencies", &
                 "dev-dependencies", "executable", "example", "test", "preprocess")
                 
                 continue
                 
             ! OS names (lowercase)
             case("linux", "macos", "windows", "cygwin", "solaris", "freebsd", "openbsd")
                
                 continue 
                 
             ! Compiler names  
             case ("gfortran", "f95", "caf", "ifort", "ifx", "pgfortran", "nvfortran", "nagfor", &
                   "flang", "flang-new", "f18", "xlf90", "lfortran")
                 
                 continue
                 
             ! Standard feature configuration names
             case("debug", "release")  
                 
                 continue
                 
            end select
        end do

    end subroutine check

    !> Construct new feature array from a TOML data structure
    subroutine new_features(features, table, root, error)

        !> Instance of the feature configuration array
        type(feature_config_t), allocatable, intent(out) :: features(:)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(len=*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: node
        type(toml_key), allocatable :: list(:)
        integer :: ifeature, stat

        call table%get_keys(list)

        if (size(list) < 1) then
            allocate(features(0))
            return
        end if

        allocate(features(size(list)))

        do ifeature = 1, size(list)
            call get_value(table, list(ifeature)%key, node, stat=stat)
            if (stat /= toml_stat%success) then
                call fatal_error(error, "Feature "//list(ifeature)%key//" must be a table entry")
                exit
            end if
            call new_feature(features(ifeature), node, root, error)
            if (allocated(error)) exit
        end do

    end subroutine new_features

    !> Find matching feature configuration (similar to find_profile)
    subroutine find_feature(features, feature_name, current_platform, found, chosen_feature)
        type(feature_config_t), allocatable, intent(in) :: features(:)
        character(*), intent(in) :: feature_name
        type(platform_config_t), intent(in) :: current_platform
        logical, intent(out) :: found
        type(feature_config_t), intent(out) :: chosen_feature
        
        integer :: i
        
        found = .false.
        if (size(features) < 1) return
        
        ! Try to find exact match (feature + compiler + OS)
        do i = 1, size(features)
            if (features(i)%name == feature_name .and. &
                features(i)%platform%matches(current_platform)) then
                chosen_feature = features(i)
                found = .true.
                return
            end if
        end do
        
        ! Try to find compiler match with OS_ALL
        do i = 1, size(features) 
            if (features(i)%name == feature_name .and. &
                features(i)%platform%matches(current_platform)) then
                chosen_feature = features(i)
                found = .true.
                return
            end if
        end do
        
        ! Try to find COMPILER_ALL match
        do i = 1, size(features)
            if (features(i)%name == feature_name .and. &
                features(i)%platform%matches(current_platform)) then
                chosen_feature = features(i) 
                found = .true.
                return
            end if
        end do
    end subroutine find_feature

    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the feature configuration
        class(feature_config_t), intent(in) :: self

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

        write(unit, fmt) "Feature"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if
        if (allocated(self%description)) then
            write(unit, fmt) "- description", self%description
        end if

        call self%platform%info(unit, verbosity)

        if (allocated(self%flags)) then
            write(unit, fmt) "- flags", self%flags
        end if
        if (allocated(self%c_flags)) then
            write(unit, fmt) "- c-flags", self%c_flags
        end if
        if (allocated(self%cxx_flags)) then
            write(unit, fmt) "- cxx-flags", self%cxx_flags
        end if
        if (allocated(self%link_time_flags)) then
            write(unit, fmt) "- link-time-flags", self%link_time_flags
        end if

        if (allocated(self%build)) then
            call self%build%info(unit, pr - 1)
        end if
        if (allocated(self%install)) then
            call self%install%info(unit, pr - 1)
        end if

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

    end subroutine info

    !> Check that two feature configs are equal
    logical function feature_is_same(this, that)
        class(feature_config_t), intent(in) :: this
        class(serializable_t), intent(in) :: that

        integer :: ii

        feature_is_same = .false.

        select type (other=>that)
            type is (feature_config_t)
                
            if (allocated(this%name).neqv.allocated(other%name)) return
            if (allocated(this%name)) then
                if (.not.(this%name==other%name)) return
            end if
            
            if (allocated(this%description).neqv.allocated(other%description)) return
            if (allocated(this%description)) then
                if (.not.(this%description==other%description)) return
            end if
            
            if (.not.this%platform == other%platform) return
            
            if (allocated(this%build).neqv.allocated(other%build)) return
            if (allocated(this%build)) then
                if (.not.(this%build==other%build)) return
            end if
            
            if (allocated(this%install).neqv.allocated(other%install)) return
            if (allocated(this%install)) then
                if (.not.(this%install==other%install)) return
            end if
            
            if (allocated(this%fortran).neqv.allocated(other%fortran)) return
            if (allocated(this%fortran)) then
                if (.not.(this%fortran==other%fortran)) return
            end if
            
            if (allocated(this%library).neqv.allocated(other%library)) return
            if (allocated(this%library)) then
                if (.not.(this%library==other%library)) return
            end if
            
            if (allocated(this%executable).neqv.allocated(other%executable)) return
            if (allocated(this%executable)) then
                if (.not.(size(this%executable)==size(other%executable))) return
                do ii = 1, size(this%executable)
                    if (.not.(this%executable(ii)==other%executable(ii))) return
                end do
            end if
            
            if (allocated(this%dependency).neqv.allocated(other%dependency)) return
            if (allocated(this%dependency)) then
                if (.not.(size(this%dependency)==size(other%dependency))) return
                do ii = 1, size(this%dependency)
                    if (.not.(this%dependency(ii)==other%dependency(ii))) return
                end do
            end if
            
            if (allocated(this%dev_dependency).neqv.allocated(other%dev_dependency)) return
            if (allocated(this%dev_dependency)) then
                if (.not.(size(this%dev_dependency)==size(other%dev_dependency))) return
                do ii = 1, size(this%dev_dependency)
                    if (.not.(this%dev_dependency(ii)==other%dev_dependency(ii))) return
                end do
            end if
            
            if (allocated(this%example).neqv.allocated(other%example)) return
            if (allocated(this%example)) then
                if (.not.(size(this%example)==size(other%example))) return
                do ii = 1, size(this%example)
                    if (.not.(this%example(ii)==other%example(ii))) return
                end do
            end if

            if (allocated(this%test).neqv.allocated(other%test)) return
            if (allocated(this%test)) then
                if (.not.(size(this%test)==size(other%test))) return
                do ii = 1, size(this%test)
                    if (.not.(this%test(ii)==other%test(ii))) return
                end do
            end if
            
            if (allocated(this%preprocess).neqv.allocated(other%preprocess)) return
            if (allocated(this%preprocess)) then
                if (.not.(size(this%preprocess)==size(other%preprocess))) return
                do ii = 1, size(this%preprocess)
                    if (.not.(this%preprocess(ii)==other%preprocess(ii))) return
                end do
            end if
            
            if (allocated(this%flags).neqv.allocated(other%flags)) return
            if (allocated(this%flags)) then
                if (.not.(this%flags==other%flags)) return
            end if
            
            if (allocated(this%c_flags).neqv.allocated(other%c_flags)) return
            if (allocated(this%c_flags)) then
                if (.not.(this%c_flags==other%c_flags)) return
            end if
            
            if (allocated(this%cxx_flags).neqv.allocated(other%cxx_flags)) return
            if (allocated(this%cxx_flags)) then
                if (.not.(this%cxx_flags==other%cxx_flags)) return
            end if
            
            if (allocated(this%link_time_flags).neqv.allocated(other%link_time_flags)) return
            if (allocated(this%link_time_flags)) then
                if (.not.(this%link_time_flags==other%link_time_flags)) return
            end if
            
            if (allocated(this%requires_features).neqv.allocated(other%requires_features)) return
            if (allocated(this%requires_features)) then
                if (.not.(size(this%requires_features)==size(other%requires_features))) return
                do ii = 1, size(this%requires_features)
                    if (.not.(this%requires_features(ii)==other%requires_features(ii))) return
                end do
            end if
            
            if (.not.this%meta==other%meta) return
            
            class default
                return
        end select

        feature_is_same = .true.

    end function feature_is_same

    !> Dump feature to toml table
    subroutine dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(feature_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: ii
        type(toml_table), pointer :: ptr, ptr_pkg
        character(30) :: unnamed

        call set_string(table, "name", self%name, error, class_name)
        if (allocated(error)) return
        call set_string(table, "description", self%description, error, class_name)
        if (allocated(error)) return
        
        call add_table(table, "platform", ptr, error, class_name)
        if (allocated(error)) return
        call self%platform%dump_to_toml(ptr, error)
        if (allocated(error)) return        

        call set_string(table, "flags", self%flags, error, class_name)
        if (allocated(error)) return
        call set_string(table, "c-flags", self%c_flags, error, class_name)
        if (allocated(error)) return
        call set_string(table, "cxx-flags", self%cxx_flags, error, class_name)
        if (allocated(error)) return
        call set_string(table, "link-time-flags", self%link_time_flags, error, class_name)
        if (allocated(error)) return

        call set_list(table, "requires", self%requires_features, error)
        if (allocated(error)) return

        if (allocated(self%build)) then
            call add_table(table, "build", ptr, error, class_name)
            if (allocated(error)) return
            call self%build%dump_to_toml(ptr, error)
            if (allocated(error)) return
        end if

        if (allocated(self%install)) then
            call add_table(table, "install", ptr, error, class_name)
            if (allocated(error)) return
            call self%install%dump_to_toml(ptr, error)
            if (allocated(error)) return
        end if

        if (allocated(self%fortran)) then
            call add_table(table, "fortran", ptr, error, class_name)
            if (allocated(error)) return
            call self%fortran%dump_to_toml(ptr, error)
            if (allocated(error)) return
        end if

        if (allocated(self%library)) then
            call add_table(table, "library", ptr, error, class_name)
            if (allocated(error)) return
            call self%library%dump_to_toml(ptr, error)
            if (allocated(error)) return
        end if

        if (allocated(self%executable)) then
            call add_table(table, "executable", ptr_pkg)
            if (.not. associated(ptr_pkg)) then
                call fatal_error(error, class_name//" cannot create 'executable' table ")
                return
            end if

            do ii = 1, size(self%executable)
                associate (pkg => self%executable(ii))

                    !> Because dependencies are named, fallback if this has no name
                    !> So, serialization will work regardless of size(self%dep) == self%ndep                    
                    
                    if (len_trim(pkg%name)==0) then
                        write(unnamed,1) 'EXECUTABLE',ii
                        call add_table(ptr_pkg, trim(unnamed), ptr, error, class_name//'(executable)')
                    else
                        call add_table(ptr_pkg, pkg%name, ptr, error, class_name//'(executable)')
                    end if
                    if (allocated(error)) return
                    call pkg%dump_to_toml(ptr, error)
                    if (allocated(error)) return
                end associate
            end do
        end if

        if (allocated(self%dependency)) then
            call add_table(table, "dependencies", ptr_pkg)
            if (.not. associated(ptr_pkg)) then
                call fatal_error(error, class_name//" cannot create 'dependencies' table ")
                return
            end if

            do ii = 1, size(self%dependency)
                associate (pkg => self%dependency(ii))
                    if (len_trim(pkg%name)==0) then
                        write(unnamed,1) 'DEPENDENCY',ii
                        call add_table(ptr_pkg, trim(unnamed), ptr, error, class_name//'(dependencies)')
                    else
                        call add_table(ptr_pkg, pkg%name, ptr, error, class_name//'(dependencies)')
                    end if
                    if (allocated(error)) return
                    call pkg%dump_to_toml(ptr, error)
                    if (allocated(error)) return
                end associate
            end do
        end if

       if (allocated(self%example)) then

           call add_table(table, "example", ptr_pkg)
           if (.not. associated(ptr_pkg)) then
              call fatal_error(error, class_name//" cannot create 'example' table ")
              return
           end if

           do ii = 1, size(self%example)

              associate (pkg => self%example(ii))

                 !> Because dependencies are named, fallback if this has no name
                 !> So, serialization will work regardless of size(self%dep) == self%ndep
                 if (len_trim(pkg%name)==0) then
                    write(unnamed,1) 'EXAMPLE',ii
                    call add_table(ptr_pkg, trim(unnamed), ptr, error, class_name//'(example)')
                 else
                    call add_table(ptr_pkg, pkg%name, ptr, error, class_name//'(example)')
                 end if
                 if (allocated(error)) return
                 call pkg%dump_to_toml(ptr, error)
                 if (allocated(error)) return

              end associate

           end do
       end if

       if (allocated(self%test)) then

           call add_table(table, "test", ptr_pkg)
           if (.not. associated(ptr_pkg)) then
              call fatal_error(error, class_name//" cannot create 'test' table ")
              return
           end if

           do ii = 1, size(self%test)

              associate (pkg => self%test(ii))

                 !> Because dependencies are named, fallback if this has no name
                 !> So, serialization will work regardless of size(self%dep) == self%ndep
                 if (len_trim(pkg%name)==0) then
                    write(unnamed,1) 'TEST',ii
                    call add_table(ptr_pkg, trim(unnamed), ptr, error, class_name//'(test)')
                 else
                    call add_table(ptr_pkg, pkg%name, ptr, error, class_name//'(test)')
                 end if
                 if (allocated(error)) return
                 call pkg%dump_to_toml(ptr, error)
                 if (allocated(error)) return

              end associate

           end do
       end if

       if (allocated(self%preprocess)) then

           call add_table(table, "preprocess", ptr_pkg)
           if (.not. associated(ptr_pkg)) then
              call fatal_error(error, class_name//" cannot create 'preprocess' table ")
              return
           end if

           do ii = 1, size(self%preprocess)

              associate (pkg => self%preprocess(ii))

                 !> Because dependencies are named, fallback if this has no name
                 !> So, serialization will work regardless of size(self%dep) == self%ndep
                 if (len_trim(pkg%name)==0) then
                    write(unnamed,1) 'PREPROCESS',ii
                    call add_table(ptr_pkg, trim(unnamed), ptr, error, class_name//'(preprocess)')
                 else
                    call add_table(ptr_pkg, pkg%name, ptr, error, class_name//'(preprocess)')
                 end if
                 if (allocated(error)) return
                 call pkg%dump_to_toml(ptr, error)
                 if (allocated(error)) return

              end associate

           end do
       end if

       call add_table(table, "metapackages", ptr, error, class_name)
       if (allocated(error)) return
       call self%meta%dump_to_toml(ptr, error)
       if (allocated(error)) return

       1 format('UNNAMED_',a,'_',i0)

    end subroutine dump_to_toml

    !> Read feature from toml table (no checks made at this stage)
    subroutine load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(feature_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: keys(:), pkg_keys(:)
        integer :: ii, jj
        type(toml_table), pointer :: ptr, ptr_pkg

        call table%get_keys(keys)

        call get_value(table, "name", self%name)
        call get_value(table, "description", self%description)

        call get_value(table, "flags", self%flags)
        call get_value(table, "c-flags", self%c_flags)
        call get_value(table, "cxx-flags", self%cxx_flags)
        call get_value(table, "link-time-flags", self%link_time_flags)

        call get_list(table, "requires", self%requires_features, error)
        if (allocated(error)) return

        if (allocated(self%executable)) deallocate(self%executable)
        if (allocated(self%dependency)) deallocate(self%dependency)
        if (allocated(self%dev_dependency)) deallocate(self%dev_dependency)
        if (allocated(self%example)) deallocate(self%example)
        if (allocated(self%test)) deallocate(self%test)
        if (allocated(self%preprocess)) deallocate(self%preprocess)

        do ii = 1, size(keys)
            select case (keys(ii)%key)
                case ("platform")
                    
                    call get_value(table, keys(ii), ptr)
                    if (.not.associated(ptr)) then
                        call fatal_error(error,class_name//': error retrieving '//keys(ii)%key//' table')
                        return
                    end if
                    call self%platform%load_from_toml(ptr, error)
                    if (allocated(error)) return                    
                    
                case ("build")
                    allocate(self%build)
                    call get_value(table, keys(ii), ptr)
                    if (.not.associated(ptr)) then
                        call fatal_error(error,class_name//': error retrieving '//keys(ii)%key//' table')
                        return
                    end if
                    call self%build%load_from_toml(ptr, error)
                    if (allocated(error)) return

                case ("install")
                    allocate(self%install)
                    call get_value(table, keys(ii), ptr)
                    if (.not.associated(ptr)) then
                        call fatal_error(error,class_name//': error retrieving '//keys(ii)%key//' table')
                        return
                    end if
                    call self%install%load_from_toml(ptr, error)

                case ("fortran")
                    allocate(self%fortran)
                    call get_value(table, keys(ii), ptr)
                    if (.not.associated(ptr)) then
                        call fatal_error(error,class_name//': error retrieving '//keys(ii)%key//' table')
                        return
                    end if
                    call self%fortran%load_from_toml(ptr, error)

                case ("library")
                    allocate(self%library)
                    call get_value(table, keys(ii), ptr)
                    if (.not.associated(ptr)) then
                        call fatal_error(error,class_name//': error retrieving '//keys(ii)%key//' table')
                        return
                    end if
                    call self%library%load_from_toml(ptr, error)

              case ("executable")

                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving executable table')
                      return
                   end if

                   !> Read all packages
                   call ptr%get_keys(pkg_keys)
                   allocate(self%executable(size(pkg_keys)))

                   do jj = 1, size(pkg_keys)
                      call get_value(ptr, pkg_keys(jj), ptr_pkg)
                      call self%executable(jj)%load_from_toml(ptr_pkg, error)
                      if (allocated(error)) return
                   end do

              case ("dependencies")

                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving dependency table')
                      return
                   end if

                   !> Read all packages
                   call ptr%get_keys(pkg_keys)
                   allocate(self%dependency(size(pkg_keys)))

                   do jj = 1, size(pkg_keys)
                      call get_value(ptr, pkg_keys(jj), ptr_pkg)
                      call self%dependency(jj)%load_from_toml(ptr_pkg, error)
                      if (allocated(error)) return
                   end do

              case ("dev-dependencies")

                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving dev-dependencies table')
                      return
                   end if

                   !> Read all packages
                   call ptr%get_keys(pkg_keys)
                   allocate(self%dev_dependency(size(pkg_keys)))

                   do jj = 1, size(pkg_keys)
                      call get_value(ptr, pkg_keys(jj), ptr_pkg)
                      call self%dev_dependency(jj)%load_from_toml(ptr_pkg, error)
                      if (allocated(error)) return
                   end do

              case ("example")

                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving example table')
                      return
                   end if

                   !> Read all packages
                   call ptr%get_keys(pkg_keys)
                   allocate(self%example(size(pkg_keys)))

                   do jj = 1, size(pkg_keys)
                      call get_value(ptr, pkg_keys(jj), ptr_pkg)
                      call self%example(jj)%load_from_toml(ptr_pkg, error)
                      if (allocated(error)) return
                   end do

              case ("test")

                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving test table')
                      return
                   end if

                   !> Read all packages
                   call ptr%get_keys(pkg_keys)
                   allocate(self%test(size(pkg_keys)))

                   do jj = 1, size(pkg_keys)
                      call get_value(ptr, pkg_keys(jj), ptr_pkg)
                      call self%test(jj)%load_from_toml(ptr_pkg, error)
                      if (allocated(error)) return
                   end do

              case ("preprocess")

                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving preprocess table')
                      return
                   end if

                   !> Read all packages
                   call ptr%get_keys(pkg_keys)
                   allocate(self%preprocess(size(pkg_keys)))

                   do jj = 1, size(pkg_keys)
                      call get_value(ptr, pkg_keys(jj), ptr_pkg)
                      call self%preprocess(jj)%load_from_toml(ptr_pkg, error)
                      if (allocated(error)) return
                   end do
                   
                case ("metapackages")
                    
                    call get_value(table, keys(ii), ptr)
                    if (.not.associated(ptr)) then
                        call fatal_error(error,class_name//': error retrieving '//keys(ii)%key//' table')
                        return
                    end if
                    call self%meta%load_from_toml(ptr, error)                    
                       
                case default
                    cycle
            end select
        end do

    end subroutine load_from_toml


      !> Initialize the feature components (shared between new_feature and new_package)
      subroutine init_feature_components(self, table, platform, root, error)
          type(feature_config_t), intent(inout) :: self
          type(toml_table), intent(inout) :: table
          type(platform_config_t), optional, intent(in) :: platform
          character(len=*), intent(in), optional :: root
          type(error_t), allocatable, intent(out) :: error

          type(toml_table), pointer :: child, node
          type(toml_array), pointer :: children
          integer :: ii, nn, stat

          ! Initialize platform with defaults 
          if (present(platform)) then 
              self%platform = platform
          else
              self%platform = platform_config_t(id_all,OS_ALL)  
          end if

          ! Get description
          call get_value(table, "description", self%description)

          ! Get compiler flags
          call get_value(table, "flags", self%flags)
          call get_value(table, "c-flags", self%c_flags)
          call get_value(table, "cxx-flags", self%cxx_flags)
          call get_value(table, "link-time-flags", self%link_time_flags)

          ! Get feature dependencies
          call get_list(table, "requires", self%requires_features, error)
          if (allocated(error)) return

          ! Get build configuration
          call get_value(table, "build", child, requested=.false., stat=stat)
          if (stat == toml_stat%success .and. associated(child)) then
              allocate(self%build)
              call new_build_config(self%build, child, self%name, error)
              if (allocated(error)) return
          end if

          ! Get install configuration
          call get_value(table, "install", child, requested=.false., stat=stat)
          if (stat == toml_stat%success .and. associated(child)) then
              allocate(self%install)
              call new_install_config(self%install, child, error)
              if (allocated(error)) return
          end if

          ! Get Fortran configuration
          call get_value(table, "fortran", child, requested=.false., stat=stat)
          if (stat == toml_stat%success .and. associated(child)) then
              allocate(self%fortran)
              call new_fortran_config(self%fortran, child, error)          
              if (allocated(error)) return
          end if
     
          ! Get library configuration
          call get_value(table, "library", child, requested=.false.)
          if (associated(child)) then
              allocate(self%library)
              call new_library(self%library, child, error)
              if (allocated(error)) return
          end if

          ! Get dependencies and metapackage dependencies
          call get_value(table, "dependencies", child, requested=.false.)
          if (associated(child)) then
              call new_dependencies(self%dependency, child, root, self%meta, error=error)
              if (allocated(error)) return
          end if

          ! Get development dependencies
          call get_value(table, "dev-dependencies", child, requested=.false.)
          if (associated(child)) then
              call new_dependencies(self%dev_dependency, child, root, error=error)
              if (allocated(error)) return
          end if

          ! Get executables
          call get_value(table, "executable", children, requested=.false.)
          if (associated(children)) then
              nn = len(children)
              allocate(self%executable(nn))
              do ii = 1, nn
                  call get_value(children, ii, node, stat=stat)
                  if (stat /= toml_stat%success) then
                      call fatal_error(error, "Could not retrieve executable from array entry")
                      exit
                  end if
                  call new_executable(self%executable(ii), node, error)
                  if (allocated(error)) exit
              end do
              if (allocated(error)) return
          end if

          ! Get examples
          call get_value(table, "example", children, requested=.false.)
          if (associated(children)) then
              nn = len(children)
              allocate(self%example(nn))
              do ii = 1, nn
                  call get_value(children, ii, node, stat=stat)
                  if (stat /= toml_stat%success) then
                      call fatal_error(error, "Could not retrieve example from array entry")
                      exit
                  end if
                  call new_example(self%example(ii), node, error)
                  if (allocated(error)) exit
              end do
              if (allocated(error)) return
          end if

          ! Get tests
          call get_value(table, "test", children, requested=.false.)
          if (associated(children)) then
              nn = len(children)
              allocate(self%test(nn))
              do ii = 1, nn
                  call get_value(children, ii, node, stat=stat)
                  if (stat /= toml_stat%success) then
                      call fatal_error(error, "Could not retrieve test from array entry")
                      exit
                  end if
                  call new_test(self%test(ii), node, error)
                  if (allocated(error)) exit
              end do
              if (allocated(error)) return
          end if

          ! Get preprocessors
          call get_value(table, "preprocess", child, requested=.false.)
          if (associated(child)) then
              call new_preprocessors(self%preprocess, child, error)
              if (allocated(error)) return
          end if

          ! Validate unique program names
          if (allocated(self%executable)) then
              call unique_programs(self%executable, error)
              if (allocated(error)) return
          end if

          if (allocated(self%example)) then
              call unique_programs(self%example, error)
              if (allocated(error)) return

              if (allocated(self%executable)) then
                  call unique_programs(self%executable, self%example, error)
                  if (allocated(error)) return
              end if
          end if

          if (allocated(self%test)) then
              call unique_programs(self%test, error)
              if (allocated(error)) return
          end if

      end subroutine init_feature_components

      !> Check whether or not the names in a set of executables are unique
      subroutine unique_programs1(executable, error)

          !> Array of executables
          class(executable_config_t), intent(in) :: executable(:)

          !> Error handling
          type(error_t), allocatable, intent(out) :: error

          integer :: i, j

          do i = 1, size(executable)
              do j = 1, i - 1
                  if (executable(i)%name == executable(j)%name) then
                      call fatal_error(error, "The program named '"//&
                          executable(j)%name//"' is duplicated. "//&
                          "Unique program names are required.")
                      exit
                  end if
              end do
          end do
          if (allocated(error)) return

      end subroutine unique_programs1


      !> Check whether or not the names in a set of executables are unique
      subroutine unique_programs2(executable_i, executable_j, error)

          !> Array of executables
          class(executable_config_t), intent(in) :: executable_i(:)

          !> Array of executables
          class(executable_config_t), intent(in) :: executable_j(:)

          !> Error handling
          type(error_t), allocatable, intent(out) :: error

          integer :: i, j

          do i = 1, size(executable_i)
              do j = 1, size(executable_j)
                  if (executable_i(i)%name == executable_j(j)%name) then
                      call fatal_error(error, "The program named '"//&
                          executable_j(j)%name//"' is duplicated. "//&
                          "Unique program names are required.")
                      exit
                  end if
              end do
          end do
          if (allocated(error)) return

      end subroutine unique_programs2
      
      !> Return a name string as it would appear in the TOML manifest
      function manifest_name(self) result(name)
          class(feature_config_t), intent(in) :: self
          character(:), allocatable :: name
          
          character(:), allocatable :: platform
          
          platform = self%platform%name()
          
          if (len(platform)>0) then 
              name = self%name//'.'//platform
          else  
              name = self%name
          end if
          
      end function manifest_name
      
      !> Check if there is a CPP preprocessor configuration
      elemental logical function has_cpp(self) 
          class(feature_config_t), intent(in) :: self
          
          integer :: i
          
          has_cpp = .false.
          if (.not.allocated(self%preprocess)) return
          
          do i=1,size(self%preprocess)
              has_cpp = self%preprocess(i)%is_cpp()
              if (has_cpp) return
          end do
          
      end function has_cpp

end module fpm_manifest_feature
