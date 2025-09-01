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
    use fpm_manifest_profile, only : profile_config_t, new_profiles
    use fpm_manifest_example, only : example_config_t, new_example
    use fpm_manifest_executable, only : executable_config_t, new_executable
    use fpm_manifest_fortran, only : fortran_config_t, new_fortran_config
    use fpm_manifest_library, only : library_config_t, new_library
    use fpm_manifest_install, only: install_config_t, new_install_config
    use fpm_manifest_test, only : test_config_t, new_test
    use fpm_manifest_preprocess, only : preprocess_config_t, new_preprocessors
    use fpm_manifest_metapackages, only: metapackage_config_t, new_meta_config
    use fpm_manifest_feature, only: feature_config_t, new_features, get_default_features
    use fpm_filesystem, only : exists, getline, join_path
    use fpm_error, only : error_t, fatal_error, syntax_error, bad_name_error
    use tomlf, only : toml_table, toml_array, toml_key, toml_stat
    use fpm_toml, only : get_value, len, serializable_t, set_value, set_string, set_list, add_table
    use fpm_versioning, only : version_t, new_version
    
    implicit none
    private

    public :: package_config_t, new_package


    interface unique_programs
        module procedure :: unique_programs1
        module procedure :: unique_programs2
    end interface unique_programs


    !> Package meta data
    !> Package configuration data - extends a `feature_config_t` to represent the "default" 
    !> package feature. The following are now inherited from feature_config_t: name (but for package 
    !> it's the package name), description, compiler, os_type (defaults to id_all, OS_ALL for packages)
    !> library, executable(:), dependency(:), dev_dependency(:), example(:), test(:), preprocess(:)
    !> flags, c_flags, cxx_flags, link_time_flags, requires_features(:)

    type, extends(feature_config_t) :: package_config_t

        !> Package version (name is inherited from feature_config_t%name)
        type(version_t) :: version

        !> Metapackage data (package-specific)
        type(metapackage_config_t) :: meta

        !> Package metadata (package-specific)  
        character(len=:), allocatable :: license
        character(len=:), allocatable :: author
        character(len=:), allocatable :: maintainer
        character(len=:), allocatable :: copyright

        !> Additional features beyond the default package feature
        type(feature_config_t), allocatable :: features(:)

        !> Profiles (collections of features)
        type(profile_config_t), allocatable :: profiles(:)

    contains

        !> Print information on this instance
        procedure :: info

        !> Serialization interface
        procedure :: serializable_is_same => manifest_is_same
        procedure :: dump_to_toml
        procedure :: load_from_toml

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

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve package name")
           return
        end if
        if (bad_name_error(error,'package',self%name))then
           return
        endif

        call get_value(table, "license", self%license)
        call get_value(table, "author", self%author)
        call get_value(table, "maintainer", self%maintainer)
        call get_value(table, "copyright", self%copyright)

        if (len(self%name) <= 0) then
            call syntax_error(error, "Package name must be a non-empty string")
            return
        end if

        ii = scan(self%name, invalid_chars)
        if (ii > 0) then
            call syntax_error(error, "Package name contains invalid characters")
            return
        end if

        call get_value(table, "build", child, requested=.true., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error, "Type mismatch for build entry, must be a table")
            return
        end if
        call new_build_config(self%build, child, self%name, error)
        if (allocated(error)) return

        call get_value(table, "install", child, requested=.true., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error, "Type mismatch for install entry, must be a table")
            return
        end if
        call new_install_config(self%install, child, error)
        if (allocated(error)) return

        call get_value(table, "fortran", child, requested=.true., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error, "Type mismatch for fortran entry, must be a table")
            return
        end if
        call new_fortran_config(self%fortran, child, error)
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

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, root, self%meta, error)
            if (allocated(error)) return
        end if

        call get_value(table, "dev-dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dev_dependency, child, root, error=error)
            if (allocated(error)) return
        end if

        call get_value(table, "library", child, requested=.false.)
        if (associated(child)) then
            allocate(self%library)
            call new_library(self%library, child, error)
            if (allocated(error)) return
        end if

        call get_value(table, "profiles", child, requested=.false.)
        if (associated(child)) then
            call new_profiles(self%profiles, child, error)
            if (allocated(error)) return
        else
            ! Leave profiles unallocated for now
            allocate(self%profiles(0))
        end if

        call get_value(table, "features", child, requested=.false.)
        if (associated(child)) then
            call new_features(self%features, child, error=error)
            if (allocated(error)) return
        else
            ! Initialize with default features (converted from old default profiles)
            call get_default_features(self%features, error)
            if (allocated(error)) return
        end if

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

            call unique_programs(self%executable, error)
            if (allocated(error)) return
        end if

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

            call unique_programs(self%example, error)
            if (allocated(error)) return

            if (allocated(self%executable)) then
                call unique_programs(self%executable, self%example, error)
                if (allocated(error)) return
            end if
        end if

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

            call unique_programs(self%test, error)
            if (allocated(error)) return
        end if

        call get_value(table, "preprocess", child, requested=.false.)
        if (associated(child)) then
            call new_preprocessors(self%preprocess, child, error)
            if (allocated(error)) return
        end if
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

   logical function manifest_is_same(this,that)
      class(package_config_t), intent(in) :: this
      class(serializable_t), intent(in) :: that

      integer :: ii

      manifest_is_same = .false.

      select type (other=>that)
         type is (package_config_t)
         if (allocated(this%name).neqv.allocated(other%name)) return
            if (allocated(this%name) .and. allocated(other%name)) then
                if (.not.this%name==other%name) return
            end if
            if (.not.this%version==other%version) return
            if (.not.this%build==other%build) return
            if (.not.this%install==other%install) return
            if (.not.this%fortran==other%fortran) return
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
            if (allocated(this%library).neqv.allocated(other%library)) return
            if (allocated(this%library)) then
                if (.not.this%library==other%library) return
            endif
            if (allocated(this%executable).neqv.allocated(other%executable)) return
            if (allocated(this%executable)) then
                if (.not.size(this%executable)==size(other%executable)) return
                do ii=1,size(this%executable)
                    if (.not.this%executable(ii)==other%executable(ii)) return
                end do
            end if
            if (allocated(this%dependency).neqv.allocated(other%dependency)) return
            if (allocated(this%dependency)) then
                if (.not.size(this%dependency)==size(other%dependency)) return
                do ii=1,size(this%dependency)
                    if (.not.this%dependency(ii)==other%dependency(ii)) return
                end do
            end if
            if (allocated(this%dev_dependency).neqv.allocated(other%dev_dependency)) return
            if (allocated(this%dev_dependency)) then
                if (.not.size(this%dev_dependency)==size(other%dev_dependency)) return
                do ii=1,size(this%dev_dependency)
                    if (.not.this%dev_dependency(ii)==other%dev_dependency(ii)) return
                end do
            end if
            if (allocated(this%profiles).neqv.allocated(other%profiles)) return
            if (allocated(this%profiles)) then
                if (.not.size(this%profiles)==size(other%profiles)) return
                do ii=1,size(this%profiles)
                    if (.not.this%profiles(ii)==other%profiles(ii)) return
                end do
            end if
            if (allocated(this%example).neqv.allocated(other%example)) return
            if (allocated(this%example)) then
                if (.not.size(this%example)==size(other%example)) return
                do ii=1,size(this%example)
                    if (.not.this%example(ii)==other%example(ii)) return
                end do
            end if
            if (allocated(this%preprocess).neqv.allocated(other%preprocess)) return
            if (allocated(this%preprocess)) then
                if (.not.size(this%preprocess)==size(other%preprocess)) return
                do ii=1,size(this%preprocess)
                    if (.not.this%preprocess(ii)==other%preprocess(ii)) return
                end do
            end if
            if (allocated(this%test).neqv.allocated(other%test)) return
            if (allocated(this%test)) then
                if (.not.size(this%test)==size(other%test)) return
                do ii=1,size(this%test)
                    if (.not.this%test(ii)==other%test(ii)) return
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

       call set_string(table, "name", self%name, error, class_name)
       if (allocated(error)) return
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

       call add_table(table, "build", ptr, error, class_name)
       if (allocated(error)) return
       call self%build%dump_to_toml(ptr, error)
       if (allocated(error)) return

       call add_table(table, "fortran", ptr, error, class_name)
       if (allocated(error)) return
       call self%fortran%dump_to_toml(ptr, error)
       if (allocated(error)) return

       call add_table(table, "install", ptr, error, class_name)
       if (allocated(error)) return
       call self%install%dump_to_toml(ptr, error)
       if (allocated(error)) return

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

                 !> Because dependencies are named, fallback if this has no name
                 !> So, serialization will work regardless of size(self%dep) == self%ndep
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

       if (allocated(self%dev_dependency)) then

           call add_table(table, "dev-dependencies", ptr_pkg)
           if (.not. associated(ptr_pkg)) then
              call fatal_error(error, class_name//" cannot create 'dev-dependencies' table ")
              return
           end if

           do ii = 1, size(self%dev_dependency)

              associate (pkg => self%dev_dependency(ii))

                 !> Because dependencies are named, fallback if this has no name
                 !> So, serialization will work regardless of size(self%dep) == self%ndep
                 if (len_trim(pkg%name)==0) then
                    write(unnamed,1) 'DEV-DEPENDENCY',ii
                    call add_table(ptr_pkg, trim(unnamed), ptr, error, class_name//'(dev-dependencies)')
                 else
                    call add_table(ptr_pkg, pkg%name, ptr, error, class_name//'(dev-dependencies)')
                 end if
                 if (allocated(error)) return
                 call pkg%dump_to_toml(ptr, error)
                 if (allocated(error)) return

              end associate

           end do
       end if

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

       1 format('UNNAMED_',a,'_',i0)
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

        call table%get_keys(keys)

        call get_value(table, "name", self%name)
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

        if (allocated(self%library)) deallocate(self%library)
        if (allocated(self%executable)) deallocate(self%executable)
        if (allocated(self%dependency)) deallocate(self%dependency)
        if (allocated(self%dev_dependency)) deallocate(self%dev_dependency)
        if (allocated(self%profiles)) deallocate(self%profiles)
        if (allocated(self%example)) deallocate(self%example)
        if (allocated(self%test)) deallocate(self%test)
        if (allocated(self%preprocess)) deallocate(self%preprocess)
        sub_deps: do ii = 1, size(keys)

           select case (keys(ii)%key)
              case ("build")
                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving '//keys(ii)%key//' table')
                      return
                   end if
                   call self%build%load_from_toml(ptr, error)
                   if (allocated(error)) return

              case ("install")
                   call get_value(table, keys(ii), ptr)
                   if (.not.associated(ptr)) then
                      call fatal_error(error,class_name//': error retrieving '//keys(ii)%key//' table')
                      return
                   end if
                   call self%install%load_from_toml(ptr, error)

              case ("fortran")
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

              case default
                    cycle sub_deps
           end select

        end do sub_deps

     end subroutine load_from_toml


end module fpm_manifest_package
