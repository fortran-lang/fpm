!> Implementation of the meta data for dependencies.
!>
!> A dependency table can currently have the following fields
!>
!>```toml
!>[dependencies]
!>"dep1" = { git = "url" }
!>"dep2" = { git = "url", branch = "name" }
!>"dep3" = { git = "url", tag = "name" }
!>"dep4" = { git = "url", rev = "sha1" }
!>"dep0" = { path = "path" }
!>"dep5" = { path = "path", features = ["feat1", "feat2"] }
!>"dep6" = { path = "path", profile = "myprofile" }
!>```
!>
!> The `features` and `profile` keys are mutually exclusive.
!> `features` provides a list of individual feature names;
!> `profile` provides a single profile name defined in the dependency's manifest.
!>
!> To reduce the amount of boilerplate code this module provides two constructors
!> for dependency types, one basic for an actual dependency (inline) table
!> and another to collect all dependency objects from a dependencies table,
!> which is handling the allocation of the objects and is forwarding the
!> individual dependency tables to their respective constructors.
!> The usual entry point should be the constructor for the super table.
!>
!> This objects contains a target to retrieve required `fpm` projects to
!> build the target declaring the dependency.
!> Resolving a dependency will result in obtaining a new package configuration
!> data for the respective project.
module fpm_manifest_dependency
    use fpm_error, only: error_t, syntax_error, fatal_error
    use fpm_git, only: git_target_t, git_target_tag, git_target_branch, &
        & git_target_revision, git_target_default, git_matches_manifest
    use tomlf, only: toml_table, toml_key, toml_stat
    use fpm_toml, only: get_value, check_keys, serializable_t, add_table, &
        & set_value, set_string, get_list, set_list
    use fpm_filesystem, only: windows_path, join_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use fpm_manifest_metapackages, only: metapackage_config_t, is_meta_package, new_meta_config, &
            metapackage_request_t, new_meta_request
    use fpm_versioning, only: version_t, new_version
    use fpm_strings, only: string_t
    use fpm_manifest_preprocess
    implicit none
    private

    public :: dependency_config_t, new_dependency, new_dependencies, manifest_has_changed, &
        & dependency_destroy, resize

    !> Configuration meta data for a dependency
    type, extends(serializable_t) :: dependency_config_t

        !> Name of the dependency
        character(len=:), allocatable :: name

        !> Local target
        character(len=:), allocatable :: path

        !> Namespace which the dependency belongs to.
        !> Enables multiple dependencies with the same name.
        !> Required for dependencies that are obtained via the official registry.
        character(len=:), allocatable :: namespace

        !> The requested version of the dependency.
        !> The latest version is used if not specified.
        type(version_t), allocatable :: requested_version

        !> Requested macros for the dependency
        type(preprocess_config_t), allocatable :: preprocess(:)
        
        !> Requested features for the dependency
        type(string_t), allocatable :: features(:)

        !> Requested profile for the dependency (mutually exclusive with features)
        character(len=:), allocatable :: profile

        !> Git descriptor
        type(git_target_t), allocatable :: git

    contains

        !> Print information on this instance
        procedure :: info
        
        !> Add a preprocessor configuration
        procedure :: add_preprocess

        !> Serialization interface
        procedure :: serializable_is_same => dependency_is_same
        procedure :: dump_to_toml
        procedure :: load_from_toml

    end type dependency_config_t

    !> Common output format for writing to the command line
    character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

    interface resize
        module procedure resize_dependency_config
    end interface resize

contains

    !> Construct a new dependency configuration from a TOML data structure
    subroutine new_dependency(self, table, root, error)

        !> Instance of the dependency configuration
        type(dependency_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: uri, value, requested_version

        type(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call table%get_key(self%name)
        call get_value(table, "namespace", self%namespace)

        call get_value(table, "v", requested_version)
        if (allocated(requested_version)) then
            if (.not. allocated(self%requested_version)) allocate (self%requested_version)
            call new_version(self%requested_version, requested_version, error)
            if (allocated(error)) return
        end if

        !> Get optional preprocessor directives
        call get_value(table, "preprocess", child, requested=.false.)
        if (associated(child)) then
            call new_preprocessors(self%preprocess, child, error)
            if (allocated(error)) return
        endif
        
        !> Get optional features list
        call get_list(table, "features", self%features, error)
        if (allocated(error)) return

        !> Get optional profile name
        call get_value(table, "profile", self%profile)

        call get_value(table, "path", uri)
        if (allocated(uri)) then
            if (get_os_type() == OS_WINDOWS) uri = windows_path(uri)
            if (present(root)) uri = join_path(root,uri)  ! Relative to the fpm.toml it’s written in
            call move_alloc(uri, self%path)
            return
        end if

        call get_value(table, "git", uri)
        if (allocated(uri)) then
            call get_value(table, "tag", value)
            if (allocated(value)) then
                self%git = git_target_tag(uri, value)
            end if

            if (.not. allocated(self%git)) then
                call get_value(table, "branch", value)
                if (allocated(value)) then
                    self%git = git_target_branch(uri, value)
                end if
            end if

            if (.not. allocated(self%git)) then
                call get_value(table, "rev", value)
                if (allocated(value)) then
                    self%git = git_target_revision(uri, value)
                end if
            end if

            if (.not. allocated(self%git)) then
                self%git = git_target_default(uri)
            end if
            return
        end if

    end subroutine new_dependency

    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: name
        type(string_t), allocatable :: string_list(:)
        type(toml_key), allocatable :: list(:)
        type(toml_table), pointer :: child

        !> List of valid keys for the dependency table.
        character(*), dimension(*), parameter :: valid_keys = [character(24) :: &
            & "namespace", &
              "v", &
              "path", &
              "git", &
              "tag", &
              "branch", &
              "rev", &
              "preprocess", &
              "features", &
              "profile" &
            & ]

        call table%get_key(name)
        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Dependency '"//name//"' does not provide sufficient entries")
            return
        end if

        call check_keys(table, valid_keys, error)
        if (allocated(error)) return

        if (table%has_key("path") .and. table%has_key("git")) then
            call syntax_error(error, "Dependency '"//name//"' cannot have both git and path entries")
            return
        end if

        if ((table%has_key("branch") .and. table%has_key("rev")) .or. &
            (table%has_key("branch") .and. table%has_key("tag")) .or. &
            (table%has_key("rev") .and. table%has_key("tag"))) then
            call syntax_error(error, "Dependency '"//name//"' can only have one of branch, rev or tag present")
            return
        end if

        if ((table%has_key("branch") .or. table%has_key("tag") .or. table%has_key("rev")) &
            .and. .not. table%has_key("git")) then
            call syntax_error(error, "Dependency '"//name//"' has git identifier but no git url")
            return
        end if

        if (.not. table%has_key("path") .and. .not. table%has_key("git") &
            .and. .not. table%has_key("namespace")) then
            call syntax_error(error, "Please provide a 'namespace' for dependency '"//name// &
            & "' if it is not a local path or git repository")
            return
        end if

        if (table%has_key('v') .and. (table%has_key('path') .or. table%has_key('git'))) then
            call syntax_error(error, "Dependency '"//name//"' cannot have both v and git/path entries")
            return
        end if

        ! Check preprocess key
        if (table%has_key('preprocess')) then

            call get_value(table, 'preprocess', child)

            if (.not.associated(child)) then
                call syntax_error(error, "Dependency '"//name//"' has invalid 'preprocess' entry")
                return
            end if

        end if

        ! Check that features and profile are not both specified
        if (table%has_key('features') .and. table%has_key('profile')) then
            call syntax_error(error, "Dependency '"//name//"' cannot have both 'features' and 'profile' entries")
            return
        end if

    end subroutine check

    !> Construct new dependency array from a TOML data structure
    subroutine new_dependencies(deps, table, root, meta, error)

        !> Instance of the dependency configuration
        type(dependency_config_t), allocatable, intent(out) :: deps(:)

        !> (optional) metapackages
        type(metapackage_config_t), optional, intent(out) :: meta

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: node
        type(toml_key), allocatable :: list(:)
        type(dependency_config_t), allocatable :: all_deps(:)
        type(metapackage_request_t) :: meta_request
        logical, allocatable :: is_meta(:)
        logical :: metapackages_allowed
        integer :: idep, stat, ndep

        call table%get_keys(list)
        ! An empty table is okay
        if (size(list) < 1) return

        !> Flag dependencies that should be treated as metapackages
        metapackages_allowed = present(meta)
        allocate(is_meta(size(list)),source=.false.)
        allocate(all_deps(size(list)))

        !> Parse all meta- and non-metapackage dependencies
        do idep = 1, size(list)

            ! Check if this is a standard dependency node
            call get_value(table, list(idep)%key, node, stat=stat)
            is_standard_dependency: if (stat /= toml_stat%success) then

                ! See if it can be a valid metapackage name
                call new_meta_request(meta_request, list(idep)%key, table, error=error)

                !> Neither a standard dep nor a metapackage
                if (allocated(error)) then
                   call syntax_error(error, "Dependency "//list(idep)%key//" is not a valid metapackage or a table entry")
                   return
                endif

                !> Valid meta dependency
                is_meta(idep) = .true.

            else

                ! Parse as a standard dependency
                is_meta(idep) = .false.

                call new_dependency(all_deps(idep), node, root, error)
                if (allocated(error)) return

            end if is_standard_dependency

        end do

        ! Non-meta dependencies
        ndep = count(.not.is_meta)

        ! Finalize standard dependencies
        allocate(deps(ndep))
        ndep = 0
        do idep = 1, size(list)
            if (is_meta(idep)) cycle
            ndep = ndep+1
            deps(ndep) = all_deps(idep)
        end do

        ! Finalize meta dependencies
        if (metapackages_allowed) call new_meta_config(meta,table,is_meta,error)

    end subroutine new_dependencies

    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the dependency configuration
        class(dependency_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ilink
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        write (unit, fmt) "Dependency"
        if (allocated(self%name)) then
            write (unit, fmt) "- name", self%name
        end if

        if (allocated(self%git)) then
            write (unit, fmt) "- kind", "git"
            call self%git%info(unit, pr - 1)
        end if

        if (allocated(self%path)) then
            write (unit, fmt) "- kind", "local"
            write (unit, fmt) "- path", self%path
        end if

       if (allocated(self%features)) then
          write(unit, fmt) " - features"
          do ilink = 1, size(self%features)
             write(unit, fmt) "   - " // self%features(ilink)%s
          end do
       end if

       if (allocated(self%profile)) then
          write(unit, fmt) " - profile", self%profile
       end if

    end subroutine info

    !> Check if two dependency configurations are different
    logical function manifest_has_changed(cached, manifest, verbosity, iunit) result(has_changed)

        !> Two instances of the dependency configuration
        class(dependency_config_t), intent(in) :: cached, manifest

        !> Log verbosity
        integer, intent(in) :: verbosity, iunit

        has_changed = .true.

        !> Perform all checks
        if (allocated(cached%git).neqv.allocated(manifest%git)) then
            if (verbosity>1) write(iunit,out_fmt) "GIT presence has changed. "
            return
        endif
        if (allocated(cached%git)) then
            if (.not.git_matches_manifest(cached%git,manifest%git,verbosity,iunit)) return
        end if

        !> All checks passed! The two instances are equal
        has_changed = .false.

    end function manifest_has_changed

    !> Clean memory
    elemental subroutine dependency_destroy(self)
        class(dependency_config_t), intent(inout) :: self

        if (allocated(self%name)) deallocate(self%name)
        if (allocated(self%path)) deallocate(self%path)
        if (allocated(self%namespace)) deallocate(self%namespace)
        if (allocated(self%requested_version)) deallocate(self%requested_version)
        if (allocated(self%git)) deallocate(self%git)
        if (allocated(self%features)) deallocate(self%features)
        if (allocated(self%profile)) deallocate(self%profile)

    end subroutine dependency_destroy

    !> Check that two dependency configs are equal
    logical function dependency_is_same(this,that)
        class(dependency_config_t), intent(in) :: this
        class(serializable_t), intent(in) :: that

        dependency_is_same = .false.

        select type (other=>that)
           type is (dependency_config_t)

              if (allocated(this%name).neqv.allocated(other%name)) return
              if (allocated(this%name)) then
                if (.not.(this%name==other%name)) return
              endif
              if (allocated(this%path).neqv.allocated(other%path)) return
              if (allocated(this%path)) then
                if (.not.(this%path==other%path)) return
              endif
              if (allocated(this%namespace).neqv.allocated(other%namespace)) return
              if (allocated(this%namespace)) then
                if (.not.(this%namespace==other%namespace)) return
              endif
              if (allocated(this%requested_version).neqv.allocated(other%requested_version)) return
              if (allocated(this%requested_version)) then
                if (.not.(this%requested_version==other%requested_version)) return
              endif
              if (allocated(this%features).neqv.allocated(other%features)) return
              if (allocated(this%features)) then
                if (.not.(this%features==other%features)) return
              endif
              if (allocated(this%profile).neqv.allocated(other%profile)) return
              if (allocated(this%profile)) then
                if (.not.(this%profile==other%profile)) return
              endif

              if ((allocated(this%git).neqv.allocated(other%git))) return
              if (allocated(this%git)) then
                if (.not.(this%git==other%git)) return
              endif

           class default
              ! Not the same type
              return
        end select

        !> All checks passed!
        dependency_is_same = .true.

    end function dependency_is_same

    !> Dump dependency to toml table
    subroutine dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(dependency_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(toml_table), pointer :: ptr
        type(error_t), allocatable, intent(out) :: error

        integer :: ierr

        call set_string(table, "name", self%name, error, 'dependency_config_t')
        if (allocated(error)) return
        call set_string(table, "path", self%path, error, 'dependency_config_t')
        if (allocated(error)) return
        call set_string(table, "namespace", self%namespace, error, 'dependency_config_t')
        if (allocated(error)) return
        if (allocated(self%requested_version)) then
             call set_string(table, "requested_version", self%requested_version%s(), error, 'dependency_config_t')
             if (allocated(error)) return
        endif
       call set_list(table, "features", self%features, error)
       if (allocated(error)) return
       call set_string(table, "profile", self%profile, error, 'dependency_config_t')
       if (allocated(error)) return

        if (allocated(self%git)) then
            call add_table(table, "git", ptr, error)
            if (allocated(error)) return
            call self%git%dump_to_toml(ptr, error)
            if (allocated(error)) return
        endif

    end subroutine dump_to_toml

    !> Read dependency from toml table (no checks made at this stage)
    subroutine load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(dependency_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Local variables
        type(toml_key), allocatable :: list(:)
        type(toml_table), pointer :: ptr
        character(len=:), allocatable :: requested_version
        integer :: ierr,ii

        call dependency_destroy(self)

        call get_value(table, "name", self%name)
        call get_value(table, "path", self%path)
        call get_value(table, "namespace", self%namespace)
        call get_value(table, "requested_version", requested_version)
        if (allocated(requested_version)) then
            allocate(self%requested_version)
            call new_version(self%requested_version, requested_version, error)
            if (allocated(error)) then
                error%message = 'dependency_config_t: version error from TOML table - '//error%message
                return
            endif
        end if
        call get_list(table, "features", self%features, error)
        if (allocated(error)) return
        call get_value(table, "profile", self%profile)

        call table%get_keys(list)
        add_git: do ii = 1, size(list)
            if (list(ii)%key=="git") then
               call get_value(table, list(ii)%key, ptr, stat=ierr)
               if (ierr /= toml_stat%success) then
                   call fatal_error(error,'dependency_config_t: cannot retrieve git from TOML table')
                   exit
               endif
               allocate(self%git)
               call self%git%load_from_toml(ptr, error)
               if (allocated(error)) return
               exit add_git
            end if
        end do add_git

    end subroutine load_from_toml

    !> Reallocate a list of dependencies
    pure subroutine resize_dependency_config(var, n)
        !> Instance of the array to be resized
        type(dependency_config_t), allocatable, intent(inout) :: var(:)
        !> Dimension of the final array size
        integer, intent(in), optional :: n

        type(dependency_config_t), allocatable :: tmp(:)
        integer :: this_size, new_size
        integer, parameter :: initial_size = 16

        if (allocated(var)) then
          this_size = size(var, 1)
          call move_alloc(var, tmp)
        else
          this_size = initial_size
        end if

        if (present(n)) then
          new_size = n
        else
          new_size = this_size + this_size/2 + 1
        end if

        allocate (var(new_size))

        if (allocated(tmp)) then
          this_size = min(size(tmp, 1), size(var, 1))
          var(:this_size) = tmp(:this_size)
          deallocate (tmp)
        end if

    end subroutine resize_dependency_config
    
    subroutine add_preprocess(dep, preprocess)
        !> Instance of the dependency config
        class(dependency_config_t), intent(inout) :: dep
        !> Instance of the preprocessor configuration
        type(preprocess_config_t), intent(in) :: preprocess
        
        integer :: i,n
        type(preprocess_config_t), allocatable :: new_preprocess(:)
        
        if (allocated(dep%preprocess)) then 
            
            n = size(dep%preprocess)
            
            if (n<1) then 
                deallocate(dep%preprocess)
                allocate(dep%preprocess(1),source=preprocess)
            else
                
                find_similar: do i=1,n
                    if (dep%preprocess(i)%name==dep%name) then                             
                        call dep%preprocess(i)%add_config(preprocess)
                        return                            
                    end if
                end do find_similar                   
                
                ! Similar preprocessor config not found: add a new one
                allocate(new_preprocess(n+1))
                new_preprocess(1:n) = dep%preprocess
                new_preprocess(n+1) = preprocess
                call move_alloc(from=new_preprocess,to=dep%preprocess)
                
            end if
        else
            
            ! Copy configuration
            allocate(dep%preprocess(1),source=preprocess)
            
        end if                
               
    end subroutine add_preprocess


end module fpm_manifest_dependency
