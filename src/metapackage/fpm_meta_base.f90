module fpm_meta_base
    use fpm_error, only: error_t, fatal_error
    use fpm_versioning, only: version_t
    use fpm_model, only: fpm_model_t, fortran_features_t
    use fpm_command_line, only: fpm_cmd_settings, fpm_run_settings
    use fpm_manifest_dependency, only: dependency_config_t
    use fpm_manifest, only: package_config_t
    use fpm_strings, only: string_t, len_trim, split, join
    use fpm_compiler, only: append_clean_flags, append_clean_flags_array

    implicit none

    private

    public :: destroy

    !> Type for describing a source file
    type, public :: metapackage_t
        
        !> Package name
        character(:), allocatable :: name

        !> Package version (if supported)
        type(version_t), allocatable :: version

        logical :: has_link_libraries   = .false.
        logical :: has_link_flags       = .false.
        logical :: has_build_flags      = .false.
        logical :: has_fortran_flags    = .false.
        logical :: has_c_flags          = .false.
        logical :: has_cxx_flags        = .false.
        logical :: has_include_dirs     = .false.
        logical :: has_dependencies     = .false.
        logical :: has_run_command      = .false.
        logical :: has_external_modules = .false.

        !> List of compiler flags and options to be added
        type(string_t) :: flags
        type(string_t) :: fflags
        type(string_t) :: cflags
        type(string_t) :: cxxflags
        type(string_t) :: link_flags
        type(string_t) :: run_command
        type(string_t), allocatable :: incl_dirs(:)
        type(string_t), allocatable :: link_libs(:)
        type(string_t), allocatable :: external_modules(:)

        !> Special fortran features
        type(fortran_features_t), allocatable :: fortran

        !> List of Development dependency meta data.
        !> Metapackage dependencies are never exported from the model
        type(dependency_config_t), allocatable :: dependency(:)

        contains

            !> Clean metapackage structure
            procedure :: destroy

            !> Add metapackage dependencies to the model
            procedure, private :: resolve_cmd
            procedure, private :: resolve_model
            procedure, private :: resolve_package_config
            generic :: resolve => resolve_cmd,resolve_model,resolve_package_config

    end type metapackage_t

    contains

    elemental subroutine destroy(this)
        class(metapackage_t), intent(inout) :: this
        this%has_link_libraries = .false.
        this%has_link_flags = .false.
        this%has_build_flags = .false.
        this%has_fortran_flags = .false.
        this%has_c_flags = .false.
        this%has_cxx_flags = .false.
        this%has_include_dirs = .false.
        this%has_dependencies = .false.
        this%has_run_command = .false.
        this%has_external_modules = .false.
        if (allocated(this%name)) deallocate(this%name)
        if (allocated(this%version)) deallocate(this%version)
        if (allocated(this%flags%s)) deallocate(this%flags%s)
        if (allocated(this%link_libs)) deallocate(this%link_libs)
        if (allocated(this%incl_dirs)) deallocate(this%incl_dirs)
        if (allocated(this%external_modules)) deallocate(this%external_modules)
    end subroutine destroy

    !> Resolve metapackage dependencies into the command line settings
    subroutine resolve_cmd(self,settings,error)
        class(metapackage_t), intent(in) :: self
        class(fpm_cmd_settings), intent(inout) :: settings
        type(error_t), allocatable, intent(out) :: error

        ! Add customize run commands
        if (self%has_run_command) then

            select type (cmd=>settings)
               class is (fpm_run_settings) ! includes fpm_test_settings

                  ! Only override runner if user has not provided a custom one
                  if (.not.len_trim(cmd%runner)>0) cmd%runner = self%run_command%s

            end select

        endif

    end subroutine resolve_cmd

    !> Resolve metapackage dependencies into the model
    subroutine resolve_model(self,model,error)
        class(metapackage_t), intent(in) :: self
        type(fpm_model_t), intent(inout) :: model
        type(error_t), allocatable, intent(out) :: error

        ! Add global build flags, to apply to all sources
        if (self%has_build_flags) then
            call append_clean_flags(model%fortran_compile_flags, self%flags%s)
            call append_clean_flags(model%c_compile_flags, self%flags%s)
            call append_clean_flags(model%cxx_compile_flags, self%flags%s)
        endif

        ! Add language-specific flags
        if (self%has_fortran_flags) call append_clean_flags(model%fortran_compile_flags, self%fflags%s)
        if (self%has_c_flags)       call append_clean_flags(model%c_compile_flags, self%cflags%s)
        if (self%has_cxx_flags)     call append_clean_flags(model%cxx_compile_flags, self%cxxflags%s)

        if (self%has_link_flags) then
            call append_clean_flags(model%link_flags, self%link_flags%s)
        end if

        if (self%has_link_libraries) then
            call append_clean_flags_array(model%link_libraries, self%link_libs)
        end if

        if (self%has_include_dirs) then
            call append_clean_flags_array(model%include_dirs, self%incl_dirs)
        end if

        if (self%has_external_modules) then
            call append_clean_flags_array(model%external_modules, self%external_modules)
        end if

    end subroutine resolve_model

    subroutine resolve_package_config(self,package,error)
        class(metapackage_t), intent(in) :: self
        type(package_config_t), intent(inout) :: package
        type(error_t), allocatable, intent(out) :: error

        ! All metapackage dependencies are added as dev-dependencies,
        ! as they may change if built upstream
        if (self%has_dependencies) then
            if (allocated(package%dev_dependency)) then
               package%dev_dependency = [package%dev_dependency,self%dependency]
            else
               package%dev_dependency = self%dependency
            end if
        end if

        ! Check if there are any special fortran requests which the package does not comply to
        if (allocated(self%fortran)) then

            if (self%fortran%implicit_external.neqv.package%fortran%implicit_external) then
                call fatal_error(error,'metapackage fortran error: metapackage '// &
                                       dn(self%fortran%implicit_external)//' require implicit-external, main package '//&
                                       dn(package%fortran%implicit_external))
                return
            end if

            if (self%fortran%implicit_typing.neqv.package%fortran%implicit_typing) then
                call fatal_error(error,'metapackage fortran error: metapackage '// &
                                       dn(self%fortran%implicit_external)//' require implicit-typing, main package '//&
                                       dn(package%fortran%implicit_external))
                return
            end if

        end if

        contains

        pure function dn(bool)
           logical, intent(in) :: bool
           character(len=:), allocatable :: dn
           if (bool) then
              dn = "does"
           else
              dn = "does not"
           end if
        end function dn

    end subroutine resolve_package_config

end module fpm_meta_base
