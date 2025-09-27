module fpm_meta_util
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_filesystem, only: join_path
    use fpm_strings, only: split, string_t, str_begins_with_str, add_strings
    use fpm_error, only: error_t
    use fpm_versioning, only: new_version
    use fpm_pkg_config, only: pkgcfg_get_libs, pkgcfg_get_build_flags, pkgcfg_get_version

    implicit none

    private

    public :: add_pkg_config_compile_options, lib_get_trailing, add_strings
    
    contains

    !> Add pkgconfig compile options to a metapackage
    subroutine add_pkg_config_compile_options(this, name, include_flag, libdir, error)
        class(metapackage_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: include_flag
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: libdir
        type(string_t) :: log, current_include_dir, current_lib
        type(string_t), allocatable :: libs(:), flags(:)
        integer :: i

        !> Get version
        if (.not. allocated(this%version)) then
            log = pkgcfg_get_version(name, error)
            if (allocated(error)) return
            allocate(this%version)
            call new_version(this%version, log%s, error)
            if (allocated(error)) return
        end if

        !> Get libraries
        libs = pkgcfg_get_libs(name, error)
        if (allocated(error)) return

        libdir = ""
        do i = 1, size(libs)
            if (str_begins_with_str(libs(i)%s, '-l')) then
                current_lib = string_t(libs(i)%s(3:))
                if (len_trim(current_lib%s) == 0) cycle
                this%has_link_libraries = .true.
                call add_strings(this%link_libs, current_lib)
            else ! -L and others: concatenate
                this%has_link_flags = .true.
                this%link_flags = string_t(trim(this%link_flags%s)//' '//libs(i)%s)

                ! Also save library dir
                if (str_begins_with_str(libs(i)%s, '-L')) then
                    libdir = libs(i)%s(3:)
                elseif (str_begins_with_str(libs(i)%s, '/LIBPATH')) then
                    libdir = libs(i)%s(9:)
                end if
            end if
        end do

        !> Get compiler flags
        flags = pkgcfg_get_build_flags(name, .true., error)
        if (allocated(error)) return

        do i = 1, size(flags)
            if (str_begins_with_str(flags(i)%s, include_flag)) then
                current_include_dir = string_t(flags(i)%s(len(include_flag)+1:))
                if (len_trim(current_include_dir%s) == 0) cycle
                this%has_include_dirs = .true.
                call add_strings(this%incl_dirs, current_include_dir)
            else
                this%has_build_flags = .true.
                this%flags = string_t(trim(this%flags%s)//' '//flags(i)%s)
            end if
        end do
    end subroutine add_pkg_config_compile_options

    !> Given a library name and folder, find extension and prefix
    subroutine lib_get_trailing(lib_name,lib_dir,prefix,suffix,found)
        character(*), intent(in) :: lib_name,lib_dir
        character(:), allocatable, intent(out) :: prefix,suffix
        logical, intent(out) :: found

        character(*), parameter :: extensions(*) = [character(11) :: '.dll.a','.a','.dylib','.dll']
        logical :: is_file
        character(:), allocatable :: noext,tokens(:),path
        integer :: l,k

        ! Extract name with no extension
        call split(lib_name,tokens,'.')
        noext = trim(tokens(1))

        ! Get library extension: find file name: NAME.a, NAME.dll.a, NAME.dylib, libNAME.a, etc.
        found = .false.
        suffix = ""
        prefix = ""
        with_pref: do l=1,2
            if (l==2) then
               prefix = "lib"
            else
               prefix = ""
            end if
            find_ext: do k=1,size(extensions)
                path = join_path(lib_dir,prefix//noext//trim(extensions(k)))
                inquire(file=path,exist=is_file)

                if (is_file) then
                   suffix = trim(extensions(k))
                   found = .true.
                   exit with_pref
                end if
            end do find_ext
        end do with_pref

        if (.not.found) then
             prefix = ""
             suffix = ""
        end if

    end subroutine lib_get_trailing
    
end module fpm_meta_util
