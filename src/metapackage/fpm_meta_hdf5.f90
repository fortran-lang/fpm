module fpm_meta_hdf5
    use fpm_compiler, only: compiler_t, get_include_flag
    use fpm_strings, only: str_begins_with_str, str_ends_with
    use fpm_filesystem, only: join_path
    use fpm_pkg_config, only: assert_pkg_config, pkgcfg_has_package, &
        pkgcfg_get_libs, pkgcfg_get_build_flags, pkgcfg_get_version, pkgcfg_list_all
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_strings, only: string_t, split
    use fpm_error, only: error_t, fatal_error
    use fpm_versioning, only: new_version

    implicit none

    private

    public :: init_hdf5

    contains

    !> Initialize HDF5 metapackage for the current system
    subroutine init_hdf5(this,compiler,error)
        class(metapackage_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        type(error_t), allocatable, intent(out) :: error

        character(*), parameter :: find_hl(*) = &
                     [character(11) :: '_hl_fortran','hl_fortran','_fortran','_hl']
        character(*), parameter :: candidates(*) = &
                     [character(15) :: 'hdf5_hl_fortran','hdf5-hl-fortran','hdf5_fortran','hdf5-fortran',&
                                       'hdf5_hl','hdf5','hdf5-serial']

        integer :: i,j,k,l
        logical :: s,found_hl(size(find_hl)),found
        type(string_t) :: log,this_lib
        type(string_t), allocatable :: libs(:),flags(:),modules(:),non_fortran(:)
        character(len=:), allocatable :: name, include_flag, libdir, ext, pref

        include_flag = get_include_flag(compiler,"")

        !> Cleanup
        call destroy(this)
        allocate(this%link_libs(0),this%incl_dirs(0),this%external_modules(0),non_fortran(0))
        this%link_flags = string_t("")
        this%flags = string_t("")

        !> Assert pkg-config is installed
        if (.not.assert_pkg_config()) then
            call fatal_error(error,'hdf5 metapackage requires pkg-config')
            return
        end if

        !> Find pkg-config package file by priority
        name = 'NOT_FOUND'
        find_package: do i=1,size(candidates)
            if (pkgcfg_has_package(trim(candidates(i)))) then
                name = trim(candidates(i))
                exit find_package
            end if
        end do find_package

        !> some distros put hdf5-1.2.3.pc with version number in .pc filename.
        if (name=='NOT_FOUND') then
            modules = pkgcfg_list_all(error)
            find_global_package: do i=1,size(modules)
                if (str_begins_with_str(modules(i)%s,'hdf5')) then
                    name = modules(i)%s
                    exit find_global_package
                end if
            end do find_global_package
        end if

        if (name=='NOT_FOUND') then
            call fatal_error(error,'pkg-config could not find a suitable hdf5 package.')
            return
        end if

        !> Get version
        log = pkgcfg_get_version(name,error)
        if (allocated(error)) return
        allocate(this%version)
        call new_version(this%version,log%s,error)
        if (allocated(error)) return

        !> Get libraries
        libs = pkgcfg_get_libs(name,error)
        if (allocated(error)) return

        libdir = ""
        do i=1,size(libs)

            if (str_begins_with_str(libs(i)%s,'-l')) then
                this%has_link_libraries = .true.
                this%link_libs = [this%link_libs, string_t(libs(i)%s(3:))]

            else ! -L and others: concatenate
                this%has_link_flags = .true.
                this%link_flags = string_t(trim(this%link_flags%s)//' '//libs(i)%s)

                ! Also save library dir
                if (str_begins_with_str(libs(i)%s,'-L')) then
                   libdir = libs(i)%s(3:)
                elseif (str_begins_with_str(libs(i)%s,'/LIBPATH')) then
                   libdir = libs(i)%s(9:)
                endif

            end if
        end do

        ! Some pkg-config hdf5.pc (e.g. Ubuntu) don't include the commonly-used HL HDF5 libraries,
        ! so let's add them if they exist
        if (len_trim(libdir)>0) then
            do i=1,size(this%link_libs)

                found_hl = .false.

                if (.not.str_ends_with(this%link_libs(i)%s, find_hl)) then

                   ! Extract name with no extension
                   call lib_get_trailing(this%link_libs(i)%s, libdir, pref, ext, found)

                   ! Search how many versions with the Fortran endings there are
                   finals: do k=1,size(find_hl)
                      do j=1,size(this%link_libs)
                       if (str_begins_with_str(this%link_libs(j)%s,this%link_libs(i)%s) .and. &
                           str_ends_with(this%link_libs(j)%s,trim(find_hl(k)))) then
                           found_hl(k) = .true.
                           cycle finals
                       end if
                      end do
                   end do finals

                   ! For each of the missing ones, if there is a file, add it
                   add_missing: do k=1,size(find_hl)
                      if (found_hl(k)) cycle add_missing

                      ! Build file name
                      this_lib%s = join_path(libdir,pref//this%link_libs(i)%s//trim(find_hl(k))//ext)
                      inquire(file=this_lib%s,exist=found)

                      ! File exists, but it is not linked against
                      if (found) this%link_libs = [this%link_libs, &
                                                   string_t(this%link_libs(i)%s//trim(find_hl(k)))]

                   end do add_missing

                end if

            end do
        endif

        !> Get compiler flags
        flags = pkgcfg_get_build_flags(name,.true.,error)
        if (allocated(error)) return

        do i=1,size(flags)

            if (str_begins_with_str(flags(i)%s,include_flag)) then
                this%has_include_dirs = .true.
                this%incl_dirs = [this%incl_dirs, string_t(flags(i)%s(len(include_flag)+1:))]
            else
                this%has_build_flags = .true.
                this%flags = string_t(trim(this%flags%s)//' '//flags(i)%s)
            end if

        end do

        !> Add HDF5 modules as external
        this%has_external_modules = .true.
        this%external_modules = [string_t('h5a'), &
                                 string_t('h5d'), &
                                 string_t('h5es'), &
                                 string_t('h5e'), &
                                 string_t('h5f'), &
                                 string_t('h5g'), &
                                 string_t('h5i'), &
                                 string_t('h5l'), &
                                 string_t('h5o'), &
                                 string_t('h5p'), &
                                 string_t('h5r'), &
                                 string_t('h5s'), &
                                 string_t('h5t'), &
                                 string_t('h5vl'), &
                                 string_t('h5z'), &
                                 string_t('h5lt'), &
                                 string_t('h5lib'), &
                                 string_t('h5global'), &
                                 string_t('h5_gen'), &
                                 string_t('h5fortkit'), &
                                 string_t('hdf5')]

    end subroutine init_hdf5

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

end module fpm_meta_hdf5
