module fpm_meta_hdf5
    use fpm_compiler, only: compiler_t, get_include_flag
    use fpm_strings, only: str_begins_with_str, str_ends_with, string_t
    use fpm_filesystem, only: join_path
    use fpm_pkg_config, only: assert_pkg_config, pkgcfg_has_package, pkgcfg_list_all
    use fpm_meta_base, only: metapackage_t, destroy
    use fpm_meta_util, only: add_pkg_config_compile_options, lib_get_trailing, add_strings
    use fpm_manifest_metapackages, only: metapackage_request_t
    use fpm_error, only: error_t, fatal_error

    implicit none

    private

    public :: init_hdf5

    contains

    !> Initialize HDF5 metapackage for the current system
    subroutine init_hdf5(this,compiler,all_meta,error)
        class(metapackage_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        type(metapackage_request_t), intent(in) :: all_meta(:)
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
        
        !> Set name
        this%name = "hdf5"

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

        call add_pkg_config_compile_options(this, name, include_flag, libdir, error)
        if (allocated(error)) return

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
                      if (found) call add_strings(this%link_libs, &
                                                   string_t(this%link_libs(i)%s//trim(find_hl(k))))

                   end do add_missing

                end if

            end do
        endif

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
end module fpm_meta_hdf5
