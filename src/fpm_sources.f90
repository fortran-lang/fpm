!># Discovery of sources
!>
!> This module implements subroutines for building a list of
!> `[[srcfile_t]]` objects by looking for source files in the filesystem.
!>
module fpm_sources
use fpm_error, only: error_t
use fpm_model, only: srcfile_t, FPM_UNIT_PROGRAM
use fpm_filesystem, only: basename, canon_path, dirname, join_path, list_files, is_hidden_file
use fpm_environment, only: get_os_type,OS_WINDOWS
use fpm_strings, only: lower, str_ends_with, string_t, operator(.in.), add_strings
use fpm_source_parsing, only: parse_f_source, parse_c_source
use fpm_manifest_executable, only: executable_config_t
use fpm_manifest_preprocess, only: preprocess_config_t
implicit none

private
public :: add_sources_from_dir, add_executable_sources
public :: get_exe_name_with_suffix, add_srcfile, is_fypp_file

character(4), parameter :: fortran_suffixes(2) = [".f90", &
                                                  ".f  "]
character(4), parameter :: c_suffixes(4) = [".c  ", ".h  ", ".cpp", ".hpp"]

!> Add one or multiple source files to a source file array (gcc-15 bug workaround)
interface add_srcfile
    module procedure add_srcfile_one
    module procedure add_srcfile_many
end interface add_srcfile

contains

!> Wrapper to source parsing routines.
!> Selects parsing routine based on source file name extension
type(srcfile_t) function parse_source(source_file_path,custom_f_ext,error,preprocess,include_dirs) &
        result(source)
    character(*), intent(in) :: source_file_path
    type(string_t), optional, intent(in) :: custom_f_ext(:)
    type(error_t), allocatable, intent(out) :: error
    type(preprocess_config_t), optional, intent(in) :: preprocess
    !> Include directories, so that source digests cover the included files
    type(string_t), optional, intent(in) :: include_dirs(:)
    
    type(string_t), allocatable :: f_ext(:)

    call list_fortran_suffixes(f_ext,custom_f_ext)

    if (str_ends_with(lower(source_file_path), f_ext)) then

        source = parse_f_source(source_file_path, error, preprocess, include_dirs)

        if (source%unit_type == FPM_UNIT_PROGRAM) then
            source%exe_name = basename(source_file_path,suffix=.false.)
        end if

    else if (str_ends_with(lower(source_file_path), c_suffixes)) then

        source = parse_c_source(source_file_path, error, include_dirs)

    endif

    if (allocated(error)) then
        return
    end if

end function parse_source

!> List fortran suffixes, including optional ones
subroutine list_fortran_suffixes(suffixes,with_f_ext)
    type(string_t), allocatable, intent(out) :: suffixes(:)
    !> Additional user-defined (preprocessor) extensions that should be treated as Fortran sources
    type(string_t), intent(in), optional :: with_f_ext(:)

    integer :: ndefault,nuser,i

    ndefault = size(fortran_suffixes)
    nuser    = 0; if (present(with_f_ext)) nuser = size(with_f_ext)

    allocate(suffixes(ndefault + nuser))
    do i=1,ndefault
        suffixes(i) = string_t(fortran_suffixes(i))
    end do
    if (present(with_f_ext)) then
        do i=1,nuser
            suffixes(ndefault+i) = string_t(with_f_ext(i)%s)
        end do
    endif

end subroutine list_fortran_suffixes

!> Add to `sources` by looking for source files in `directory`
subroutine add_sources_from_dir(sources,directory,scope,with_executables,with_f_ext,recurse,error,&
                                preprocess,include_dirs)
    !> List of `[[srcfile_t]]` objects to append to. Allocated if not allocated
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    !> Directory in which to search for source files
    character(*), intent(in) :: directory
    !> Scope to apply to the discovered sources, see [[fpm_model]] for enumeration
    integer, intent(in) :: scope
    !> Executable sources (fortran `program`s) are ignored unless `with_executables=.true.`
    logical, intent(in), optional :: with_executables
    !> Additional user-defined (preprocessor) extensions that should be treated as Fortran sources
    type(string_t), intent(in), optional :: with_f_ext(:)
    !> Whether to recursively search subdirectories, default is `.true.`
    logical, intent(in), optional :: recurse
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    !> Optional source preprocessor configuration
    type(preprocess_config_t), optional, intent(in) :: preprocess    
    !> Include directories, so that source digests cover the included files
    type(string_t), optional, intent(in) :: include_dirs(:)

    integer :: i
    logical, allocatable :: is_source(:), exclude_source(:)
    logical :: recurse_
    type(string_t), allocatable :: file_names(:)
    type(string_t), allocatable :: src_file_names(:),f_ext(:)
    type(string_t), allocatable :: existing_src_files(:)
    type(string_t), allocatable :: stems(:)
    type(srcfile_t), allocatable :: dir_sources(:)

    recurse_ = .true.
    if (present(recurse)) recurse_ = recurse
    ! Scan directory for sources
    call list_files(directory, file_names,recurse=recurse_)

    if (allocated(sources)) then
        allocate(existing_src_files(size(sources)))
        do i=1,size(sources)
            existing_src_files(i)%s = canon_path(sources(i)%file_name)
        end do
    else
        allocate(existing_src_files(0))
    end if

    ! Get legal fortran suffixes
    call list_fortran_suffixes(f_ext,with_f_ext)

    is_source = [(.not.(is_hidden_file(basename(file_names(i)%s))) .and. &
                 .not.(canon_path(file_names(i)%s) .in. existing_src_files) .and. &
                 (str_ends_with(lower(file_names(i)%s), f_ext) .or. &
                 str_ends_with(lower(file_names(i)%s), c_suffixes) ),i=1,size(file_names))]

    ! With fypp enabled, a Fortran file next to a .fypp file of the same name is assumed to be
    ! a previously generated copy of it: skip it, the .fypp file is the source
    ! (avoids duplicate module definitions)
    if (present(preprocess)) then
        if (preprocess%has_fypp()) then
            stems = fypp_stems(file_names)
            do i = 1, size(file_names)
                if (is_source(i)) is_source(i) = .not.has_fypp_twin(file_names(i)%s, stems)
            end do
        end if
    end if


    src_file_names = pack(file_names,is_source)

    allocate(dir_sources(size(src_file_names)))
    allocate(exclude_source(size(src_file_names)))

    do i = 1, size(src_file_names)

        dir_sources(i) = parse_source(src_file_names(i)%s,with_f_ext,error,preprocess,include_dirs)
        if (allocated(error)) return

        dir_sources(i)%unit_scope = scope
        allocate(dir_sources(i)%link_libraries(0))

        ! Exclude executables unless specified otherwise
        exclude_source(i) = (dir_sources(i)%unit_type == FPM_UNIT_PROGRAM)
        if (dir_sources(i)%unit_type == FPM_UNIT_PROGRAM .and. &
            & present(with_executables)) then
            if (with_executables) then

                exclude_source(i) = .false.

            end if
        end if

    end do

    if (.not.allocated(sources)) then
        sources = pack(dir_sources,.not.exclude_source)
    else
        call add_srcfile(sources, pack(dir_sources,.not.exclude_source))
    end if

end subroutine add_sources_from_dir


!> Add to `sources` using the executable and test entries in the manifest and
!> applies any executable-specific overrides such as `executable%name`.
!> Adds all sources (including modules) from each `executable%source_dir`
subroutine add_executable_sources(sources,executables,scope,auto_discover,with_f_ext,error,preprocess,&
                                  include_dirs)
    !> List of `[[srcfile_t]]` objects to append to. Allocated if not allocated
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    !> List of `[[executable_config_t]]` entries from manifest
    class(executable_config_t), intent(in) :: executables(:)
    !> Scope to apply to the discovered sources: either `FPM_SCOPE_APP` or `FPM_SCOPE_TEST`, see [[fpm_model]]
    integer, intent(in) :: scope
    !> If `.false.` only executables and tests specified in the manifest are added to `sources`
    logical, intent(in) :: auto_discover
    !> Additional user-defined (preprocessor) extensions that should be treated as Fortran sources
    type(string_t), intent(in), optional :: with_f_ext(:)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    !> Optional source preprocessor configuration
    type(preprocess_config_t), optional, intent(in) :: preprocess
    !> Include directories, so that source digests cover the included files
    type(string_t), optional, intent(in) :: include_dirs(:)

    integer :: i, j

    type(string_t), allocatable :: exe_dirs(:)
    type(srcfile_t) :: exe_source

    call get_executable_source_dirs(exe_dirs,executables)

    do i=1,size(exe_dirs)
        call add_sources_from_dir(sources,exe_dirs(i)%s, scope, &
                     with_executables=auto_discover, with_f_ext=with_f_ext,recurse=.false., &
                     error=error, preprocess=preprocess, include_dirs=include_dirs)

        if (allocated(error)) then
            return
        end if
    end do

    exe_loop: do i=1,size(executables)

        ! Check if executable already discovered automatically
        !  and apply any overrides
        do j=1,size(sources)

            !> Compare lowercase strings to allow auto-discovery of pre-processed extensions
            if (lower(basename(sources(j)%file_name,suffix=.true.)) == lower(executables(i)%main) .and.&
                 canon_path(dirname(sources(j)%file_name)) == &
                 canon_path(executables(i)%source_dir) ) then

                sources(j)%exe_name = executables(i)%name
                if (allocated(executables(i)%link)) then
                    sources(j)%link_libraries = executables(i)%link
                end if
                sources(j)%unit_type = FPM_UNIT_PROGRAM
                cycle exe_loop

            end if

        end do

        ! Add if not already discovered (auto_discovery off)
        associate(exe => executables(i))
            exe_source = parse_source(join_path(exe%source_dir,exe%main),with_f_ext,error,preprocess, &
                                      include_dirs)
            exe_source%exe_name = exe%name
            if (allocated(exe%link)) then
                exe_source%link_libraries = exe%link
            end if
            exe_source%unit_type = FPM_UNIT_PROGRAM
            exe_source%unit_scope = scope
        end associate

        if (allocated(error)) return

        if (.not.allocated(sources)) then
            sources = [exe_source]
        else
            call add_srcfile(sources, exe_source)
        end if

    end do exe_loop

end subroutine add_executable_sources

!> Build a list of unique source directories
!>  from executables specified in manifest
subroutine get_executable_source_dirs(exe_dirs,executables)
    type(string_t), allocatable, intent(inout) :: exe_dirs(:)
    class(executable_config_t), intent(in) :: executables(:)

    type(string_t) :: dirs_temp(size(executables))

    integer :: i, n

    n = 0

    do i=1,size(executables)
       dirs_temp(i)%s=' '
    enddo

    do i=1,size(executables)
        if (.not.(executables(i)%source_dir .in. dirs_temp)) then

            n = n + 1
            dirs_temp(n)%s = executables(i)%source_dir

        end if
    end do

    if (.not.allocated(exe_dirs)) then
        exe_dirs = dirs_temp(1:n)
    else
        call add_strings(exe_dirs,dirs_temp(1:n))
    end if

end subroutine get_executable_source_dirs

!> Build an executable name with suffix. Safe routine that always returns an allocated string
function get_exe_name_with_suffix(source) result(suffixed)
    type(srcfile_t), intent(in) :: source
    character(len=:), allocatable :: suffixed

    if (allocated(source%exe_name)) then
       if (get_os_type() == OS_WINDOWS) then
           suffixed = source%exe_name//'.exe'
       else
           suffixed = source%exe_name
       end if
    else
       suffixed = ""
    endif

end function get_exe_name_with_suffix

!> Whether a file has the `.fypp` extension (any case)
logical function is_fypp_file(file_path)
    character(*), intent(in) :: file_path
    is_fypp_file = str_ends_with(lower(file_path), ".fypp")
end function is_fypp_file

!> Lower-case canonical paths, without extension, of the `.fypp` files in a file list
function fypp_stems(files) result(stems)
    type(string_t), intent(in) :: files(:)
    type(string_t), allocatable :: stems(:)

    character(:), allocatable :: path
    integer :: i

    allocate(stems(0))
    do i = 1, size(files)
        if (.not.is_fypp_file(files(i)%s)) cycle
        path = lower(canon_path(files(i)%s))
        call add_strings(stems, string_t(path(1:len(path) - len(".fypp"))))
    end do

end function fypp_stems

!> Whether `file` is a `.f90`/`.F90` file with a `.fypp` file of the same name in the same
!> directory (`stems`: result of `fypp_stems`)
logical function has_fypp_twin(file, stems)
    character(*), intent(in) :: file
    type(string_t), intent(in) :: stems(:)

    character(:), allocatable :: stem

    has_fypp_twin = .false.
    if (size(stems) == 0) return
    if (.not.str_ends_with(lower(file), ".f90")) return

    stem = lower(canon_path(file))
    has_fypp_twin = stem(1:len(stem) - len(".f90")) .in. stems

end function has_fypp_twin

!> Add one source file to a source file array with a loop (gcc-15 bug on array initializer)
pure subroutine add_srcfile_one(list,new)
    type(srcfile_t), allocatable, intent(inout) :: list(:)
    type(srcfile_t), intent(in) :: new

    integer :: i,n
    type(srcfile_t), allocatable :: tmp(:)

    if (allocated(list)) then
       n = size(list)
    else
       n = 0
    end if

    allocate(tmp(n+1))
    do i=1,n
       tmp(i) = list(i)
    end do
    tmp(n+1) = new
    call move_alloc(from=tmp,to=list)

end subroutine add_srcfile_one

!> Add multiple source files to a source file array with a loop (gcc-15 bug on array initializer)
pure subroutine add_srcfile_many(list,new)
    type(srcfile_t), allocatable, intent(inout) :: list(:)
    type(srcfile_t), intent(in) :: new(:)

    integer :: i,n,add
    type(srcfile_t), allocatable :: tmp(:)

    if (allocated(list)) then
       n = size(list)
    else
       n = 0
    end if

    add = size(new)
    if (add == 0) return

    allocate(tmp(n+add))
    do i=1,n
       tmp(i) = list(i)
    end do
    do i=1,add
       tmp(n+i) = new(i)
    end do
    call move_alloc(from=tmp,to=list)

end subroutine add_srcfile_many

end module fpm_sources
