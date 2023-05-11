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
use fpm_strings, only: lower, str_ends_with, string_t, operator(.in.)
use fpm_source_parsing, only: parse_f_source, parse_c_source
use fpm_manifest_executable, only: executable_config_t
implicit none

private
public :: add_sources_from_dir, add_executable_sources
public :: get_exe_name_with_suffix

character(4), parameter :: fortran_suffixes(2) = [".f90", &
                                                  ".f  "]
character(4), parameter :: c_suffixes(4) = [".c  ", ".h  ", ".cpp", ".hpp"]

contains

!> Wrapper to source parsing routines.
!> Selects parsing routine based on source file name extension
function parse_source(source_file_path,error) result(source)
    character(*), intent(in) :: source_file_path
    type(error_t), allocatable, intent(out) :: error
    type(srcfile_t)  :: source

    if (str_ends_with(lower(source_file_path), fortran_suffixes)) then

        source = parse_f_source(source_file_path, error)

        if (source%unit_type == FPM_UNIT_PROGRAM) then
            source%exe_name = basename(source_file_path,suffix=.false.)
        end if

    else if (str_ends_with(lower(source_file_path), c_suffixes)) then

        source = parse_c_source(source_file_path,error)

    end if

    if (allocated(error)) then
        return
    end if

end function parse_source

!> Add to `sources` by looking for source files in `directory`
subroutine add_sources_from_dir(sources,directory,scope,with_executables,recurse,error)
    !> List of `[[srcfile_t]]` objects to append to. Allocated if not allocated
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    !> Directory in which to search for source files
    character(*), intent(in) :: directory
    !> Scope to apply to the discovered sources, see [[fpm_model]] for enumeration
    integer, intent(in) :: scope
    !> Executable sources (fortran `program`s) are ignored unless `with_executables=.true.`
    logical, intent(in), optional :: with_executables
    !> Whether to recursively search subdirectories, default is `.true.`
    logical, intent(in), optional :: recurse
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: i
    logical, allocatable :: is_source(:), exclude_source(:)
    logical :: recurse_
    type(string_t), allocatable :: file_names(:)
    type(string_t), allocatable :: src_file_names(:)
    type(string_t), allocatable :: existing_src_files(:)
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

    is_source = [(.not.(is_hidden_file(basename(file_names(i)%s))) .and. &
                 .not.(canon_path(file_names(i)%s) .in. existing_src_files) .and. &
                 (str_ends_with(lower(file_names(i)%s), fortran_suffixes) .or. &
                 str_ends_with(lower(file_names(i)%s), c_suffixes) ),i=1,size(file_names))]
    src_file_names = pack(file_names,is_source)

    allocate(dir_sources(size(src_file_names)))
    allocate(exclude_source(size(src_file_names)))

    do i = 1, size(src_file_names)

        dir_sources(i) = parse_source(src_file_names(i)%s,error)
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
        sources = [sources, pack(dir_sources,.not.exclude_source)]
    end if

end subroutine add_sources_from_dir


!> Add to `sources` using the executable and test entries in the manifest and
!> applies any executable-specific overrides such as `executable%name`.
!> Adds all sources (including modules) from each `executable%source_dir`
subroutine add_executable_sources(sources,executables,scope,auto_discover,error)
    !> List of `[[srcfile_t]]` objects to append to. Allocated if not allocated
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    !> List of `[[executable_config_t]]` entries from manifest
    class(executable_config_t), intent(in) :: executables(:)
    !> Scope to apply to the discovered sources: either `FPM_SCOPE_APP` or `FPM_SCOPE_TEST`, see [[fpm_model]]
    integer, intent(in) :: scope
    !> If `.false.` only executables and tests specified in the manifest are added to `sources`
    logical, intent(in) :: auto_discover
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: i, j

    type(string_t), allocatable :: exe_dirs(:)
    type(srcfile_t) :: exe_source

    call get_executable_source_dirs(exe_dirs,executables)

    do i=1,size(exe_dirs)
        call add_sources_from_dir(sources,exe_dirs(i)%s, scope, &
                     with_executables=auto_discover, recurse=.false., error=error)

        if (allocated(error)) then
            return
        end if
    end do

    exe_loop: do i=1,size(executables)

        ! Check if executable already discovered automatically
        !  and apply any overrides
        do j=1,size(sources)

            if (basename(sources(j)%file_name,suffix=.true.) == executables(i)%main .and.&
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
            exe_source = parse_source(join_path(exe%source_dir,exe%main),error)
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
            sources = [sources, exe_source]
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
        exe_dirs = [exe_dirs,dirs_temp(1:n)]
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

end module fpm_sources
