module fpm_sources
use fpm_error, only: error_t, fatal_error
use fpm_model, only: srcfile_t, FPM_UNIT_PROGRAM, &
                    FPM_SCOPE_LIB, FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST
                    
use fpm_filesystem, only: basename, canon_path, dirname, join_path, list_files
use fpm_strings, only: lower, str_ends_with, string_t, operator(.in.)
use fpm_manifest_executable, only: executable_t
use fpm_source_parsing, only: parse_source
implicit none

private
public :: add_sources_from_dir, add_executable_sources

contains

subroutine add_sources_from_dir(sources,directory,scope,with_executables,error)
    ! Enumerate sources in a directory
    !
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    character(*), intent(in) :: directory
    integer, intent(in) :: scope
    logical, intent(in), optional :: with_executables
    type(error_t), allocatable, intent(out) :: error

    integer :: i
    logical, allocatable :: is_source(:), exclude_source(:)
    type(string_t), allocatable :: file_names(:)
    type(string_t), allocatable :: src_file_names(:)
    type(string_t), allocatable :: existing_src_files(:)
    type(srcfile_t), allocatable :: dir_sources(:)

    ! Scan directory for sources
    call list_files(directory, file_names,recurse=.true.)

    if (allocated(sources)) then
        allocate(existing_src_files(size(sources)))
        do i=1,size(sources)
            existing_src_files(i)%s = canon_path(sources(i)%file_name)
        end do
    else
        allocate(existing_src_files(0))
    end if

    is_source = [(.not.(canon_path(file_names(i)%s) .in. existing_src_files) .and. &
                  (str_ends_with(lower(file_names(i)%s), ".f90") .or. &
                   str_ends_with(lower(file_names(i)%s), ".c") .or. &
                   str_ends_with(lower(file_names(i)%s), ".h") ),i=1,size(file_names))]
    src_file_names = pack(file_names,is_source)

    allocate(dir_sources(size(src_file_names)))
    allocate(exclude_source(size(src_file_names)))

    do i = 1, size(src_file_names)

        dir_sources(i) = parse_source(src_file_names(i)%s,error)
        if (allocated(error)) return

        dir_sources(i)%unit_scope = scope

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


subroutine add_executable_sources(sources,executables,scope,auto_discover,error)
    ! Include sources from any directories specified 
    !  in [[executable]] entries and apply any customisations
    !  
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    class(executable_t), intent(in) :: executables(:)
    integer, intent(in) :: scope
    logical, intent(in) :: auto_discover
    type(error_t), allocatable, intent(out) :: error

    integer :: i, j

    type(string_t), allocatable :: exe_dirs(:)
    type(srcfile_t) :: exe_source

    call get_executable_source_dirs(exe_dirs,executables)

    do i=1,size(exe_dirs)
        call add_sources_from_dir(sources,exe_dirs(i)%s, &
                     scope, with_executables=auto_discover,error=error)

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
                cycle exe_loop

            end if

        end do

        ! Add if not already discovered (auto_discovery off)
        exe_source = parse_source(join_path(executables(i)%source_dir,executables(i)%main),error)
        exe_source%exe_name = executables(i)%name
        exe_source%unit_scope = scope
        
        if (allocated(error)) return

        if (.not.allocated(sources)) then
            sources = [exe_source]
        else
            sources = [sources, exe_source]
        end if

    end do exe_loop

end subroutine add_executable_sources


subroutine get_executable_source_dirs(exe_dirs,executables)
    ! Build a list of unique source directories
    !  from executables specified in manifest
    type(string_t), allocatable, intent(inout) :: exe_dirs(:)
    class(executable_t), intent(in) :: executables(:)

    type(string_t) :: dirs_temp(size(executables))

    integer :: i, n

    n = 0
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


end module fpm_sources
