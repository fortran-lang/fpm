!> CMake staleness detection utilities
module fpm_cmake_check
    use fpm_filesystem, only: exists, read_lines
    use fpm_strings, only: string_t, fnv_1a
    use fpm_model, only: srcfile_t
    use fpm_hash_table, only: string_hash_map_t
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private
    public :: compute_manifest_hash, compute_project_hash, check_cmake_staleness

    !> Marker string used in CMakeLists.txt header to store the project hash
    character(len=*), parameter :: project_hash_marker = "Project hash: "

contains

    !> Sort string_t array in-place using insertion sort (for deterministic ordering)
    subroutine sort_strings(arr)
        type(string_t), intent(inout) :: arr(:)
        integer :: i, j
        type(string_t) :: temp

        do i = 2, size(arr)
            temp = arr(i)
            j = i - 1
            do while (j >= 1)
                if (.not. llt(temp%s, arr(j)%s)) exit
                arr(j + 1) = arr(j)
                j = j - 1
            end do
            arr(j + 1) = temp
        end do
    end subroutine sort_strings

    !> Compute hash of manifest + all source files for comprehensive staleness detection
    !> This combines the manifest file hash with hashes of all source files (paths and content)
    function compute_project_hash(manifest_file, sources) result(hash_str)
        character(len=*), intent(in) :: manifest_file
        type(srcfile_t), intent(in) :: sources(:)
        character(len=:), allocatable :: hash_str

        type(string_t), allocatable :: lines(:), file_paths(:)
        type(string_hash_map_t) :: source_index
        integer(int64) :: hash_val
        character(len=20) :: temp_str, digest_str
        integer :: i, src_idx

        ! Start with manifest file content hash
        if (exists(manifest_file)) then
            lines = read_lines(manifest_file)
            if (size(lines) > 0) then
                hash_val = fnv_1a(lines(1)%s)
                do i = 2, size(lines)
                    hash_val = fnv_1a(lines(i)%s, hash_val)
                end do
            else
                hash_val = 0_int64
            end if
        else
            hash_val = 0_int64
        end if

        ! Collect and sort source file paths for deterministic ordering
        if (size(sources) > 0) then
            ! Build hash map from file path to source index for O(1) lookup
            call source_index%init(max(size(sources) * 2, 16))
            allocate(file_paths(size(sources)))
            do i = 1, size(sources)
                file_paths(i)%s = sources(i)%file_name
                call source_index%set(sources(i)%file_name, i)
            end do
            call sort_strings(file_paths)

            ! Chain-hash each file: path + content digest
            do i = 1, size(file_paths)
                ! Hash the file path
                hash_val = fnv_1a(file_paths(i)%s, hash_val)

                ! Look up source index and hash its digest
                if (source_index%get(file_paths(i)%s, src_idx)) then
                    write(digest_str, '(z16.16)') sources(src_idx)%digest
                    hash_val = fnv_1a(trim(digest_str), hash_val)
                end if
            end do

            call source_index%destroy()
        end if

        ! Convert hash to hex string
        write(temp_str, '(z16.16)') hash_val
        hash_str = trim(adjustl(temp_str))
    end function compute_project_hash

    !> Compute hash of fpm.toml manifest file for staleness detection
    function compute_manifest_hash(manifest_file) result(hash_str)
        character(len=*), intent(in) :: manifest_file
        character(len=:), allocatable :: hash_str

        type(string_t), allocatable :: lines(:)
        integer(int64) :: hash_val
        character(len=20) :: temp_str
        integer :: i

        ! Read manifest file content and compute hash
        if (exists(manifest_file)) then
            lines = read_lines(manifest_file)
            if (size(lines) > 0) then
                ! Hash all lines together
                hash_val = fnv_1a(lines(1)%s)
                do i = 2, size(lines)
                    hash_val = fnv_1a(lines(i)%s, hash_val)
                end do
            else
                hash_val = 0_int64
            end if
            ! Convert hash to hex string
            write(temp_str, '(z16.16)') hash_val
            hash_str = trim(adjustl(temp_str))
        else
            hash_str = "0000000000000000"
        end if
    end function compute_manifest_hash

    !> Check if CMake files are stale and need regeneration
    !> Returns .true. if CMakeLists.txt exists with fpm marker and is out of date
    !> Reads only the first few lines to avoid unnecessary I/O for non-fpm CMake files
    function check_cmake_staleness(sources) result(is_stale)
        type(srcfile_t), intent(in) :: sources(:)
        logical :: is_stale

        character(len=:), allocatable :: current_hash, stored_hash
        character(len=512) :: line_buf
        integer :: lun, ios, i
        logical :: has_fpm_marker, has_hash

        is_stale = .false.

        ! Check if CMakeLists.txt exists
        if (.not. exists("CMakeLists.txt")) return

        ! Read only the first 5 lines to check for fpm marker and hash
        open(newunit=lun, file="CMakeLists.txt", status='old', action='read', iostat=ios)
        if (ios /= 0) return

        has_fpm_marker = .false.
        has_hash = .false.
        stored_hash = ""

        do i = 1, 5
            read(lun, '(a)', iostat=ios) line_buf
            if (ios /= 0) exit
            if (index(line_buf, "generated by fpm") > 0) then
                has_fpm_marker = .true.
            end if
            if (index(line_buf, project_hash_marker) > 0) then
                has_hash = .true.
                stored_hash = trim(adjustl(line_buf(index(line_buf, project_hash_marker) + len(project_hash_marker):)))
            end if
        end do

        close(lun)

        ! If not generated by fpm, don't warn
        if (.not. has_fpm_marker) return

        ! If no hash found, it's an old format - consider stale
        if (.not. has_hash) then
            is_stale = .true.
            return
        end if

        ! Compare with current project hash (manifest + all sources)
        if (exists("fpm.toml")) then
            current_hash = compute_project_hash("fpm.toml", sources)
            if (trim(stored_hash) /= trim(current_hash)) then
                is_stale = .true.
            end if
        end if

    end function check_cmake_staleness

end module fpm_cmake_check
