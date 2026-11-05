!> Cache of source file analysis results
!>
!> Building the build target graph requires the result of a rudimentary parse
!> of every source file (unit type, modules provided, modules used, includes,
!> and the source digest). Re-reading and re-parsing every unchanged source on
!> every invocation is a significant part of a no-op build.
!>
!> This module persists those parse results in a single file in the build
!> directory. An entry is only reused when the source file is unchanged, which
!> is detected from its size and modification time. `SCHEMA_VERSION` only
!> covers the file layout and the stored fields; a change of the parse logic
!> invalidates the cache automatically, because the file records the size and
!> modification stamp of the fpm executable that produced the entries.
!>
!> The cache is an optimization only: any failure (unreadable cache, malformed
!> entries, or an unusable file timestamp) silently disables it.
module fpm_source_cache
    use,intrinsic :: iso_fortran_env, only: int64
    use fpm_error, only: error_t
    use fpm_filesystem, only: exists, file_info, canon_path, which
    use fpm_model, only: srcfile_t
    use fpm_strings, only: string_t
    use fpm_toml, only: load_table_from_file, toml_table, toml_key, get_value, &
                        set_value, set_string, get_list, set_list, toml_serialize
    implicit none
    private

    public :: source_cache_t

    !> Version of the cache file layout and of the stored fields.
    !> Increment whenever the layout or the stored fields change; a change of
    !> the parse logic is detected through the program fingerprint instead.
    !> Version 1 records the fingerprint of the fpm executable.
    integer, parameter :: SCHEMA_VERSION = 1

    !> A single cached source analysis
    type, private :: cache_entry_t
        !> Canonical source file path (cache key)
        character(:), allocatable :: key
        !> File size in bytes when the entry was recorded
        integer(int64) :: size = -1
        !> File modification time when the entry was recorded
        integer(int64) :: mtime = -1
        !> Fingerprint of the preprocessing configuration, empty when unused
        character(:), allocatable :: config
        !> Files inlined by preprocessing, encoded as "path|size|mtime"
        type(string_t), allocatable :: inputs(:)
        !> Cached analysis result
        type(srcfile_t) :: source
    end type cache_entry_t

    !> Persistent cache of source analyses, backed by a file in the build dir
    type :: source_cache_t
        !> Path of the cache file
        character(:), allocatable :: file
        !> Whether the cache is active
        logical :: enabled = .false.
        !> Whether the cache file has been read
        logical :: loaded = .false.
        !> Whether entries were added or updated since the cache was read
        logical :: dirty = .false.
        !> Cached source analyses
        type(cache_entry_t), allocatable :: entries(:)
        !> Number of valid entries in `entries`
        integer :: n_entries = 0
    contains
        !> Activate the cache and set the file used to persist it
        procedure :: init
        !> Return the analysis of a source file if it is unchanged
        procedure :: lookup
        !> Record the analysis of a source file
        procedure :: store
        !> Persist the cache to disk if it was modified
        procedure :: save
        !> Read the cache file once
        procedure, private :: ensure_loaded
        !> Grow the entry storage
        procedure, private :: ensure_capacity
    end type source_cache_t

contains

    !> Activate the cache and set the file used to persist it
    subroutine init(self, file)
        class(source_cache_t), intent(inout) :: self
        character(*), intent(in) :: file

        self%file = file
        self%enabled = .true.
        self%loaded = .false.
        self%dirty = .false.
        self%n_entries = 0
        if (allocated(self%entries)) deallocate (self%entries)

    end subroutine init

    !> Look up the analysis of `filename` and return it in `source`
    !>
    !> Returns `.true.` only when the file (and every file it depends on) is
    !> unchanged since the entry was recorded, and the preprocessing
    !> configuration is the same.
    logical function lookup(self, filename, source, config) result(hit)
        class(source_cache_t), intent(inout) :: self
        character(*), intent(in) :: filename
        type(srcfile_t), intent(out) :: source
        !> Fingerprint of the preprocessing configuration, if any
        character(*), optional, intent(in) :: config

        integer(int64) :: file_size, file_mtime, dep_size, dep_mtime
        integer :: i, j
        character(:), allocatable :: key, dep_path

        hit = .false.
        if (.not. self%enabled) return

        call self%ensure_loaded()

        if (.not. file_info(filename, file_size, file_mtime)) return
        key = canon_path(filename)

        do i = 1, self%n_entries
            if (self%entries(i)%key /= key) cycle
            if (self%entries(i)%size /= file_size) return
            if (self%entries(i)%mtime /= file_mtime) return
            if (.not. same_config(self%entries(i)%config, config)) return
            if (allocated(self%entries(i)%inputs)) then
                do j = 1, size(self%entries(i)%inputs)
                    call decode_input(self%entries(i)%inputs(j)%s, dep_path, dep_size, dep_mtime)
                    if (len_trim(dep_path) == 0) return
                    if (.not. file_info(dep_path, file_size, file_mtime)) return
                    if (file_size /= dep_size .or. file_mtime /= dep_mtime) return
                end do
            end if
            source = self%entries(i)%source
            hit = .true.
            return
        end do

    end function lookup

    !> Record the analysis of `filename` in the cache
    subroutine store(self, filename, source, config, inputs)
        class(source_cache_t), intent(inout) :: self
        character(*), intent(in) :: filename
        type(srcfile_t), intent(in) :: source
        !> Fingerprint of the preprocessing configuration, if any
        character(*), optional, intent(in) :: config
        !> Files inlined by preprocessing
        type(string_t), optional, intent(in) :: inputs(:)

        integer(int64) :: file_size, file_mtime
        integer :: i, j
        character(:), allocatable :: key, encoded
        type(string_t), allocatable :: encoded_inputs(:)

        if (.not. self%enabled) return
        if (.not. file_info(filename, file_size, file_mtime)) return

        ! Encode the inputs together with their current timestamps; if any of
        ! them cannot be inspected the entry cannot be validated, so skip it.
        if (present(inputs)) then
            if (size(inputs) > 0) then
                allocate (encoded_inputs(size(inputs)))
                do j = 1, size(inputs)
                    call encode_input(inputs(j)%s, encoded)
                    if (len_trim(encoded) == 0) return
                    encoded_inputs(j)%s = encoded
                end do
            end if
        end if

        call self%ensure_loaded()

        key = canon_path(filename)

        do i = 1, self%n_entries
            if (self%entries(i)%key == key) then
                call update_entry(self%entries(i))
                self%dirty = .true.
                return
            end if
        end do

        call self%ensure_capacity(self%n_entries + 1)
        self%n_entries = self%n_entries + 1
        self%entries(self%n_entries)%key = key
        call update_entry(self%entries(self%n_entries))
        self%dirty = .true.

        return

    contains

        subroutine update_entry(entry)
            type(cache_entry_t), intent(inout) :: entry

            entry%size = file_size
            entry%mtime = file_mtime
            entry%source = source
            if (present(config)) then
                entry%config = config
            else
                if (allocated(entry%config)) deallocate (entry%config)
            end if
            if (allocated(entry%inputs)) deallocate (entry%inputs)
            if (allocated(encoded_inputs)) call move_alloc(encoded_inputs, entry%inputs)
        end subroutine update_entry

    end subroutine store

    !> Persist the cache to disk if it was modified
    subroutine save(self)
        class(source_cache_t), intent(inout) :: self

        type(toml_table), allocatable :: entry_table
        type(error_t), allocatable :: error
        character(:), allocatable :: fingerprint
        integer :: i, stat, unit

        if (.not. self%enabled) return
        if (.not. self%dirty) return
        if (.not. allocated(self%file)) return

        open (newunit=unit, file=self%file, status='replace', action='write', &
              iostat=stat)
        if (stat /= 0) return

        write (unit, '(a,i0)') 'version = ', SCHEMA_VERSION

        fingerprint = program_fingerprint()
        if (len(fingerprint) > 0) then
            write (unit, '(a)') 'fingerprint = "'//fingerprint//'"'
        end if

        do i = 1, self%n_entries
            ! Serialize one entry at a time: the toml-f serializer appends to a
            ! single growing string, and flang places the concatenation
            ! temporaries on the stack, which overflows for a document the size
            ! of the complete cache. A single entry stays small enough.
            allocate (entry_table)
            call build_entry_table(self%entries(i), entry_table, error)
            if (allocated(error)) then
                ! Skip this entry but keep writing the others: a cache that is
                ! missing one entry is still usable.
                deallocate (error)
            else
                write (unit, '(a,i0,a)', iostat=stat) '[entries.', i, ']'
                if (stat == 0) write (unit, '(a)', iostat=stat) toml_serialize(entry_table)
            end if
            call entry_table%destroy()
            deallocate (entry_table)
            if (stat /= 0) exit
        end do

        close (unit, iostat=stat)
        ! Keep the cache dirty when it could not be written completely, so that
        ! a later invocation retries instead of trusting a truncated file.
        if (stat == 0) self%dirty = .false.

    end subroutine save

    !> Fill a table with the stored fields of a single cache entry
    subroutine build_entry_table(entry, table, error)
        type(cache_entry_t), intent(inout) :: entry
        type(toml_table), intent(inout) :: table
        type(error_t), allocatable, intent(out) :: error

        call set_value(table, "size", entry%size, error, 'source-cache')
        if (allocated(error)) return
        call set_value(table, "mtime", entry%mtime, error, 'source-cache')
        if (allocated(error)) return
        if (allocated(entry%config)) then
            call set_string(table, "config", entry%config, error, 'source-cache')
            if (allocated(error)) return
        end if
        if (allocated(entry%inputs)) then
            call set_list(table, "inputs", entry%inputs, error)
            if (allocated(error)) return
        end if

        ! Store everything the parser reported (including the digest)
        call entry%source%dump_to_toml(table, error)

    end subroutine build_entry_table

    !> Load the cache file once
    subroutine ensure_loaded(self)
        class(source_cache_t), intent(inout) :: self

        type(toml_table), allocatable :: table
        type(toml_table), pointer :: entries_table, entry_table
        type(toml_key), allocatable :: keys(:)
        type(error_t), allocatable :: error
        type(string_t), allocatable :: input_list(:)
        character(:), allocatable :: fingerprint, stored_fingerprint
        integer :: i, j, version

        if (self%loaded) return
        self%loaded = .true.

        if (.not. exists(self%file)) return

        call load_table_from_file(table, self%file, error)
        if (allocated(error)) return

        call get_value(table, "version", version, error, 'source-cache')
        if (allocated(error)) return
        if (version /= SCHEMA_VERSION) return

        ! The entries were produced by the parser of a specific fpm executable:
        ! only reuse them when the running executable is the same one. When the
        ! executable cannot be identified the check is skipped, so that an
        ! unusual environment does not disable the cache permanently.
        fingerprint = program_fingerprint()
        if (len(fingerprint) > 0) then
            if (.not. table%has_key("fingerprint")) return
            call get_value(table, "fingerprint", stored_fingerprint, error, 'source-cache')
            if (allocated(error)) return
            if (stored_fingerprint /= fingerprint) return
        end if

        if (.not. table%has_key("entries")) return
        call get_value(table, "entries", entries_table, requested=.false.)
        if (.not. associated(entries_table)) return

        call entries_table%get_keys(keys)
        call self%ensure_capacity(size(keys))

        do i = 1, size(keys)
            call get_value(entries_table, keys(i)%key, entry_table, requested=.false.)
            if (.not. associated(entry_table)) cycle

            call get_value(entry_table, "size", self%entries(self%n_entries + 1)%size, &
                           error, 'source-cache')
            if (allocated(error)) then
                deallocate(error)
                return
            end if
            call get_value(entry_table, "mtime", self%entries(self%n_entries + 1)%mtime, &
                           error, 'source-cache')
            if (allocated(error)) then
                deallocate(error)
                return
            end if
            if (entry_table%has_key("config")) then
                call get_value(entry_table, "config", self%entries(self%n_entries + 1)%config, &
                               error, 'source-cache')
                if (allocated(error)) then
                    deallocate(error)
                    return
                end if
            end if
            call get_list(entry_table, "inputs", input_list, error)
            if (allocated(error)) then
                deallocate(error)
                return
            end if
            if (allocated(input_list)) then
                allocate (self%entries(self%n_entries + 1)%inputs(size(input_list)))
                do j = 1, size(input_list)
                    self%entries(self%n_entries + 1)%inputs(j)%s = input_list(j)%s
                end do
                deallocate (input_list)
            end if

            call self%entries(self%n_entries + 1)%source%load_from_toml(entry_table, error)
            if (allocated(error)) then
                deallocate(error)
                cycle
            end if

            if (.not. allocated(self%entries(self%n_entries + 1)%source%file_name)) cycle
            self%entries(self%n_entries + 1)%key = &
                canon_path(self%entries(self%n_entries + 1)%source%file_name)
            self%n_entries = self%n_entries + 1
        end do

        call table%destroy()
        deallocate(table)

    end subroutine ensure_loaded

    !> Grow the entry storage to hold at least `min_capacity` entries
    subroutine ensure_capacity(self, min_capacity)
        class(source_cache_t), intent(inout) :: self
        integer, intent(in) :: min_capacity

        type(cache_entry_t), allocatable :: tmp(:)
        integer :: new_capacity

        if (min_capacity <= 0) return
        if (allocated(self%entries)) then
            if (size(self%entries) >= min_capacity) return
        end if

        new_capacity = 8
        if (allocated(self%entries)) new_capacity = 2 * size(self%entries)
        do while (new_capacity < min_capacity)
            new_capacity = 2 * new_capacity
        end do

        allocate (tmp(new_capacity))
        if (self%n_entries > 0) tmp(1:self%n_entries) = self%entries(1:self%n_entries)
        call move_alloc(tmp, self%entries)

    end subroutine ensure_capacity

    !> Identify the fpm executable that is running
    !>
    !> Cached results are produced by the parser compiled into this executable.
    !> Its size and modification stamp change whenever fpm is rebuilt or
    !> upgraded, which is the only automatic signal for a change of the parse
    !> logic that leaves the cache file layout untouched.
    function program_fingerprint() result(fingerprint)
        character(:), allocatable :: fingerprint

        character(4096) :: argument
        character(:), allocatable :: path
        integer(int64) :: size, mtime
        character(len=32) :: value
        integer :: length, stat

        fingerprint = ''
        call get_command_argument(0, argument, length, stat)
        if (stat /= 0) return
        if (length <= 0) return

        path = trim(adjustl(argument(1:length)))
        if (exists(path)) then
            path = canon_path(path)
        else
            path = which(path)
        end if
        if (len_trim(path) == 0) return
        if (.not. file_info(path, size, mtime)) return

        write (value, '(i0)') size
        fingerprint = trim(value)
        write (value, '(i0)') mtime
        fingerprint = fingerprint//'|'//trim(value)

    end function program_fingerprint

    !> Whether a stored preprocessing fingerprint matches the requested one
    logical function same_config(stored, requested) result(matches)
        character(:), allocatable, intent(in) :: stored
        character(*), optional, intent(in) :: requested

        matches = .false.
        if (.not. allocated(stored)) then
            matches = .not. present(requested)
            return
        end if
        if (.not. present(requested)) return
        matches = stored == requested

    end function same_config

    !> Encode an input file as "path|size|mtime" using its current state
    subroutine encode_input(path, encoded)
        character(*), intent(in) :: path
        character(:), allocatable, intent(out) :: encoded

        integer(int64) :: size, mtime
        character(len=32) :: value

        encoded = ''
        if (.not. file_info(path, size, mtime)) return
        write(value, '(i0)') size
        encoded = path//'|'//trim(value)
        write(value, '(i0)') mtime
        encoded = encoded//'|'//trim(value)

    end subroutine encode_input

    !> Decode an "path|size|mtime" entry; an empty path marks an invalid entry
    subroutine decode_input(encoded, path, size, mtime)
        character(*), intent(in) :: encoded
        character(:), allocatable, intent(out) :: path
        integer(int64), intent(out) :: size, mtime

        integer :: sep1, sep2, ios

        path = ''
        size = 0
        mtime = 0

        sep2 = scan(encoded, '|', back=.true.)
        if (sep2 <= 1) return
        sep1 = scan(encoded(1:sep2 - 1), '|', back=.true.)
        if (sep1 <= 1) return
        ! The size and modification stamp fields must not be empty; a single
        ! character path such as "a|1|2" is a valid encoding.
        if (sep2 <= sep1 + 1) return
        if (sep2 >= len(encoded)) return

        read(encoded(sep1 + 1:sep2 - 1), *, iostat=ios) size
        if (ios /= 0) return
        read(encoded(sep2 + 1:), *, iostat=ios) mtime
        if (ios /= 0) return
        path = encoded(1:sep1 - 1)

    end subroutine decode_input

end module fpm_source_cache
