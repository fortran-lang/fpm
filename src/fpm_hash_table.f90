!> Hash table data structures for efficient string-based lookups.
!>
!> Provides a string-keyed hash map with integer values using FNV-1a hashing
!> and linear probing for collision resolution. Automatically resizes when
!> load factor exceeds threshold.
module fpm_hash_table
    use, intrinsic :: iso_fortran_env, only: int64
    use fpm_strings, only: fnv_1a
    implicit none
    private

    public :: string_hash_map_t

    !> Entry in the hash table
    type :: hash_entry
        character(:), allocatable :: key
        integer :: value
    end type

    !> String-keyed hash map with integer values
    !>
    !> Supports both map operations (key-value storage) and set operations
    !> (membership testing). Automatically resizes when load factor exceeds
    !> threshold (default 0.75).
    type :: string_hash_map_t
        type(hash_entry), allocatable :: table(:)
        integer :: capacity = 0
        integer :: count = 0
        real :: load_factor_threshold = 0.75
    contains
        procedure :: init => hash_map_init
        procedure :: set => hash_map_set
        procedure :: get => hash_map_get
        procedure :: contains => hash_map_contains
        procedure :: increment => hash_map_increment
        procedure :: destroy => hash_map_destroy
        procedure, private :: resize => hash_map_resize
    end type

contains

    !> Initialize hash map with given capacity
    subroutine hash_map_init(self, capacity)
        class(string_hash_map_t), intent(inout) :: self
        integer, intent(in) :: capacity

        if (capacity <= 0) then
            error stop "hash_map_init: capacity must be positive"
        end if

        self%capacity = capacity
        self%count = 0
        allocate(self%table(capacity))
    end subroutine hash_map_init

    !> Set key-value pair (inserts new or updates existing)
    !>
    !> Automatically resizes table if load factor exceeds threshold
    subroutine hash_map_set(self, key, value)
        class(string_hash_map_t), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        integer(int64) :: hash_val
        integer :: hash_idx, probe, idx

        ! Auto-resize if load factor exceeds threshold
        if (real(self%count) / real(self%capacity) >= self%load_factor_threshold) then
            call self%resize()
        end if

        ! FNV-1a hash
        hash_val = fnv_1a(key)
        hash_idx = modulo(iand(hash_val, huge(hash_val)), int(self%capacity, int64)) + 1

        ! Linear probing
        do probe = 0, self%capacity - 1
            idx = modulo(hash_idx + probe - 1, self%capacity) + 1

            if (.not. allocated(self%table(idx)%key)) then
                ! Empty slot - insert new entry
                self%table(idx)%key = trim(key)
                self%table(idx)%value = value
                self%count = self%count + 1
                return
            else if (trim(self%table(idx)%key) == trim(key)) then
                ! Key exists - update value
                self%table(idx)%value = value
                return
            end if
        end do

        ! Should never reach here if resize works correctly
        error stop "hash_map_set: table full despite resize"
    end subroutine hash_map_set

    !> Get value for key
    !>
    !> Returns .true. if key found, .false. otherwise
    function hash_map_get(self, key, value) result(found)
        class(string_hash_map_t), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(out) :: value
        logical :: found
        integer(int64) :: hash_val
        integer :: hash_idx, probe, idx

        found = .false.
        hash_val = fnv_1a(key)
        hash_idx = modulo(iand(hash_val, huge(hash_val)), int(self%capacity, int64)) + 1

        do probe = 0, self%capacity - 1
            idx = modulo(hash_idx + probe - 1, self%capacity) + 1

            if (.not. allocated(self%table(idx)%key)) then
                return  ! Not found
            else if (trim(self%table(idx)%key) == trim(key)) then
                value = self%table(idx)%value
                found = .true.
                return
            end if
        end do
    end function hash_map_get

    !> Check if key exists in map
    function hash_map_contains(self, key) result(found)
        class(string_hash_map_t), intent(in) :: self
        character(len=*), intent(in) :: key
        logical :: found
        integer(int64) :: hash_val
        integer :: hash_idx, probe, idx

        found = .false.
        hash_val = fnv_1a(key)
        hash_idx = modulo(iand(hash_val, huge(hash_val)), int(self%capacity, int64)) + 1

        do probe = 0, self%capacity - 1
            idx = modulo(hash_idx + probe - 1, self%capacity) + 1

            if (.not. allocated(self%table(idx)%key)) then
                return  ! Not found
            else if (trim(self%table(idx)%key) == trim(key)) then
                found = .true.
                return
            end if
        end do
    end function hash_map_contains

    !> Increment value for key (or set to 1 if new)
    subroutine hash_map_increment(self, key)
        class(string_hash_map_t), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer :: current_value

        if (self%get(key, current_value)) then
            call self%set(key, current_value + 1)
        else
            call self%set(key, 1)
        end if
    end subroutine hash_map_increment

    !> Deallocate hash map
    subroutine hash_map_destroy(self)
        class(string_hash_map_t), intent(inout) :: self
        if (allocated(self%table)) deallocate(self%table)
        self%capacity = 0
        self%count = 0
    end subroutine hash_map_destroy

    !> Resize table (doubles capacity and rehashes all entries)
    subroutine hash_map_resize(self)
        class(string_hash_map_t), intent(inout) :: self
        type(hash_entry), allocatable :: old_table(:)
        integer :: old_capacity, i, new_capacity

        ! Save old table
        old_capacity = self%capacity
        call move_alloc(self%table, old_table)

        ! Allocate new table with double capacity
        new_capacity = old_capacity * 2
        self%capacity = new_capacity
        self%count = 0
        allocate(self%table(new_capacity))

        ! Rehash all entries from old table
        do i = 1, old_capacity
            if (allocated(old_table(i)%key)) then
                call self%set(old_table(i)%key, old_table(i)%value)
            end if
        end do

        ! Old table automatically deallocated
    end subroutine hash_map_resize

end module fpm_hash_table
