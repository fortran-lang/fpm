!> Implementation of the metapackage configuration data.
!>
!> A metapackage table can currently have the following fields
!>
!>```toml
!>[metapackages]
!>fpm = "0.1.0"
!>openmp = bool
!>stdlib = bool
!>```
module fpm_manifest_metapackages
    use fpm_error,       only: error_t, fatal_error, syntax_error
    use tomlf,           only: toml_table, toml_key
    use fpm_toml,        only: get_value, set_value, set_string, add_table, serializable_t
    use fpm_environment
    implicit none
    private

    public :: metapackage_config_t, new_meta_config, is_meta_package
    public :: metapackage_request_t, new_meta_request

    !> Configuration data for a single metapackage request
    type, extends(serializable_t) :: metapackage_request_t

        !> Request flag
        logical :: on = .false.

        !> Metapackage name
        character(len=:), allocatable :: name

        !> Version Specification string
        character(len=:), allocatable :: version

      contains
      
        procedure :: serializable_is_same => meta_request_same
        procedure :: dump_to_toml        => meta_request_dump
        procedure :: load_from_toml      => meta_request_load
        
    end type metapackage_request_t


    !> Configuration data for metapackages
    type, extends(serializable_t) :: metapackage_config_t

        !> Request MPI support
        type(metapackage_request_t) :: mpi

        !> Request OpenMP support
        type(metapackage_request_t) :: openmp

        !> Request stdlib support
        type(metapackage_request_t) :: stdlib

        !> fortran-lang minpack
        type(metapackage_request_t) :: minpack

        !> HDF5
        type(metapackage_request_t) :: hdf5

        !> NetCDF
        type(metapackage_request_t) :: netcdf

        !> BLAS
        type(metapackage_request_t) :: blas
        
        contains

           procedure :: get_requests

           !> Merge another config into this one (for propagating dependency metapackages)
           procedure :: merge => meta_config_merge

           ! Cleanup configuration; assert package names
           final     :: meta_config_final
           procedure :: reset => meta_config_reset

           procedure :: serializable_is_same => meta_config_same
           procedure :: dump_to_toml        => meta_config_dump
           procedure :: load_from_toml      => meta_config_load

    end type metapackage_config_t


contains

    !> Destroy a metapackage request
    elemental subroutine request_destroy(self)
        class(metapackage_request_t), intent(inout) :: self
        self%on = .false.
        if (allocated(self%version)) deallocate(self%version)
        if (allocated(self%name))    deallocate(self%name)
    end subroutine request_destroy

    !> Parse version string of a metapackage request
    subroutine request_parse(self, version_request, error)
        
        ! Instance of this metapackage
        type(metapackage_request_t), intent(inout) :: self
        
        ! Parse version request
        character(len=*), intent(in) :: version_request

        ! Error message
        type(error_t), allocatable, intent(out) :: error

        ! wildcard = use any versions
        if (version_request=="*") then
            
            ! Any version is OK
            self%on = .true.
            self%version = version_request
            
        else

            call fatal_error(error,'Value <'//version_request//'> for metapackage '//self%name//&
                                   'is not currently supported. Try "*" instead. ')
            return

        end if

    end subroutine request_parse

    !> Construct a new metapackage request from the dependencies table
    subroutine new_meta_request(self, key, table, meta_allowed, error)
        type(metapackage_request_t), intent(out) :: self

        !> The package name
        character(len=*), intent(in) :: key

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> List of keys allowed to be metapackages
        logical, intent(in), optional :: meta_allowed(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: value
        logical, allocatable :: allow_meta(:)
        type(toml_key), allocatable :: keys(:)

        call request_destroy(self)

        !> Set name
        self%name = key
        if (.not.is_meta_package(key)) then
            call fatal_error(error,"Error reading fpm.toml: <"//key//"> is not a valid metapackage name")
            return
        end if

        call table%get_keys(keys)

        if (present(meta_allowed)) then
            if (size(meta_allowed)/=size(keys)) then
                 call fatal_error(error,"Internal error: list of metapackage-enable entries does not match table size")
                 return
            end if
            allow_meta = meta_allowed
        else
            allocate(allow_meta(size(keys)),source=.true.)
        endif

        do i=1,size(keys)
            if (.not.allow_meta(i)) cycle
            if (keys(i)%key==key) then
                call get_value(table, key, value)
                if (.not. allocated(value)) then
                    call syntax_error(error, "Could not retrieve version string for metapackage key <"//key//">. Check syntax")
                    return
                else
                    call request_parse(self, value, error)
                    return
                endif
            end if
        end do
        ! If we reach here, key not present => request remains off
    end subroutine new_meta_request

    !> Construct a new build configuration from a TOML data structure
    subroutine new_meta_config(self, table, meta_allowed, error)

        !> Instance of the build configuration
        type(metapackage_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> List of keys allowed to be metapackages
        logical, intent(in) :: meta_allowed(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: stat

        !> The toml table is not checked here because it already passed
        !> the "new_dependencies" check
        call new_meta_request(self%openmp, "openmp", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%stdlib, "stdlib", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%minpack, "minpack", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%mpi, "mpi", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%hdf5, "hdf5", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%netcdf, "netcdf", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%blas, "blas", table, meta_allowed, error)
        if (allocated(error)) return
        
    end subroutine new_meta_config

    !> Check local schema for allowed entries
    logical function is_meta_package(key)
        character(*), intent(in) :: key
        select case (key)
            case ("openmp","stdlib","mpi","minpack","hdf5","netcdf","blas")
                is_meta_package = .true.
            case default
                is_meta_package = .false.
        end select
    end function is_meta_package

    !> Return a list of metapackages requested for the current build
    function get_requests(meta) result(requests)
        class(metapackage_config_t), intent(in) :: meta
        type(metapackage_request_t), allocatable :: requests(:)
        integer :: nreq

        nreq = count([ meta%mpi%on, &
                       meta%openmp%on, &
                       meta%stdlib%on, &
                       meta%minpack%on, &
                       meta%hdf5%on, &
                       meta%netcdf%on, &
                       meta%blas%on ]) 

        allocate(requests(nreq)); if (nreq <= 0) return

        nreq = 0
        call add_if_active(meta%mpi    ,requests,nreq)
        call add_if_active(meta%openmp ,requests,nreq)
        call add_if_active(meta%stdlib ,requests,nreq)
        call add_if_active(meta%minpack,requests,nreq)
        call add_if_active(meta%hdf5   ,requests,nreq)
        call add_if_active(meta%netcdf ,requests,nreq)
        call add_if_active(meta%blas   ,requests,nreq)
        
        contains
        
        subroutine add_if_active(req,list,count)
            type(metapackage_request_t), intent(in) :: req
            type(metapackage_request_t), intent(inout) :: list(:)
            integer, intent(inout) :: count
            if (.not.req%on) return
            count = count+1
            list(count) = req
        end subroutine add_if_active
        
    end function get_requests

    !> Merge another metapackage configuration into this one
    !> This is used to propagate metapackage requests from dependencies to the main package
    subroutine meta_config_merge(self, other)
        class(metapackage_config_t), intent(inout) :: self
        type(metapackage_config_t), intent(in) :: other

        ! Merge each metapackage request: if 'other' has it on, enable it in self
        call merge_request(self%openmp, other%openmp)
        call merge_request(self%stdlib, other%stdlib)
        call merge_request(self%minpack, other%minpack)
        call merge_request(self%mpi, other%mpi)
        call merge_request(self%hdf5, other%hdf5)
        call merge_request(self%netcdf, other%netcdf)
        call merge_request(self%blas, other%blas)

    contains

        subroutine merge_request(self_req, other_req)
            type(metapackage_request_t), intent(inout) :: self_req
            type(metapackage_request_t), intent(in) :: other_req

            ! If other request is on and self is off, copy the request
            if (other_req%on .and. .not. self_req%on) then
                self_req%on = .true.
                if (allocated(other_req%version)) self_req%version = other_req%version
            end if
        end subroutine merge_request

    end subroutine meta_config_merge

    logical function meta_request_same(this, that)
        class(metapackage_request_t), intent(in) :: this
        class(serializable_t),        intent(in) :: that

        meta_request_same = .false.
        select type (other => that)
        type is (metapackage_request_t)
            if (this%on .neqv. other%on) return
            if (allocated(this%name)   .neqv. allocated(other%name))   return
            if (allocated(this%version).neqv. allocated(other%version))return
            if (allocated(this%name))    then; if (this%name   /= other%name)   return; end if
            if (allocated(this%version)) then; if (this%version/= other%version)return; end if
        class default
            return
        end select
        meta_request_same = .true.
    end function meta_request_same

    subroutine meta_request_dump(self, table, error)
        class(metapackage_request_t), intent(inout) :: self
        type(toml_table),             intent(inout) :: table
        type(error_t), allocatable,   intent(out)   :: error

        call set_value (table, "on",      self%on,     error, 'metapackage_request_t')        
        if (allocated(error)) return
        
        call set_string(table, "name",    self%name,   error)
        if (allocated(error)) return
        
        call set_string(table, "version", self%version,error)
        if (allocated(error)) return
    end subroutine meta_request_dump

    subroutine meta_request_load(self, table, error)
        class(metapackage_request_t), intent(inout) :: self
        type(toml_table),             intent(inout) :: table
        type(error_t), allocatable,   intent(out)   :: error

        call get_value(table, "on",      self%on)
        call get_value(table, "name",    self%name)
        call get_value(table, "version", self%version)
        
    end subroutine meta_request_load

    logical function meta_config_same(this, that)
        class(metapackage_config_t), intent(in) :: this
        class(serializable_t),       intent(in) :: that

        meta_config_same = .false.
        select type (other => that)
            type is (metapackage_config_t)
                
                if (.not. this%mpi     == other%mpi)    return
                if (.not. this%openmp  == other%openmp) return
                if (.not. this%stdlib  == other%stdlib) return
                if (.not. this%minpack == other%minpack)return
                if (.not. this%hdf5    == other%hdf5)   return
                if (.not. this%netcdf  == other%netcdf) return
                if (.not. this%blas    == other%blas)   return
                
                meta_config_same = .true.
                
            class default
                return
        end select

    end function meta_config_same

    subroutine meta_config_dump(self, table, error)
        class(metapackage_config_t), intent(inout) :: self
        type(toml_table),            intent(inout) :: table
        type(error_t), allocatable,  intent(out)   :: error

        type(toml_table), pointer :: ptr

        ! openmp
        call add_table(table, "openmp", ptr); if (.not.associated(ptr)) then
            call fatal_error(error, "metapackage_config_t: cannot create 'openmp' table"); return
        end if
        call self%openmp%dump_to_toml(ptr, error); if (allocated(error)) return

        ! stdlib
        call add_table(table, "stdlib", ptr); if (.not.associated(ptr)) then
            call fatal_error(error, "metapackage_config_t: cannot create 'stdlib' table"); return
        end if
        call self%stdlib%dump_to_toml(ptr, error); if (allocated(error)) return

        ! minpack
        call add_table(table, "minpack", ptr); if (.not.associated(ptr)) then
            call fatal_error(error, "metapackage_config_t: cannot create 'minpack' table"); return
        end if
        call self%minpack%dump_to_toml(ptr, error); if (allocated(error)) return

        ! mpi
        call add_table(table, "mpi", ptr); if (.not.associated(ptr)) then
            call fatal_error(error, "metapackage_config_t: cannot create 'mpi' table"); return
        end if
        call self%mpi%dump_to_toml(ptr, error); if (allocated(error)) return

        ! hdf5
        call add_table(table, "hdf5", ptr); if (.not.associated(ptr)) then
            call fatal_error(error, "metapackage_config_t: cannot create 'hdf5' table"); return
        end if
        call self%hdf5%dump_to_toml(ptr, error); if (allocated(error)) return

        ! netcdf
        call add_table(table, "netcdf", ptr); if (.not.associated(ptr)) then
            call fatal_error(error, "metapackage_config_t: cannot create 'netcdf' table"); return
        end if
        call self%netcdf%dump_to_toml(ptr, error); if (allocated(error)) return

        ! blas
        call add_table(table, "blas", ptr); if (.not.associated(ptr)) then
            call fatal_error(error, "metapackage_config_t: cannot create 'blas' table"); return
        end if
        call self%blas%dump_to_toml(ptr, error); if (allocated(error)) return
    end subroutine meta_config_dump
    
    ! Ensure the names of all packages are always defined
    subroutine meta_config_final(self)
        type(metapackage_config_t), intent(inout) :: self    
        call meta_config_reset(self)            
    end subroutine meta_config_final

    ! Ensure the names of all packages are always defined
    subroutine meta_config_reset(self)
        class(metapackage_config_t), intent(inout) :: self
        
        call request_destroy(self%openmp); self%openmp%name = "openmp"
        call request_destroy(self%stdlib); self%stdlib%name = "stdlib"
        call request_destroy(self%minpack);self%minpack%name= "minpack"
        call request_destroy(self%mpi);    self%mpi%name    = "mpi"
        call request_destroy(self%hdf5);   self%hdf5%name   = "hdf5"
        call request_destroy(self%netcdf); self%netcdf%name = "netcdf"
        call request_destroy(self%blas);   self%blas%name   = "blas"        
        
    end subroutine meta_config_reset

    subroutine meta_config_load(self, table, error)
        class(metapackage_config_t), intent(inout) :: self
        type(toml_table),            intent(inout) :: table
        type(error_t), allocatable,  intent(out)   :: error

        type(toml_table), pointer :: ptr

        ! openmp
        call get_value(table, "openmp", ptr)
        if (associated(ptr)) call self%openmp%load_from_toml(ptr, error); if (allocated(error)) return

        ! stdlib
        call get_value(table, "stdlib", ptr)
        if (associated(ptr)) call self%stdlib%load_from_toml(ptr, error); if (allocated(error)) return

        ! minpack
        call get_value(table, "minpack", ptr)
        if (associated(ptr)) call self%minpack%load_from_toml(ptr, error); if (allocated(error)) return

        ! mpi
        call get_value(table, "mpi", ptr)
        if (associated(ptr)) call self%mpi%load_from_toml(ptr, error); if (allocated(error)) return

        ! hdf5
        call get_value(table, "hdf5", ptr)
        if (associated(ptr)) call self%hdf5%load_from_toml(ptr, error); if (allocated(error)) return

        ! netcdf
        call get_value(table, "netcdf", ptr)
        if (associated(ptr)) call self%netcdf%load_from_toml(ptr, error); if (allocated(error)) return

        ! blas
        call get_value(table, "blas", ptr)
        if (associated(ptr)) call self%blas%load_from_toml(ptr, error); if (allocated(error)) return
    end subroutine meta_config_load

end module fpm_manifest_metapackages
