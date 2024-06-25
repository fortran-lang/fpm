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
    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    use fpm_environment
    implicit none
    private

    public :: metapackage_config_t, new_meta_config, is_meta_package
    public :: metapackage_request_t, new_meta_request


    !> Configuration data for a single metapackage request
    type :: metapackage_request_t

        !> Request flag
        logical :: on = .false.

        !> Metapackage name
        character(len=:), allocatable :: name

        !> Version Specification string
        character(len=:), allocatable :: version

    end type metapackage_request_t


    !> Configuration data for metapackages
    type :: metapackage_config_t

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

    end type metapackage_config_t


contains

    !> Destroy a metapackage request
    elemental subroutine request_destroy(self)

        !> Instance of the request
        class(metapackage_request_t), intent(inout) :: self

        self%on = .false.
        if (allocated(self%version)) deallocate(self%version)
        if (allocated(self%name)) deallocate(self%name)

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


        integer :: stat,i
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

        !> The toml table is not checked here because it already passed
        !> the "new_dependencies" check

        call table%get_keys(keys)

        !> Set list of entries that are allowed to be metapackages
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

            ! Skip standard dependencies
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

        ! Key is not present, metapackage not requested
        return

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

    end subroutine new_meta_config

    !> Check local schema for allowed entries
    logical function is_meta_package(key)

        !> Instance of the TOML data structure
        character(*), intent(in) :: key

        select case (key)

            !> Supported metapackages
            case ("openmp","stdlib","mpi","minpack","hdf5")
                is_meta_package = .true.

            case default
                is_meta_package = .false.

        end select

    end function is_meta_package

end module fpm_manifest_metapackages
