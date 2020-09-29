!> Define the package data containing the meta data from the configuration file.
!
!  The package data defines a Fortran type corresponding to the respective
!  TOML document, after creating it from a package file no more interaction
!  with the TOML document is required.
!
!  Every configuration type provides it custom constructor (prefixed with `new_`)
!  and knows how to deserialize itself from a TOML document.
!  To ensure we find no untracked content in the package file all keywords are
!  checked and possible entries have to be explicitly allowed in the `check`
!  function.
!  If entries are mutally exclusive or interdependent inside the current table
!  the `check` function is required to enforce this schema on the data structure.
!
!  The package file root allows the following keywords
!
!  ```toml
!  name = "string"
!  version = "string"
!  license = "string"
!  author = "string"
!  maintainer = "string"
!  copyright = "string
!  [library]
!  [dependencies]
!  [dev-dependencies]
!  [[executable]]
!  [[test]]
!  ```
module fpm_manifest_package
    use fpm_manifest_build_config, only: build_config_t, new_build_config
    use fpm_manifest_dependency, only : dependency_t, new_dependencies
    use fpm_manifest_executable, only : executable_t, new_executable
    use fpm_manifest_library, only : library_t, new_library
    use fpm_manifest_test, only : test_t, new_test
    use fpm_error, only : error_t, fatal_error, syntax_error
    use fpm_toml, only : toml_table, toml_array, toml_key, toml_stat, get_value, &
        & len
    implicit none
    private

    public :: package_t, new_package


    !> Package meta data
    type :: package_t

        !> Name of the package
        character(len=:), allocatable :: name

        !> Build configuration data
        type(build_config_t), allocatable :: build_config

        !> Library meta data
        type(library_t), allocatable :: library

        !> Executable meta data
        type(executable_t), allocatable :: executable(:)

        !> Dependency meta data
        type(dependency_t), allocatable :: dependency(:)

        !> Development dependency meta data
        type(dependency_t), allocatable :: dev_dependency(:)

        !> Test meta data
        type(test_t), allocatable :: test(:)

    contains

        !> Print information on this instance
        procedure :: info

    end type package_t


contains


    !> Construct a new package configuration from a TOML data structure
    subroutine new_package(self, table, error)

        !> Instance of the package configuration
        type(package_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: child, node
        type(toml_array), pointer :: children
        integer :: ii, nn, stat

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve package name")
           return
        end if

        call get_value(table, "build", child, requested=.false.)
        if (associated(child)) then
            allocate(self%build_config)
            call new_build_config(self%build_config, child, error)
            if (allocated(error)) return
        end if

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, error)
            if (allocated(error)) return
        end if

        call get_value(table, "dev-dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dev_dependency, child, error)
            if (allocated(error)) return
        end if

        call get_value(table, "library", child, requested=.false.)
        if (associated(child)) then
            allocate(self%library)
            call new_library(self%library, child, error)
            if (allocated(error)) return
        end if

        call get_value(table, "executable", children, requested=.false.)
        if (associated(children)) then
            nn = len(children)
            allocate(self%executable(nn))
            do ii = 1, nn
                call get_value(children, ii, node, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Could not retrieve executable from array entry")
                    exit
                end if
                call new_executable(self%executable(ii), node, error)
                if (allocated(error)) exit
            end do
            if (allocated(error)) return
        end if

        call get_value(table, "test", children, requested=.false.)
        if (associated(children)) then
            nn = len(children)
            allocate(self%test(nn))
            do ii = 1, nn
                call get_value(children, ii, node, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Could not retrieve test from array entry")
                    exit
                end if
                call new_test(self%test(ii), node, error)
                if (allocated(error)) exit
            end do
            if (allocated(error)) return
        end if

    end subroutine new_package


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        logical :: name_present
        integer :: ikey

        name_present = .false.

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Package file is empty")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in package file")
                exit

            case("name")
                name_present = .true.

            case("version", "license", "author", "maintainer", "copyright", &
                    & "description", "keywords", "categories", "homepage", "build", &
                    & "dependencies", "dev-dependencies", "test", "executable", &
                    & "library")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Package name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the package configuration
        class(package_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ii
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)', &
            & fmti = '("#", 1x, a, t30, i0)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Package"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if

        if (allocated(self%build_config)) then
            write(unit, fmt) "- build configuration", ""
            call self%build_config%info(unit, pr - 1)
        end if

        if (allocated(self%library)) then
            write(unit, fmt) "- target", "archive"
            call self%library%info(unit, pr - 1)
        end if

        if (allocated(self%executable)) then
            if (size(self%executable) > 1 .or. pr > 2) then
                write(unit, fmti) "- executables", size(self%executable)
            end if
            do ii = 1, size(self%executable)
                call self%executable(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%dependency)) then
            if (size(self%dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- dependencies", size(self%dependency)
            end if
            do ii = 1, size(self%dependency)
                call self%dependency(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%test)) then
            if (size(self%test) > 1 .or. pr > 2) then
                write(unit, fmti) "- tests", size(self%test)
            end if
            do ii = 1, size(self%test)
                call self%test(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%dev_dependency)) then
            if (size(self%dev_dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- development deps.", size(self%dev_dependency)
            end if
            do ii = 1, size(self%dev_dependency)
                call self%dev_dependency(ii)%info(unit, pr - 1)
            end do
        end if

    end subroutine info


end module fpm_manifest_package
