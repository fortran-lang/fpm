!> Define the package data containing the meta data from the configuration file.
!>
!> The package data defines a Fortran type corresponding to the respective
!> TOML document, after creating it from a package file no more interaction
!> with the TOML document is required.
!>
!> Every configuration type provides it custom constructor (prefixed with `new_`)
!> and knows how to deserialize itself from a TOML document.
!> To ensure we find no untracked content in the package file all keywords are
!> checked and possible entries have to be explicitly allowed in the `check`
!> function.
!> If entries are mutally exclusive or interdependent inside the current table
!> the `check` function is required to enforce this schema on the data structure.
!>
!> The package file root allows the following keywords
!>
!>```toml
!>name = "string"
!>version = "string"
!>license = "string"
!>author = "string"
!>maintainer = "string"
!>copyright = "string"
!>[library]
!>[dependencies]
!>[dev-dependencies]
!>[build]
!>[install]
!>[[ executable ]]
!>[[ example ]]
!>[[ test ]]
!>```
module fpm_manifest_package
    use fpm_manifest_build, only: build_config_t, new_build_config
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies
    use fpm_manifest_example, only : example_config_t, new_example
    use fpm_manifest_executable, only : executable_config_t, new_executable
    use fpm_manifest_library, only : library_config_t, new_library
    use fpm_manifest_install, only: install_config_t, new_install_config
    use fpm_manifest_test, only : test_config_t, new_test
    use fpm_error, only : error_t, fatal_error, syntax_error
    use fpm_toml, only : toml_table, toml_array, toml_key, toml_stat, get_value, &
        & len
    use fpm_versioning, only : version_t, new_version
    implicit none
    private

    public :: package_config_t, new_package


    interface unique_programs
        module procedure :: unique_programs1
        module procedure :: unique_programs2
    end interface unique_programs


    !> Package meta data
    type :: package_config_t

        !> Name of the package
        character(len=:), allocatable :: name

        !> Package version
        type(version_t) :: version

        !> Build configuration data
        type(build_config_t) :: build

        !> Installation configuration data
        type(install_config_t) :: install

        !> Library meta data
        type(library_config_t), allocatable :: library

        !> Executable meta data
        type(executable_config_t), allocatable :: executable(:)

        !> Dependency meta data
        type(dependency_config_t), allocatable :: dependency(:)

        !> Development dependency meta data
        type(dependency_config_t), allocatable :: dev_dependency(:)

        !> Example meta data
        type(example_config_t), allocatable :: example(:)

        !> Test meta data
        type(test_config_t), allocatable :: test(:)

    contains

        !> Print information on this instance
        procedure :: info

    end type package_config_t


contains


    !> Construct a new package configuration from a TOML data structure
    subroutine new_package(self, table, error)

        !> Instance of the package configuration
        type(package_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        ! Backspace (8), tabulator (9), newline (10), formfeed (12) and carriage
        ! return (13) are invalid in package names
        character(len=*), parameter :: invalid_chars = &
           achar(8) // achar(9) // achar(10) // achar(12) // achar(13)
        type(toml_table), pointer :: child, node
        type(toml_array), pointer :: children
        character(len=:), allocatable :: version
        integer :: ii, nn, stat

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve package name")
           return
        end if

        if (len(self%name) <= 0) then
            call syntax_error(error, "Package name must be a non-empty string")
            return
        end if

        ii = scan(self%name, invalid_chars)
        if (ii > 0) then
            call syntax_error(error, "Package name contains invalid characters")
            return
        end if

        call get_value(table, "build", child, requested=.true., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error, "Type mismatch for build entry, must be a table")
            return
        end if
        call new_build_config(self%build, child, error)
        if (allocated(error)) return

        call get_value(table, "install", child, requested=.true., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error, "Type mismatch for install entry, must be a table")
            return
        end if
        call new_install_config(self%install, child, error)
        if (allocated(error)) return
        
        call get_value(table, "version", version, "0")
        call new_version(self%version, version, error)
        if (allocated(error)) return

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

            call unique_programs(self%executable, error)
            if (allocated(error)) return
        end if

        call get_value(table, "example", children, requested=.false.)
        if (associated(children)) then
            nn = len(children)
            allocate(self%example(nn))
            do ii = 1, nn
                call get_value(children, ii, node, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Could not retrieve example from array entry")
                    exit
                end if
                call new_example(self%example(ii), node, error)
                if (allocated(error)) exit
            end do
            if (allocated(error)) return

            call unique_programs(self%example, error)
            if (allocated(error)) return

            if (allocated(self%executable)) then
                call unique_programs(self%executable, self%example, error)
                if (allocated(error)) return
            end if
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

            call unique_programs(self%test, error)
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
                    & "example", "library", "install")
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
        class(package_config_t), intent(in) :: self

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

        call self%build%info(unit, pr - 1)

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

        if (allocated(self%example)) then
            if (size(self%example) > 1 .or. pr > 2) then
                write(unit, fmti) "- examples", size(self%example)
            end if
            do ii = 1, size(self%example)
                call self%example(ii)%info(unit, pr - 1)
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


    !> Check whether or not the names in a set of executables are unique
    subroutine unique_programs1(executable, error)

        !> Array of executables
        class(executable_config_t), intent(in) :: executable(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i, j

        do i = 1, size(executable)
            do j = 1, i - 1
                if (executable(i)%name == executable(j)%name) then
                    call fatal_error(error, "The program named '"//&
                        executable(j)%name//"' is duplicated. "//&
                        "Unique program names are required.")
                    exit
                end if
            end do
        end do
        if (allocated(error)) return

    end subroutine unique_programs1


    !> Check whether or not the names in a set of executables are unique
    subroutine unique_programs2(executable_i, executable_j, error)

        !> Array of executables
        class(executable_config_t), intent(in) :: executable_i(:)

        !> Array of executables
        class(executable_config_t), intent(in) :: executable_j(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i, j

        do i = 1, size(executable_i)
            do j = 1, size(executable_j)
                if (executable_i(i)%name == executable_j(j)%name) then
                    call fatal_error(error, "The program named '"//&
                        executable_j(j)%name//"' is duplicated. "//&
                        "Unique program names are required.")
                    exit
                end if
            end do
        end do
        if (allocated(error)) return

    end subroutine unique_programs2


end module fpm_manifest_package
