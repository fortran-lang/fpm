!> Package configuration data.
!>
!> This module provides the necessary procedure to translate a TOML document
!> to the corresponding Fortran type, while verifying it with respect to
!> its schema.
!>
!> Additionally, the required data types for users of this module are reexported
!> to hide the actual implementation details.
module fpm_manifest
    use fpm_manifest_build, only: build_config_t
    use fpm_manifest_executable, only : executable_config_t
    use fpm_manifest_dependency, only : dependency_config_t
    use fpm_manifest_library, only : library_config_t
    use fpm_manifest_package, only : package_config_t, new_package
    use fpm_error, only : error_t, fatal_error, file_not_found_error
    use fpm_toml, only : toml_table, read_package_file
    use fpm_manifest_test, only : test_config_t
    use fpm_filesystem, only: join_path, exists, basename
    implicit none
    private

    public :: get_package_data, default_executable, default_library, default_test
    public :: package_config_t, dependency_config_t


contains


    !> Populate library in case we find the default src directory
    subroutine default_library(self)

        !> Instance of the library meta data
        type(library_config_t), intent(out) :: self

        self%source_dir = "src"

    end subroutine default_library


    !> Populate executable in case we find the default app directory
    subroutine default_executable(self, name)

        !> Instance of the executable meta data
        type(executable_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name
        self%source_dir = "app"
        self%main = "main.f90"

    end subroutine default_executable

    !> Populate test in case we find the default test/ directory
    subroutine default_test(self, name)

        !> Instance of the executable meta data
        type(test_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name
        self%source_dir = "test"
        self%main = "main.f90"

    end subroutine default_test


    !> Obtain package meta data from a configuation file
    subroutine get_package_data(package, file, error, apply_defaults)

        !> Parsed package meta data
        type(package_config_t), intent(out) :: package

        !> Name of the package configuration file
        character(len=*), intent(in) :: file

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        !> Apply package defaults (uses file system operations)
        logical, intent(in), optional :: apply_defaults

        type(toml_table), allocatable :: table
        character(len=:), allocatable :: root

        call read_package_file(table, file, error)
        if (allocated(error)) return

        if (.not.allocated(table)) then
            call fatal_error(error, "Unclassified error while reading: '"//file//"'")
            return
        end if

        call new_package(package, table, error)
        if (allocated(error)) return

        if (present(apply_defaults)) then
            if (apply_defaults) then
                root = basename(file)
                if (root == file) root = "."
                call package_defaults(package, root, error)
                if (allocated(error)) return
            end if
        end if

    end subroutine get_package_data


    !> Apply package defaults
    subroutine package_defaults(package, root, error)

        !> Parsed package meta data
        type(package_config_t), intent(inout) :: package

        !> Current working directory
        character(len=*), intent(in) :: root

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        ! Populate library in case we find the default src directory
        if (.not.allocated(package%library) .and. &
            & exists(join_path(root, "src"))) then
            allocate(package%library)
            call default_library(package%library)
        end if

        ! Populate executable in case we find the default app
        if (.not.allocated(package%executable) .and. &
            & exists(join_path(root, "app", "main.f90"))) then
            allocate(package%executable(1))
            call default_executable(package%executable(1), package%name)
        end if

        ! Populate test in case we find the default test directory
        if (.not.allocated(package%test) .and. &
            & exists(join_path(root, "test", "main.f90"))) then
            allocate(package%test(1))
            call default_test(package%test(1), package%name)
        endif

        if (.not.(allocated(package%library) .or. allocated(package%executable))) then
            call fatal_error(error, "Neither library nor executable found, there is nothing to do")
            return
        end if

    end subroutine package_defaults


end module fpm_manifest
