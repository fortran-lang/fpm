!> Package configuration data.
!
!  This module provides the necessary procedure to translate a TOML document
!  to the corresponding Fortran type, while verifying it with respect to
!  its schema.
!
!  Additionally, the required data types for users of this module are reexported
!  to hide the actual implementation details.
module fpm_manifest
    use fpm_manifest_build_config, only: build_config_t
    use fpm_manifest_executable, only : executable_t
    use fpm_manifest_library, only : library_t
    use fpm_manifest_package, only : package_t, new_package
    use fpm_error, only : error_t, fatal_error, file_not_found_error
    use fpm_toml, only : toml_table, read_package_file
    implicit none
    private

    public :: get_package_data, default_executable, default_library
    public :: package_t


contains


    !> Populate library in case we find the default src directory
    subroutine default_library(self)

        !> Instance of the library meta data
        type(library_t), intent(out) :: self

        self%source_dir = "src"

    end subroutine default_library


    !> Populate executable in case we find the default app directory
    subroutine default_executable(self, name)

        !> Instance of the executable meta data
        type(executable_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name
        self%source_dir = "app"
        self%main = "main.f90"

    end subroutine default_executable


    !> Obtain package meta data from a configuation file
    subroutine get_package_data(package, file, error)

        !> Parsed package meta data
        type(package_t), intent(out) :: package

        !> Name of the package configuration file
        character(len=*), intent(in) :: file

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table

        call read_package_file(table, file, error)
        if (allocated(error)) return

        if (.not.allocated(table)) then
            call fatal_error(error, "Unclassified error while reading: '"//file//"'")
            return
        end if

        call new_package(package, table, error)

    end subroutine get_package_data


end module fpm_manifest
