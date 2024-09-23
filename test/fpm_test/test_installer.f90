!> Define tests for the `fpm_installer` module
!>
!> The tests here setup a mock environment to allow testing for Unix and Windows
!> platforms at the same time.
module test_installer
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, &
        & check_string
    use fpm_environment, only : OS_WINDOWS, OS_LINUX
    use fpm_filesystem, only : join_path
    use fpm_installer
    implicit none
    private

    public :: collect_installer


    type, extends(installer_t) :: mock_installer_t
        character(len=:), allocatable :: expected_dir
        character(len=:), allocatable :: expected_run
    contains
        procedure :: make_dir
        procedure :: run
    end type mock_installer_t

contains

    !> Collect all exported unit tests
    subroutine collect_installer(testsuite)
        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("install-lib", test_install_lib), &
            & new_unittest("install-pkgconfig", test_install_pkgconfig), &
            & new_unittest("install-sitepackages", test_install_sitepackages), &
            & new_unittest("install-mod", test_install_mod), &
            & new_unittest("install-exe-unix", test_install_exe_unix), &
            & new_unittest("install-exe-win", test_install_exe_win), &
            & new_unittest("install-test-unix", test_install_tests_unix), &
            & new_unittest("install-test-win", test_install_tests_win)]

    end subroutine collect_installer

    subroutine test_install_exe_unix(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(mock_installer_t) :: mock
        type(installer_t) :: installer

        call new_installer(installer, prefix="PREFIX", verbosity=0, copy="mock")
        mock%installer_t = installer
        mock%os = OS_LINUX
        mock%expected_dir = "PREFIX/bin"
        mock%expected_run = 'mock "name" "'//mock%expected_dir//'"'

        call mock%install_executable("name", error)

    end subroutine test_install_exe_unix

    subroutine test_install_exe_win(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(mock_installer_t) :: mock
        type(installer_t) :: installer

        call new_installer(installer, prefix="PREFIX", verbosity=0, copy="mock")
        mock%installer_t = installer
        mock%os = OS_WINDOWS
        mock%expected_dir = "PREFIX\bin"
        mock%expected_run = 'mock "name.exe" "'//mock%expected_dir//'"'

        call mock%install_executable("name", error)

    end subroutine test_install_exe_win

    subroutine test_install_tests_unix(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(mock_installer_t) :: mock
        type(installer_t) :: installer

        call new_installer(installer, prefix="PREFIX", testdir="tdir", verbosity=0, copy="mock")
        mock%installer_t = installer
        mock%os = OS_LINUX
        mock%expected_dir = "PREFIX/tdir"
        mock%expected_run = 'mock "name" "'//mock%expected_dir//'"'

        call mock%install_test("name", error)

    end subroutine test_install_tests_unix

    subroutine test_install_tests_win(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(mock_installer_t) :: mock
        type(installer_t) :: installer

        call new_installer(installer, prefix="PREFIX", testdir="tdir", verbosity=0, copy="mock")
        mock%installer_t = installer
        mock%os = OS_WINDOWS
        mock%expected_dir = "PREFIX\tdir"
        mock%expected_run = 'mock "name.exe" "'//mock%expected_dir//'"'

        call mock%install_test("name", error)

    end subroutine test_install_tests_win

    subroutine test_install_lib(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(mock_installer_t) :: mock
        type(installer_t) :: installer

        call new_installer(installer, prefix="PREFIX", verbosity=0, copy="mock")
        mock%installer_t = installer
        mock%expected_dir = join_path("PREFIX", "lib")
        mock%expected_run = 'mock "name" "'//join_path("PREFIX", "lib")//'"'

        call mock%install_library("name", error)

    end subroutine test_install_lib

    subroutine test_install_pkgconfig(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(mock_installer_t) :: mock
        type(installer_t) :: installer

        call new_installer(installer, prefix="PREFIX", verbosity=0, copy="mock")
        mock%installer_t = installer
        mock%os = OS_WINDOWS
        mock%expected_dir = "PREFIX\lib\pkgconfig"
        mock%expected_run = 'mock "name" "'//mock%expected_dir//'"'

        call mock%install("name", "lib/pkgconfig", error)

    end subroutine test_install_pkgconfig

    subroutine test_install_sitepackages(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(mock_installer_t) :: mock
        type(installer_t) :: installer

        call new_installer(installer, prefix="PREFIX", verbosity=0, copy="mock")
        mock%installer_t = installer
        mock%os = OS_LINUX
        mock%expected_dir = "PREFIX/lib/python3.7/site-packages"
        mock%expected_run = 'mock "name" "'//mock%expected_dir//'"'

        call mock%install("name", join_path("lib", "python3.7", "site-packages"), &
            error)

    end subroutine test_install_sitepackages

    subroutine test_install_mod(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(mock_installer_t) :: mock
        type(installer_t) :: installer

        call new_installer(installer, prefix="PREFIX", verbosity=0, copy="mock")
        mock%installer_t = installer
        mock%expected_dir = join_path("PREFIX", "include")
        mock%expected_run = 'mock "name" "'//join_path("PREFIX", "include")//'"'

        call mock%install_header("name", error)

    end subroutine test_install_mod

    !> Create a new directory in the prefix
    subroutine make_dir(self, dir, error)
        !> Instance of the installer
        class(mock_installer_t), intent(inout) :: self
        !> Directory to be created
        character(len=*), intent(in) :: dir
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check_string(error, self%expected_dir, dir, "dir")

    end subroutine make_dir

    !> Run an installation command
    subroutine run(self, command, error)
        !> Instance of the installer
        class(mock_installer_t), intent(inout) :: self
        !> Command to be launched
        character(len=*), intent(in) :: command
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check_string(error, self%expected_run, command, "run")
    end subroutine run

end module test_installer
