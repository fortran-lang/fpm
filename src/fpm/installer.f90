!> Implementation of an installer object.
!>
!> The installer provides a way to install objects to their respective directories
!> in the installation prefix, a generic install command allows to install
!> to any directory within the prefix.
module fpm_installer
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm_environment, only : get_os_type, os_is_unix
  use fpm_error, only : error_t, fatal_error
  use fpm_filesystem, only : join_path, mkdir, exists, unix_path, windows_path, get_local_prefix

  implicit none
  private
  public :: installer_t, new_installer

  !> Declaration of the installer type
  type :: installer_t
    !> Path to installation directory
    character(len=:), allocatable :: prefix
    !> Binary dir relative to the installation prefix
    character(len=:), allocatable :: bindir
    !> Library directory relative to the installation prefix
    character(len=:), allocatable :: libdir
    !> Test program directory relative to the installation prefix
    character(len=:), allocatable :: testdir
    !> Include directory relative to the installation prefix
    character(len=:), allocatable :: includedir
    !> Output unit for informative printout
    integer :: unit = output_unit
    !> Verbosity of the installer
    integer :: verbosity = 1
    !> Command to copy objects into the installation prefix
    character(len=:), allocatable :: copy
    !> Command to move objects into the installation prefix
    character(len=:), allocatable :: move
    !> Cached operating system
    integer :: os
  contains
    !> Install an executable in its correct subdirectory
    procedure :: install_executable
    !> Install a library in its correct subdirectory
    procedure :: install_library
    !> Install a header/module in its correct subdirectory
    procedure :: install_header
    !> Install a test program in its correct subdirectory
    procedure :: install_test
    !> Install a generic file into a subdirectory in the installation prefix
    procedure :: install
    !> Run an installation command, type-bound for unit testing purposes
    procedure :: run
    !> Create a new directory in the prefix, type-bound for unit testing purposes
    procedure :: make_dir
  end type installer_t

  !> Default name of the binary subdirectory
  character(len=*), parameter :: default_bindir = "bin"

  !> Default name of the library subdirectory
  character(len=*), parameter :: default_libdir = "lib"
  
  !> Default name of the test subdirectory
  character(len=*), parameter :: default_testdir = "test"

  !> Default name of the include subdirectory
  character(len=*), parameter :: default_includedir = "include"

  !> Copy command on Unix platforms
  character(len=*), parameter :: default_copy_unix = "cp"

  !> Copy command on Windows platforms
  character(len=*), parameter :: default_copy_win = "copy"

  !> Copy command on Unix platforms
  character(len=*), parameter :: default_force_copy_unix = "cp -f"

  !> Copy command on Windows platforms
  character(len=*), parameter :: default_force_copy_win = "copy /Y"

  !> Move command on Unix platforms
  character(len=*), parameter :: default_move_unix = "mv"

  !> Move command on Windows platforms
  character(len=*), parameter :: default_move_win = "move"

contains

  !> Create a new instance of an installer
  subroutine new_installer(self, prefix, bindir, libdir, includedir, testdir, verbosity, &
          copy, move)
    !> Instance of the installer
    type(installer_t), intent(out) :: self
    !> Path to installation directory
    character(len=*), intent(in), optional :: prefix
    !> Binary dir relative to the installation prefix
    character(len=*), intent(in), optional :: bindir
    !> Library directory relative to the installation prefix
    character(len=*), intent(in), optional :: libdir
    !> Include directory relative to the installation prefix
    character(len=*), intent(in), optional :: includedir
    !> Test directory relative to the installation prefix
    character(len=*), intent(in), optional :: testdir    
    !> Verbosity of the installer
    integer, intent(in), optional :: verbosity
    !> Copy command
    character(len=*), intent(in), optional :: copy
    !> Move command
    character(len=*), intent(in), optional :: move

    self%os = get_os_type()

    ! By default, never prompt the user for overwrites
    if (present(copy)) then
      self%copy = copy
    else
      if (os_is_unix(self%os)) then
        self%copy = default_force_copy_unix
      else
        self%copy = default_force_copy_win
      end if
    end if

    if (present(move)) then
      self%move = move
    else
      if (os_is_unix(self%os)) then
        self%move = default_move_unix
      else
        self%move = default_move_win
      end if
    end if

    if (present(includedir)) then
      self%includedir = includedir
    else
      self%includedir = default_includedir
    end if
    
    if (present(testdir)) then 
      self%testdir = testdir
    else
      self%testdir = default_testdir  
    end if

    if (present(prefix)) then
      self%prefix = prefix
    else
      self%prefix = get_local_prefix(self%os)
    end if

    if (present(bindir)) then
      self%bindir = bindir
    else
      self%bindir = default_bindir
    end if

    if (present(libdir)) then
      self%libdir = libdir
    else
      self%libdir = default_libdir
    end if

    if (present(verbosity)) then
      self%verbosity = verbosity
    else
      self%verbosity = 1
    end if

  end subroutine new_installer

  !> Install an executable in its correct subdirectory
  subroutine install_executable(self, executable, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the executable
    character(len=*), intent(in) :: executable
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    integer :: ll

    if (.not.os_is_unix(self%os)) then
        ll = len(executable)
        if (executable(max(1, ll-3):ll) /= ".exe") then
            call self%install(executable//".exe", self%bindir, error)
            return
        end if
    end if

    call self%install(executable, self%bindir, error)

  end subroutine install_executable

  !> Install a library in its correct subdirectory
  subroutine install_library(self, library, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the library
    character(len=*), intent(in) :: library
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call self%install(library, self%libdir, error)
  end subroutine install_library

  !> Install a test program in its correct subdirectory
  subroutine install_test(self, test, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the test executable
    character(len=*), intent(in) :: test
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    integer :: ll

    if (.not.os_is_unix(self%os)) then
        ll = len(test)
        if (test(max(1, ll-3):ll) /= ".exe") then
            call self%install(test//".exe", self%testdir, error)
            return
        end if
    end if

    call self%install(test, self%testdir, error)

  end subroutine install_test

  !> Install a header/module in its correct subdirectory
  subroutine install_header(self, header, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the header
    character(len=*), intent(in) :: header
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call self%install(header, self%includedir, error)
  end subroutine install_header

  !> Install a generic file into a subdirectory in the installation prefix
  subroutine install(self, source, destination, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the original file
    character(len=*), intent(in) :: source
    !> Path to the destination inside the prefix
    character(len=*), intent(in) :: destination
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: install_dest

    install_dest = join_path(self%prefix, destination)
    if (os_is_unix(self%os)) then
      install_dest = unix_path(install_dest)
    else
      install_dest = windows_path(install_dest)
    end if
    call self%make_dir(install_dest, error)
    if (allocated(error)) return

    if (self%verbosity > 0) then
      if (exists(install_dest)) then
        write(self%unit, '("# Update:", 1x, a, 1x, "->", 1x, a)') &
          source, install_dest
      else
        write(self%unit, '("# Install:", 1x, a, 1x, "->", 1x, a)') &
          source, install_dest
      end if
    end if

    ! Use force-copy to never prompt the user for overwrite if a package was already installed
    call self%run(self%copy//' "'//source//'" "'//install_dest//'"', error)

    if (allocated(error)) return

  end subroutine install

  !> Create a new directory in the prefix
  subroutine make_dir(self, dir, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Directory to be created
    character(len=*), intent(in) :: dir
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    if (.not.exists(dir)) then
       if (self%verbosity > 1) then
          write(self%unit, '("# Dir:", 1x, a)') dir
       end if
       call mkdir(dir)
    end if
  end subroutine make_dir

  !> Run an installation command
  subroutine run(self, command, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Command to be launched
    character(len=*), intent(in) :: command
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    integer :: stat

    if (self%verbosity > 1) then
      write(self%unit, '("# Run:", 1x, a)') command
    end if
    call execute_command_line(command, exitstat=stat)

    if (stat /= 0) then
      call fatal_error(error, "Failed in command: '"//command//"'")
      return
    end if
  end subroutine run

end module fpm_installer
