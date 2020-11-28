module fpm_installer
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm_environment, only : get_os_type, os_is_unix
  use fpm_error, only : error_t, fatal_error
  use fpm_filesystem, only : join_path, mkdir, exists
  implicit none
  private

  public :: installer_t, new_installer


  type :: installer_t
    character(len=:), allocatable :: prefix
    character(len=:), allocatable :: bindir
    character(len=:), allocatable :: libdir
    character(len=:), allocatable :: includedir
    integer :: unit = output_unit
    integer :: verbosity = 1
    character(len=:), allocatable :: copy
    !> Cached operating system
    integer :: os
  contains
    procedure :: install_executable
    procedure :: install_library
    procedure :: install_header
    procedure :: install_source
    procedure :: install
    procedure :: run
    procedure :: make_dir
  end type installer_t

  character(len=*), parameter :: default_bindir = "bin"
  character(len=*), parameter :: default_libdir = "lib"
  character(len=*), parameter :: default_includedir = "include"
  character(len=*), parameter :: default_prefix_unix = "/usr/local/bin"
  character(len=*), parameter :: default_prefix_win = "C:\"
  character(len=*), parameter :: default_copy_unix = "cp -v"
  character(len=*), parameter :: default_copy_win = "copy"

contains

   subroutine new_installer(self, prefix, bindir, libdir, includedir, verbosity)
     type(installer_t), intent(out) :: self
     character(len=*), intent(in), optional :: prefix
     character(len=*), intent(in), optional :: bindir
     character(len=*), intent(in), optional :: libdir
     character(len=*), intent(in), optional :: includedir
     integer, intent(in), optional :: verbosity

     self%os = get_os_type()

     if (os_is_unix(self%os)) then
       self%copy = default_copy_unix
     else
       self%copy = default_copy_win
     end if

     if (present(includedir)) then
       self%includedir = includedir
     else
       self%includedir = default_includedir
     end if

     if (present(prefix)) then
       self%prefix = prefix
     else
       if (os_is_unix(self%os)) then
         self%prefix = default_prefix_unix
       else
         self%prefix = default_prefix_win
       end if
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

   subroutine install_executable(self, executable, error)
     class(installer_t), intent(inout) :: self
     character(len=*), intent(in) :: executable
     type(error_t), allocatable, intent(out) :: error

     call self%install(executable, self%bindir, error)
   end subroutine install_executable

   subroutine install_library(self, library, error)
     class(installer_t), intent(inout) :: self
     character(len=*), intent(in) :: library
     type(error_t), allocatable, intent(out) :: error

     call self%install(library, self%libdir, error)
   end subroutine install_library

   subroutine install_header(self, header, error)
     class(installer_t), intent(inout) :: self
     character(len=*), intent(in) :: header
     type(error_t), allocatable, intent(out) :: error

     call self%install(header, self%includedir, error)
   end subroutine install_header

   subroutine install_source(self, source, error)
     class(installer_t), intent(inout) :: self
     character(len=*), intent(in) :: source
     type(error_t), allocatable, intent(out) :: error
   end subroutine install_source

   subroutine install(self, source, destination, error)
     class(installer_t), intent(inout) :: self
     character(len=*), intent(in) :: source
     character(len=*), intent(in) :: destination
     type(error_t), allocatable, intent(out) :: error

     character(len=:), allocatable :: install_dest

     install_dest = join_path(self%prefix, destination)
     call self%make_dir(install_dest, error)
     if (allocated(error)) return

     if (os_is_unix(self%os)) then
       call self%run(self%copy//" "//source//" "//install_dest, error)
     else
       call self%run(self%copy//" "//source//" "//install_dest, error)
     end if
     if (allocated(error)) return

   end subroutine install

   subroutine make_dir(self, dir, error)
     class(installer_t), intent(inout) :: self
     character(len=*), intent(in) :: dir
     type(error_t), allocatable, intent(out) :: error
     if (.not.exists(dir)) call mkdir(dir)
   end subroutine make_dir

   subroutine run(self, command, error)
     class(installer_t), intent(inout) :: self
     character(len=*), intent(in) :: command
     type(error_t), allocatable, intent(out) :: error
     integer :: stat

     call execute_command_line(command, exitstat=stat)

     if (stat /= 0) then
       call fatal_error(error, "Failed in command: '"//command//"'")
       return
     end if
   end subroutine run

end module fpm_installer
