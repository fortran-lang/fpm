module fpm_downloader
  use fpm_error, only: error_t, fatal_error
  use fpm_filesystem, only: which

  implicit none
  private

  public :: downloader_t

  !> This type could be entirely avoided but it is quite practical because it can be mocked for testing.
  type downloader_t
  contains
    procedure, nopass :: get, unpack
  end type

contains

  !> Perform an http get request and save output to file.
  subroutine get(url, tmp_file, error)
    character(*), intent(in) :: url
    character(*), intent(in) :: tmp_file
    type(error_t), allocatable, intent(out) :: error

    integer :: stat

    if (which('curl') /= '') then
      print *, "Downloading package data from '"//url//"' ..."
      call execute_command_line('curl '//url//' -s -o '//tmp_file, exitstat=stat)
    else if (which('wget') /= '') then
      print *, "Downloading package data from '"//url//"' ..."
      call execute_command_line('wget '//url//' -q -O '//tmp_file, exitstat=stat)
    else
      call fatal_error(error, "Neither 'curl' nor 'wget' installed."); return
    end if

    if (stat /= 0) then
      call fatal_error(error, "Error downloading package from '"//url//"'."); return
    end if
  end

  subroutine unpack(tmp_file, destination, error)
    character(*), intent(in) :: tmp_file
    character(*), intent(in) :: destination
    type(error_t), allocatable, intent(out) :: error

    integer :: stat

    if (which('tar') == '') then
      call fatal_error(error, "'tar' not installed."); return
    end if

    call execute_command_line('tar -zxf '//tmp_file//' -C '//destination, exitstat=stat)

    if (stat /= 0) then
      call fatal_error(error, "Error unpacking '"//tmp_file//"'."); return
    end if
  end
end
