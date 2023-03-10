module fpm_downloader
  use fpm_error, only: error_t, fatal_error
  use fpm_filesystem, only: which
  use fpm_versioning, only: version_t
  use jonquil, only: json_object, json_value, json_error, json_load, cast_to_object

  implicit none
  private

  public :: downloader_t

  !> This type could be entirely avoided but it is quite practical because it can be mocked for testing.
  type downloader_t
  contains
    procedure, nopass :: get_pkg_data, get_file, unpack
  end type

contains

  !> Perform an http get request and save output to file.
  subroutine get_pkg_data(url, version, tmp_file, json, error)
    character(*), intent(in) :: url
    type(version_t), allocatable, intent(in) :: version
    character(*), intent(in) :: tmp_file
    type(json_object), intent(out) :: json
    type(error_t), allocatable, intent(out) :: error

    class(json_value), allocatable :: j_value
    type(json_object), pointer :: ptr
    type(json_error), allocatable :: j_error

    if (allocated(version)) then
      ! Request specific version.
      call get_file(url//'/'//version%s(), tmp_file, error)
    else
      ! Request latest version.
      call get_file(url, tmp_file, error)
    end if
    if (allocated(error)) return

    call json_load(j_value, tmp_file, error=j_error)
    if (allocated(j_error)) then
      allocate (error); call move_alloc(j_error%message, error%message); call json%destroy(); return
    end if

    ptr => cast_to_object(j_value)
    if (.not. associated(ptr)) then
      call fatal_error(error, "Error parsing JSON from '"//url//"'."); return
    end if

    json = ptr
  end

  subroutine get_file(url, tmp_file, error)
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

    print *, "Unpacking '"//tmp_file//"' to '"//destination//"' ..."
    call execute_command_line('tar -zxf '//tmp_file//' -C '//destination, exitstat=stat)

    if (stat /= 0) then
      call fatal_error(error, "Error unpacking '"//tmp_file//"'."); return
    end if
  end
end
