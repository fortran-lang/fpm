!> This module contains general routines for interacting with the file system
!!
module fpm_filesystem
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
    use fpm_environment, only: get_os_type, &
                               OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                               OS_CYGWIN, OS_SOLARIS, OS_FREEBSD
    use fpm_environment, only: separator, get_env
    use fpm_strings, only: f_string, replace, string_t, split
    implicit none
    private
    public :: basename, canon_path, dirname, is_dir, join_path, number_of_rows, read_lines, list_files, env_variable, &
            mkdir, exists, get_temp_filename, windows_path, unix_path, getline, delete_file, to_fortran_name
    public :: which
    public :: fileopen, fileclose, filewrite, warnwrite

    integer, parameter :: LINE_BUFFER_LEN = 1000

contains


!> return value of environment variable
subroutine env_variable(var, name)
   character(len=:), allocatable, intent(out) :: var
   character(len=*), intent(in) :: name
   integer :: length, stat

   call get_environment_variable(name, length=length, status=stat)
   if (stat /= 0) return

   allocate(character(len=length) :: var)

   if (length > 0) then
      call get_environment_variable(name, var, status=stat)
      if (stat /= 0) then
         deallocate(var)
         return
      end if
   end if

end subroutine env_variable


!> Extract filename from path with/without suffix
function basename(path,suffix) result (base)
 
    character(*), intent(In) :: path
    logical, intent(in), optional :: suffix
    character(:), allocatable :: base

    character(:), allocatable :: file_parts(:)
    logical :: with_suffix

    if (.not.present(suffix)) then
        with_suffix = .true.
    else
        with_suffix = suffix
    end if

    if (with_suffix) then
        call split(path,file_parts,delimiters='\/')
        if(size(file_parts).gt.0)then
           base = trim(file_parts(size(file_parts)))
        else
           base = ''
        endif
    else
        call split(path,file_parts,delimiters='\/.')
        if(size(file_parts).ge.2)then
           base = trim(file_parts(size(file_parts)-1))
        else
           base = ''
        endif
    end if

end function basename


!> Canonicalize path for comparison
!! * Handles path string redundancies
!! * Does not test existence of path
!!
!! To be replaced by realpath/_fullname in stdlib_os
!!
function canon_path(path) result(canon)
    character(*), intent(in) :: path
    character(:), allocatable :: canon

    integer :: i, j
    integer :: iback
    character(len(path)) :: nixpath
    character(len(path)) :: temp

    nixpath = unix_path(path)

    j = 1
    do i=1,len(nixpath)

        ! Skip back to last directory for '/../'
        if (i > 4) then

            if (nixpath(i-3:i) == '/../') then

                iback = scan(nixpath(1:i-4),'/',back=.true.)
                if (iback > 0) then
                    j = iback + 1
                    cycle
                end if

            end if

        end if

        if (i > 1 .and. j > 1) then

            ! Ignore current directory reference
            if (nixpath(i-1:i) == './') then

                j = j - 1
                cycle

            end if

            ! Ignore repeated separators
            if (nixpath(i-1:i) == '//') then

                cycle

            end if

            ! Do NOT include trailing slash
            if (i == len(nixpath) .and. nixpath(i:i) == '/') then
                cycle
            end if

        end if


        temp(j:j) = nixpath(i:i)
        j = j + 1

    end do

    canon = temp(1:j-1)

end function canon_path


!> Extract dirname from path
function dirname(path) result (dir)
    character(*), intent(in) :: path
    character(:), allocatable :: dir

    dir = path(1:scan(path,'/\',back=.true.))

end function dirname


!> test if a name matches an existing directory path
logical function is_dir(dir)
    character(*), intent(in) :: dir
    integer :: stat

    select case (get_os_type())

    case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
        call execute_command_line("test -d " // dir , exitstat=stat)

    case (OS_WINDOWS)
        call execute_command_line('cmd /c "if not exist ' // windows_path(dir) // '\ exit /B 1"', exitstat=stat)

    end select

    is_dir = (stat == 0)

end function is_dir


!> Construct path by joining strings with os file separator
function join_path(a1,a2,a3,a4,a5) result(path)

    character(len=*), intent(in)           :: a1, a2
    character(len=*), intent(in), optional :: a3, a4, a5
    character(len=:), allocatable          :: path
    character(len=1)                       :: filesep

    select case (get_os_type())
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
            filesep = '/'
        case (OS_WINDOWS)
            filesep = '\'
    end select

    path = a1 // filesep // a2

    if (present(a3)) then
        path = path // filesep // a3
    else
        return
    end if

    if (present(a4)) then
        path = path // filesep // a4
    else
        return
    end if

    if (present(a5)) then
        path = path // filesep // a5
    else
        return
    end if

end function join_path


!> Determine number or rows in a file given a LUN
integer function number_of_rows(s) result(nrows)
    integer,intent(in)::s
    integer :: ios
    character(len=100) :: r
    rewind(s)
    nrows = 0
    do
        read(s, '(A)', iostat=ios) r
        if (ios /= 0) exit
        nrows = nrows + 1
    end do
    rewind(s)
end function number_of_rows


!> read lines into an array of TYPE(STRING_T) variables
function read_lines(fh) result(lines)
    integer, intent(in) :: fh
    type(string_t), allocatable :: lines(:)

    integer :: i
    character(LINE_BUFFER_LEN) :: line_buffer

    allocate(lines(number_of_rows(fh)))
    do i = 1, size(lines)
        read(fh, '(A)') line_buffer
        lines(i)%s = trim(line_buffer)
    end do

end function read_lines

!> Create a directory. Create subdirectories as needed
subroutine mkdir(dir)
    character(len=*), intent(in) :: dir
    integer                      :: stat

    if (is_dir(dir)) return

    select case (get_os_type())
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
            call execute_command_line('mkdir -p ' // dir, exitstat=stat)
            write (*, '(" + ",2a)') 'mkdir -p ' // dir

        case (OS_WINDOWS)
            call execute_command_line("mkdir " // windows_path(dir), exitstat=stat)
            write (*, '(" + ",2a)') 'mkdir ' // windows_path(dir)
    end select

    if (stat /= 0) then
        print *, 'execute_command_line() failed'
        error stop
    end if
end subroutine mkdir


!> Get file & directory names in directory `dir`.
!!
!!  - File/directory names return are relative to cwd, ie. preprended with `dir`
!!  - Includes files starting with `.` except current directory and parent directory
!!
recursive subroutine list_files(dir, files, recurse)
    character(len=*), intent(in) :: dir
    type(string_t), allocatable, intent(out) :: files(:)
    logical, intent(in), optional :: recurse

    integer :: stat, fh, i
    character(:), allocatable :: temp_file
    type(string_t), allocatable :: dir_files(:)
    type(string_t), allocatable :: sub_dir_files(:)

    if (.not. is_dir(dir)) then
        allocate (files(0))
        return
    end if

    allocate (temp_file, source=get_temp_filename())

    select case (get_os_type())
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
            call execute_command_line('ls -A ' // dir // ' > ' // temp_file, &
                                      exitstat=stat)
        case (OS_WINDOWS)
            call execute_command_line('dir /b ' // windows_path(dir) // ' > ' // temp_file, &
                                      exitstat=stat)
    end select

    if (stat /= 0) then
        print *, 'execute_command_line() failed'
        error stop
    end if

    open (newunit=fh, file=temp_file, status='old')
    files = read_lines(fh)
    close(fh,status="delete")

    do i=1,size(files)
        files(i)%s = join_path(dir,files(i)%s)
    end do

    if (present(recurse)) then
        if (recurse) then

            allocate(sub_dir_files(0))

            do i=1,size(files)
                if (is_dir(files(i)%s)) then

                    call list_files(files(i)%s, dir_files, recurse=.true.)
                    sub_dir_files = [sub_dir_files, dir_files]

                end if
            end do

            files = [files, sub_dir_files]

        end if
    end if

end subroutine list_files


!> test if pathname already exists
logical function exists(filename) result(r)
    character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function


!> Get a unused temporary filename
!!  Calls posix 'tempnam' - not recommended, but
!!   we have no security concerns for this application
!!   and use here is temporary.
!! Works with MinGW
function get_temp_filename() result(tempfile)
    !
    use iso_c_binding, only: c_ptr, C_NULL_PTR, c_f_pointer
    character(:), allocatable :: tempfile

    type(c_ptr) :: c_tempfile_ptr
    character(len=1), pointer :: c_tempfile(:)

    interface

        function c_tempnam(dir,pfx) result(tmp) bind(c,name="tempnam")
            import
            type(c_ptr), intent(in), value :: dir
            type(c_ptr), intent(in), value :: pfx
            type(c_ptr) :: tmp
        end function c_tempnam

        subroutine c_free(ptr) BIND(C,name="free")
            import
            type(c_ptr), value :: ptr
        end subroutine c_free

    end interface

    c_tempfile_ptr = c_tempnam(C_NULL_PTR, C_NULL_PTR)
    call c_f_pointer(c_tempfile_ptr,c_tempfile,[LINE_BUFFER_LEN])

    tempfile = f_string(c_tempfile)

    call c_free(c_tempfile_ptr)

end function get_temp_filename


!> Replace file system separators for windows
function windows_path(path) result(winpath)

    character(*), intent(in) :: path
    character(:), allocatable :: winpath

    integer :: idx

    winpath = path

    idx = index(winpath,'/')
    do while(idx > 0)
        winpath(idx:idx) = '\'
        idx = index(winpath,'/')
    end do

end function windows_path


!> Replace file system separators for unix
function unix_path(path) result(nixpath)

    character(*), intent(in) :: path
    character(:), allocatable :: nixpath

    integer :: idx

    nixpath = path

    idx = index(nixpath,'\')
    do while(idx > 0)
        nixpath(idx:idx) = '/'
        idx = index(nixpath,'\')
    end do

end function unix_path


!> read a line of arbitrary length into a CHARACTER variable from the specified LUN
subroutine getline(unit, line, iostat, iomsg)

    !> Formatted IO unit
    integer, intent(in) :: unit

    !> Line to read
    character(len=:), allocatable, intent(out) :: line

    !> Status of operation
    integer, intent(out) :: iostat

    !> Error message
    character(len=:), allocatable, optional :: iomsg

    character(len=LINE_BUFFER_LEN) :: buffer
    character(len=LINE_BUFFER_LEN) :: msg
    integer :: size
    integer :: stat

    allocate(character(len=0) :: line)
    do
        read(unit, '(a)', advance='no', iostat=stat, iomsg=msg, size=size) &
            & buffer
        if (stat > 0) exit
        line = line // buffer(:size)
        if (stat < 0) then
            if (is_iostat_eor(stat)) then
                stat = 0
            end if
            exit
        end if
    end do

    if (stat /= 0) then
        if (present(iomsg)) iomsg = trim(msg)
    end if
    iostat = stat

end subroutine getline


!> delete a file by filename
subroutine delete_file(file)
    character(len=*), intent(in) :: file
    logical :: exist
    integer :: unit
    inquire(file=file, exist=exist)
    if (exist) then
        open(file=file, newunit=unit)
        close(unit, status="delete")
    end if
end subroutine delete_file

!> write trimmed character data to a file if it does not exist
subroutine warnwrite(fname,data)
character(len=*),intent(in) :: fname
character(len=*),intent(in) :: data(:)

    if(.not.exists(fname))then
        call filewrite(fname,data)
    else
        write(stderr,'(*(g0,1x))')'<INFO>  ',fname,&
        & 'already exists. Not overwriting'
    endif

end subroutine warnwrite

!> procedure to open filename as a sequential "text" file
subroutine fileopen(filename,lun,ier)

character(len=*),intent(in)   :: filename
integer,intent(out)           :: lun
integer,intent(out),optional  :: ier
integer                       :: ios
character(len=256)            :: message

    message=' '
    ios=0
    if(filename.ne.' ')then
        open(file=filename, &
        & newunit=lun, &
        & form='formatted', &    ! FORM    = FORMATTED | UNFORMATTED
        & access='sequential', & ! ACCESS  = SEQUENTIAL| DIRECT | STREAM
        & action='write', &      ! ACTION  = READ|WRITE| READWRITE
        & position='rewind', &   ! POSITION= ASIS      | REWIND | APPEND
        & status='new', &        ! STATUS  = NEW| REPLACE| OLD| SCRATCH| UNKNOWN
        & iostat=ios, &
        & iomsg=message)
    else
        lun=stdout
        ios=0
    endif
    if(ios.ne.0)then
        write(stderr,'(*(a:,1x))')&
        & '<ERROR> *filewrite*:',filename,trim(message)
        lun=-1
        if(present(ier))then
           ier=ios
        else
           stop 1
        endif
    endif

end subroutine fileopen

!> simple close of a LUN.  On error show message and stop (by default)
subroutine fileclose(lun,ier)
integer,intent(in)    :: lun
integer,intent(out),optional :: ier
character(len=256)    :: message
integer               :: ios
    if(lun.ne.-1)then
        close(unit=lun,iostat=ios,iomsg=message)
        if(ios.ne.0)then
            write(stderr,'(*(a:,1x))')'<ERROR> *filewrite*:',trim(message)
            if(present(ier))then
               ier=ios
            else
               stop 2
            endif
        endif
    endif
end subroutine fileclose

!> procedure to write filedata to file filename
subroutine filewrite(filename,filedata)

character(len=*),intent(in)           :: filename
character(len=*),intent(in)           :: filedata(:)
integer                               :: lun, i, ios
character(len=256)                    :: message
    call fileopen(filename,lun)
    if(lun.ne.-1)then ! program currently stops on error on open, but might
                      ! want it to continue so -1 (unallowed LUN) indicates error
       ! write file
       do i=1,size(filedata)
           write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
           if(ios.ne.0)then
               write(stderr,'(*(a:,1x))')&
               & '<ERROR> *filewrite*:',filename,trim(message)
               stop 4
           endif
       enddo
    endif
    ! close file
    call fileclose(lun)

end subroutine filewrite

!> Returns string with special characters replaced with an underscore.
!! For now, only a hyphen is treated as a special character, but this can be
!! expanded to other characters if needed.
pure function to_fortran_name(string) result(res)
    character(*), intent(in) :: string
    character(len(string)) :: res
    character, parameter :: SPECIAL_CHARACTERS(*) = ['-']
    res = replace(string, SPECIAL_CHARACTERS, '_')
end function to_fortran_name

function which(command) result(pathname)
!>
!!##NAME
!!     which(3f) - [M_io:ENVIRONMENT] given a command name find the pathname by searching
!!                 the directories in the environment variable $PATH
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function which(command) result(pathname)
!!
!!    character(len=*),intent(in)  :: command
!!    character(len=:),allocatable :: pathname
!!
!!##DESCRIPTION
!!    Given a command name find the first file with that name in the directories
!!    specified by the environment variable $PATH.
!!
!!##OPTIONS
!!    COMMAND   the command to search for
!!
!!##RETURNS
!!    PATHNAME  the first pathname found in the current user path. Returns blank
!!              if the command is not found.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!   Checking the error message and counting lines:
!!
!!     program demo_which
!!     use M_io, only : which
!!     implicit none
!!        write(*,*)'ls is ',which('ls')
!!        write(*,*)'dir is ',which('dir')
!!        write(*,*)'install is ',which('install')
!!     end program demo_which
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

character(len=*),intent(in)     :: command
character(len=:),allocatable    :: pathname, checkon, paths(:), exts(:)
integer                         :: i, j
   pathname=''
   call split(get_env('PATH'),paths,delimiters=merge(';',':',separator().eq.'\'))
   SEARCH: do i=1,size(paths)
      checkon=trim(join_path(trim(paths(i)),command))
      select case(separator())
      case('/')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
      case('\')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
         if(exists(checkon//'.bat'))then
            pathname=checkon//'.bat'
            exit SEARCH
         endif
         if(exists(checkon//'.exe'))then
            pathname=checkon//'.exe'
            exit SEARCH
         endif
         call split(get_env('PATHEXT'),exts,delimiters=';')
         do j=1,size(exts)
            if(exists(checkon//'.'//trim(exts(j))))then
               pathname=checkon//'.'//trim(exts(j))
               exit SEARCH
            endif
         enddo
      end select
   enddo SEARCH
end function which

end module fpm_filesystem
