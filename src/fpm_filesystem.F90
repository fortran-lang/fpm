!> This module contains general routines for interacting with the file system
!!
module fpm_filesystem
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
    use fpm_environment, only: get_os_type, &
                               OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                               OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
    use fpm_environment, only: separator, get_env, os_is_unix
    use fpm_strings, only: f_string, replace, string_t, split, notabs, str_begins_with_str
    use iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_associated, c_f_pointer
    use fpm_error, only : fpm_stop, error_t, fatal_error
    implicit none
    private
    public :: basename, canon_path, dirname, is_dir, join_path, number_of_rows, list_files, get_local_prefix, &
            mkdir, exists, get_temp_filename, windows_path, unix_path, getline, delete_file, fileopen, fileclose, &
            filewrite, warnwrite, parent_dir, is_hidden_file, read_lines, read_lines_expanded, which, run, &
            LINE_BUFFER_LEN, os_delete_dir, is_absolute_path, env_variable, get_home, get_tmp_directory, &
            execute_and_read_output
    integer, parameter :: LINE_BUFFER_LEN = 1000

#ifndef FPM_BOOTSTRAP
    interface
        function c_opendir(dir) result(r) bind(c, name="c_opendir")
            import c_char, c_ptr
            character(kind=c_char), intent(in) :: dir(*)
            type(c_ptr) :: r
        end function c_opendir

        function c_readdir(dir) result(r) bind(c, name="c_readdir")
            import c_ptr
            type(c_ptr), intent(in), value :: dir
            type(c_ptr) :: r
        end function c_readdir

        function c_closedir(dir) result(r) bind(c, name="closedir")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: dir
            integer(kind=c_int) :: r
        end function c_closedir

        function c_get_d_name(dir) result(r) bind(c, name="get_d_name")
            import c_ptr
            type(c_ptr), intent(in), value :: dir
            type(c_ptr) :: r
        end function c_get_d_name

        function c_is_dir(path) result(r) bind(c, name="c_is_dir")
            import c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(kind=c_int) :: r
        end function c_is_dir
    end interface
#endif

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

    call split(path,file_parts,delimiters='\/')
    if(size(file_parts)>0)then
       base = trim(file_parts(size(file_parts)))
    else
       base = ''
    endif
    if(.not.with_suffix)then
        call split(base,file_parts,delimiters='.')
        if(size(file_parts)>=2)then
           base = trim(file_parts(size(file_parts)-1))
        endif
    endif

end function basename


!> Canonicalize path for comparison
!! * Handles path string redundancies
!! * Does not test existence of path
!!
!! To be replaced by realpath/_fullname in stdlib_os
!!
!! FIXME: Lot's of ugly hacks following here
function canon_path(path)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: canon_path
    character(len=:), allocatable :: nixpath

    integer :: istart, iend, nn, last
    logical :: is_path, absolute

    nixpath = unix_path(path)

    istart = 0
    nn = 0
    iend = 0
    absolute = nixpath(1:1) == "/"
    if (absolute) then
        canon_path = "/"
    else
        canon_path = ""
    end if

    do while(iend < len(nixpath))
        call next(nixpath, istart, iend, is_path)
        if (is_path) then
            select case(nixpath(istart:iend))
            case(".", "") ! always drop empty paths
            case("..")
                if (nn > 0) then
                    last = scan(canon_path(:len(canon_path)-1), "/", back=.true.)
                    canon_path = canon_path(:last)
                    nn = nn - 1
                else
                    if (.not. absolute) then
                        canon_path = canon_path // nixpath(istart:iend) // "/"
                    end if
                end if
            case default
                nn = nn + 1
                canon_path = canon_path // nixpath(istart:iend) // "/"
            end select
        end if
    end do

    if (len(canon_path) == 0) canon_path = "."
    if (len(canon_path) > 1 .and. canon_path(len(canon_path):) == "/") then
        canon_path = canon_path(:len(canon_path)-1)
    end if

contains

    subroutine next(string, istart, iend, is_path)
        character(len=*), intent(in) :: string
        integer, intent(inout) :: istart
        integer, intent(inout) :: iend
        logical, intent(inout) :: is_path

        integer :: ii, nn
        character :: tok

        nn = len(string)

        if (iend >= nn) then
            istart = nn
            iend = nn
            return
        end if

        ii = min(iend + 1, nn)
        tok = string(ii:ii)

        is_path = tok /= '/'

        if (.not.is_path) then
            is_path = .false.
            istart = ii
            iend = ii
            return
        end if

        istart = ii
        do ii = min(iend + 1, nn), nn
            tok = string(ii:ii)
            select case(tok)
            case('/')
                exit
            case default
                iend = ii
                cycle
            end select
        end do

    end subroutine next
end function canon_path


!> Extract dirname from path
function dirname(path) result (dir)
    character(*), intent(in) :: path
    character(:), allocatable :: dir

    dir = path(1:scan(path,'/\',back=.true.))

end function dirname

!> Extract dirname from path
function parent_dir(path) result (dir)
    character(*), intent(in) :: path
    character(:), allocatable :: dir

    dir = path(1:scan(path,'/\',back=.true.)-1)

end function parent_dir


!> test if a name matches an existing directory path
logical function is_dir(dir)
    character(*), intent(in) :: dir
    integer :: stat

    select case (get_os_type())

    case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
        call run( "test -d " // dir , &
                & exitstat=stat,echo=.false.,verbose=.false.)

    case (OS_WINDOWS)
        call run('cmd /c "if not exist ' // windows_path(dir) // '\ exit /B 1"', &
                & exitstat=stat,echo=.false.,verbose=.false.)

    end select

    is_dir = (stat == 0)

end function is_dir

!> test if a file is hidden
logical function is_hidden_file(file_basename) result(r)
    character(*), intent(in) :: file_basename
    if (len(file_basename) <= 2) then
        r = .false.
    else
        r = str_begins_with_str(file_basename, '.')
    end if
end function is_hidden_file

!> Construct path by joining strings with os file separator
function join_path(a1,a2,a3,a4,a5) result(path)

    character(len=*), intent(in)           :: a1, a2
    character(len=*), intent(in), optional :: a3, a4, a5
    character(len=:), allocatable          :: path
    character(len=1)                       :: filesep
    logical, save                          :: has_cache = .false.
    character(len=1), save                 :: cache = '/'
    !$omp threadprivate(has_cache, cache)

    if (has_cache) then
        filesep = cache
    else
        select case (get_os_type())
            case default
                filesep = '/'
            case (OS_WINDOWS)
                filesep = '\'
        end select

        cache = filesep
        has_cache = .true.
    end if

    if (a1 == "") then
        path = a2
    else
        path = a1 // filesep // a2
    end if

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
    rewind(s)
    nrows = 0
    do
        read(s, *, iostat=ios)
        if (ios /= 0) exit
        nrows = nrows + 1
    end do
    rewind(s)
end function number_of_rows

!> read lines into an array of TYPE(STRING_T) variables expanding tabs
function read_lines_expanded(fh) result(lines)
    integer, intent(in) :: fh
    type(string_t), allocatable :: lines(:)

    integer :: i
    integer :: ilen
    character(LINE_BUFFER_LEN) :: line_buffer_read, line_buffer_expanded

    allocate(lines(number_of_rows(fh)))
    do i = 1, size(lines)
        read(fh, '(A)') line_buffer_read
        call notabs(line_buffer_read, line_buffer_expanded, ilen)
        lines(i)%s = trim(line_buffer_expanded)
    end do

end function read_lines_expanded

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
subroutine mkdir(dir, echo)
    character(len=*), intent(in) :: dir
    logical, intent(in), optional :: echo

    integer :: stat

    if (is_dir(dir)) return

    select case (get_os_type())
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
            call run('mkdir -p ' // dir, exitstat=stat,echo=echo,verbose=.false.)

        case (OS_WINDOWS)
            call run("mkdir " // windows_path(dir), &
                    & echo=echo, exitstat=stat,verbose=.false.)

    end select

    if (stat /= 0) then
        call fpm_stop(1, '*mkdir*:directory creation failed')
    end if
end subroutine mkdir

#ifndef FPM_BOOTSTRAP
!> Get file & directory names in directory `dir` using iso_c_binding.
!!
!!  - File/directory names return are relative to cwd, ie. preprended with `dir`
!!  - Includes files starting with `.` except current directory and parent directory
!!
recursive subroutine list_files(dir, files, recurse)
    character(len=*), intent(in) :: dir
    type(string_t), allocatable, intent(out) :: files(:)
    logical, intent(in), optional :: recurse

    integer :: i
    type(string_t), allocatable :: dir_files(:)
    type(string_t), allocatable :: sub_dir_files(:)

    type(c_ptr) :: dir_handle
    type(c_ptr) :: dir_entry_c
    character(len=:,kind=c_char), allocatable :: fortran_name
    character(len=:), allocatable :: string_fortran
    integer, parameter :: N_MAX = 256
    type(string_t) :: files_tmp(N_MAX)
    integer(kind=c_int) :: r

    if (c_is_dir(dir(1:len_trim(dir))//c_null_char) == 0) then
        allocate (files(0))
        return
    end if

    dir_handle = c_opendir(dir(1:len_trim(dir))//c_null_char)
    if (.not. c_associated(dir_handle)) then
        print *, 'c_opendir() failed'
        error stop
    end if

    i = 0
    allocate(files(0))

    do
        dir_entry_c = c_readdir(dir_handle)
        if (.not. c_associated(dir_entry_c)) then
            exit
        else
            string_fortran = f_string(c_get_d_name(dir_entry_c))

            if ((string_fortran == '.' .or. string_fortran == '..')) then
                cycle
            end if

            i = i + 1

            if (i > N_MAX) then
                files = [files, files_tmp]
                i = 1
            end if

            files_tmp(i)%s = join_path(dir, string_fortran)
        end if
    end do

    r = c_closedir(dir_handle)

    if (r /= 0) then
        print *, 'c_closedir() failed'
        error stop
    end if

    if (i > 0) then
        files = [files, files_tmp(1:i)]
    end if

    if (present(recurse)) then
        if (recurse) then

            allocate(sub_dir_files(0))

            do i=1,size(files)
                if (c_is_dir(files(i)%s//c_null_char) /= 0) then
                    call list_files(files(i)%s, dir_files, recurse=.true.)
                    sub_dir_files = [sub_dir_files, dir_files]
                end if
            end do

            files = [files, sub_dir_files]
        end if
    end if
end subroutine list_files

#else
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
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
            call run('ls -A ' // dir , &
                    & redirect=temp_file, exitstat=stat,echo=.false.,verbose=.false.)
        case (OS_WINDOWS)
            call run('dir /b ' // windows_path(dir), &
                    & redirect=temp_file, exitstat=stat,echo=.false.,verbose=.false.)
    end select

    if (stat /= 0) then
        call fpm_stop(2,'*list_files*:directory listing failed')
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

#endif


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
    if(filename/=' ')then
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
    if(ios/=0)then
        lun=-1
        if(present(ier))then
           ier=ios
        else
           call fpm_stop(3,'*fileopen*:'//filename//':'//trim(message))
        endif
    endif

end subroutine fileopen

!> simple close of a LUN.  On error show message and stop (by default)
subroutine fileclose(lun,ier)
integer,intent(in)    :: lun
integer,intent(out),optional :: ier
character(len=256)    :: message
integer               :: ios
    if(lun/=-1)then
        close(unit=lun,iostat=ios,iomsg=message)
        if(ios/=0)then
            if(present(ier))then
               ier=ios
            else
               call fpm_stop(4,'*fileclose*:'//trim(message))
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
    if(lun/=-1)then ! program currently stops on error on open, but might
                      ! want it to continue so -1 (unallowed LUN) indicates error
       ! write file
       do i=1,size(filedata)
           write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
           if(ios/=0)then
               call fpm_stop(5,'*filewrite*:'//filename//':'//trim(message))
           endif
       enddo
    endif
    ! close file
    call fileclose(lun)

end subroutine filewrite

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
   call split(get_env('PATH'),paths,delimiters=merge(';',':',separator()=='\'))
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

!> echo command string and pass it to the system for execution
!call run(cmd,echo=.false.,exitstat=exitstat,verbose=.false.,redirect='')
subroutine run(cmd,echo,exitstat,verbose,redirect)
    character(len=*), intent(in) :: cmd
    logical,intent(in),optional  :: echo
    integer, intent(out),optional :: exitstat
    logical, intent(in), optional :: verbose
    character(*), intent(in), optional :: redirect

    integer            :: cmdstat
    character(len=256) :: cmdmsg, iomsg
    logical :: echo_local, verbose_local
    character(:), allocatable :: redirect_str
    character(:), allocatable :: line
    integer :: stat, fh, iostat

    if(present(echo))then
       echo_local=echo
    else
       echo_local=.true.
    end if

    if(present(verbose))then
        verbose_local=verbose
    else
        verbose_local=.true.
    end if

    if (present(redirect)) then
        if(redirect /= '')then
           redirect_str =  ">"//redirect//" 2>&1"
        endif
    else
        if(verbose_local)then
            ! No redirection but verbose output
            redirect_str = ""
        else
            ! No redirection and non-verbose output
            if (os_is_unix()) then
                redirect_str = " >/dev/null 2>&1"
            else
                redirect_str = " >NUL 2>&1"
            end if
        end if
    end if

    if(echo_local) print *, '+ ', cmd !//redirect_str

    call execute_command_line(cmd//redirect_str, exitstat=stat,cmdstat=cmdstat,cmdmsg=cmdmsg)
    if(cmdstat /= 0)then
        write(*,'(a)')'<ERROR>:failed command '//cmd//redirect_str
        call fpm_stop(1,'*run*:'//trim(cmdmsg))
    endif

    if (verbose_local.and.present(redirect)) then

        open(newunit=fh,file=redirect,status='old',iostat=iostat,iomsg=iomsg)
        if(iostat == 0)then
           do
               call getline(fh, line, iostat)
               if (iostat /= 0) exit
               write(*,'(A)') trim(line)
           end do
        else
           write(*,'(A)') trim(iomsg)
        endif

        close(fh)

    end if

    if (present(exitstat)) then
        exitstat = stat
    elseif (stat /= 0) then
        call fpm_stop(stat,'*run*: Command '//cmd//redirect_str//' returned a non-zero status code')
    end if

end subroutine run

!> Delete directory using system OS remove directory commands
subroutine os_delete_dir(unix, dir, echo)
    logical, intent(in) :: unix
    character(len=*), intent(in) :: dir
    logical, intent(in), optional :: echo

    if (unix) then
        call run('rm -rf ' // dir, echo=echo,verbose=.false.)
    else
        call run('rmdir /s/q ' // dir, echo=echo,verbose=.false.)
    end if

end subroutine os_delete_dir

    !> Determine the path prefix to the local folder. Used for installation, registry etc.
    function get_local_prefix(os) result(prefix)
        !> Installation prefix
        character(len=:), allocatable :: prefix
        !> Platform identifier
        integer, intent(in), optional :: os

        !> Default installation prefix on Unix platforms
        character(len=*), parameter :: default_prefix_unix = "/usr/local"
        !> Default installation prefix on Windows platforms
        character(len=*), parameter :: default_prefix_win = "C:\"

        character(len=:), allocatable :: home

        if (os_is_unix(os)) then
            call env_variable(home, "HOME")
            if (allocated(home)) then
                prefix = join_path(home, ".local")
            else
                prefix = default_prefix_unix
            end if
        else
            call env_variable(home, "APPDATA")
            if (allocated(home)) then
                prefix = join_path(home, "local")
            else
                prefix = default_prefix_win
            end if
        end if

    end function get_local_prefix

    !> Returns .true. if provided path is absolute.
    !>
    !> `~` not treated as absolute.
    logical function is_absolute_path(path, is_unix)
        character(len=*), intent(in) :: path
        logical, optional, intent(in):: is_unix
        character(len=*), parameter :: letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
        logical :: is_unix_os

        if (present(is_unix)) then
            is_unix_os = is_unix
        else
            is_unix_os = os_is_unix()
        end if

        if (is_unix_os) then
            is_absolute_path = path(1:1) == '/'
        else
            if (len(path) < 2) then
                is_absolute_path = .false.
                return
            end if

            is_absolute_path = index(letters, path(1:1)) /= 0 .and. path(2:2) == ':'
        end if

    end function is_absolute_path

    !> Get the HOME directory on Unix and the %USERPROFILE% directory on Windows.
    subroutine get_home(home, error)
        character(len=:), allocatable, intent(out) :: home
        type(error_t), allocatable, intent(out) :: error

        if (os_is_unix()) then
            call env_variable(home, 'HOME')
            if (.not. allocated(home)) then
                call fatal_error(error, "Couldn't retrieve 'HOME' variable")
                return
            end if
        else
            call env_variable(home, 'USERPROFILE')
            if (.not. allocated(home)) then
                call fatal_error(error, "Couldn't retrieve '%USERPROFILE%' variable")
                return
            end if
        end if
    end subroutine get_home

    !> Execute command line and return output as a string.
    subroutine execute_and_read_output(cmd, output, error, exitstat)
        character(len=*), intent(in) :: cmd
        character(len=:), allocatable, intent(out) :: output
        type(error_t), allocatable, intent(out) :: error
        integer, intent(out), optional :: exitstat

        integer :: cmdstat, unit, stat = 0
        character(len=:), allocatable :: cmdmsg, tmp_path
        character(len=1000) :: output_line

        call get_tmp_directory(tmp_path, error=error)
        if (allocated(error)) return

        if (.not. exists(tmp_path)) call mkdir(tmp_path)
        tmp_path = join_path(tmp_path, 'command_line_output')
        call delete_file(tmp_path)
        call filewrite(tmp_path, [''])

        call execute_command_line(cmd//' > '//tmp_path, exitstat=exitstat, cmdstat=cmdstat)
        if (cmdstat /= 0) then
          print *, 'Command failed: "', cmd, '"'
          call fpm_stop(1,'*run*:'//trim(cmdmsg))
        end if

        open(unit, file=tmp_path, action='read', status='old')
        output = ''
        do
          read(unit, *, iostat=stat) output_line
          if (stat /= 0) exit
          output = output//trim(output_line)//' '
        end do
        close(unit, status='delete')
    end

    !> Get system-dependent tmp directory.
    subroutine get_tmp_directory(tmp_dir, error)
        character(len=:), allocatable, intent(out) :: tmp_dir
        type(error_t), allocatable, intent(out) :: error

        tmp_dir = get_env('TMPDIR', '')
        if (tmp_dir /= '') then
          tmp_dir = join_path(tmp_dir, 'fpm'); return
        end if

        tmp_dir = get_env('TMP', '')
        if (tmp_dir /= '') then
          tmp_dir = join_path(tmp_dir, 'fpm'); return
        end if

        tmp_dir = get_env('TEMP', '')
        if (tmp_dir /= '') then
          tmp_dir = join_path(tmp_dir, 'fpm'); return
        end if

        call fatal_error(error, "Couldn't retrieve system temporary directory.")
    end

end module fpm_filesystem
