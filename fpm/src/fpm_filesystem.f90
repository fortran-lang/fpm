module fpm_filesystem
    use fpm_environment, only: get_os_type, &
                               OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                               OS_CYGWIN, OS_SOLARIS, OS_FREEBSD
    use fpm_strings, only: f_string, string_t, split
    implicit none
    private
    public :: basename, canon_path, dirname, is_dir, join_path, number_of_rows, read_lines, list_files,&
            mkdir, exists, get_temp_filename, windows_path

    integer, parameter :: LINE_BUFFER_LEN = 1000

contains


function basename(path,suffix) result (base)
    ! Extract filename from path with/without suffix
    !
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
        base = trim(file_parts(size(file_parts)))
    else
        call split(path,file_parts,delimiters='\/.')
        base = trim(file_parts(size(file_parts)-1))
    end if

end function basename


function canon_path(path) result(canon)
    ! Canonicalize path for comparison
    !  Handles path string redundancies
    !  Does not test existence of path
    ! 
    ! To be replaced by realpath/_fullname in stdlib_os
    !
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


function dirname(path) result (dir)
    ! Extract dirname from path
    !
    character(*), intent(In) :: path
    character(:), allocatable :: dir

    character(:), allocatable :: file_parts(:)

    dir = path(1:scan(path,'/\',back=.true.))

end function dirname


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


function join_path(a1,a2,a3,a4,a5) result(path)
    ! Construct path by joining strings with os file separator
    !
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


integer function number_of_rows(s) result(nrows)
    ! determine number or rows
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

subroutine mkdir(dir)
    character(len=*), intent(in) :: dir
    integer                      :: stat

    select case (get_os_type())
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
            call execute_command_line('mkdir -p ' // dir, exitstat=stat)
            write (*, '(2a)') 'mkdir -p ' // dir

        case (OS_WINDOWS)
            call execute_command_line("mkdir " // windows_path(dir), exitstat=stat)
            write (*, '(2a)') 'mkdir ' // windows_path(dir)
    end select

    if (stat /= 0) then
        print *, 'execute_command_line() failed'
        error stop
    end if
end subroutine mkdir


recursive subroutine list_files(dir, files, recurse)
    character(len=*), intent(in) :: dir
    type(string_t), allocatable, intent(out) :: files(:)
    logical, intent(in), optional :: recurse

    integer :: stat, fh, i
    character(:), allocatable :: temp_file
    type(string_t), allocatable :: dir_files(:)
    type(string_t), allocatable :: sub_dir_files(:)

    ! Using `inquire` / exists on directories works with gfortran, but not ifort
    if (.not. exists(dir)) then
        allocate (files(0))
        return
    end if

    allocate (temp_file, source=get_temp_filename())

    select case (get_os_type())
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD)
            call execute_command_line('ls ' // dir // ' > ' // temp_file, &
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


logical function exists(filename) result(r)
    character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function


function get_temp_filename() result(tempfile)
    ! Get a unused temporary filename
    !  Calls posix 'tempnam' - not recommended, but
    !   we have no security concerns for this application
    !   and use here is temporary.
    ! Works with MinGW
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


function windows_path(path) result(winpath)
    ! Replace file system separators for windows
    !
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


function unix_path(path) result(nixpath)
    ! Replace file system separators for unix
    !
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

end module fpm_filesystem
