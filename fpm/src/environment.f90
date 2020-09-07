module environment
use,intrinsic     :: iso_c_binding,   only : c_null_char, c_int
use,intrinsic     :: iso_c_binding,   only : c_char
use,intrinsic     :: iso_c_binding,   only : c_float
use,intrinsic     :: iso_c_binding,   only : c_ptr, c_f_pointer, c_null_ptr
use,intrinsic     :: iso_c_binding
use,intrinsic     :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
private

integer, parameter, public :: OS_LINUX = 1
integer, parameter, public :: OS_MACOS = 2
integer, parameter, public :: OS_WINDOWS = 3

public :: get_os_type

public :: system_mkdir
public :: system_chdir
public :: system_perror
public :: system_getcwd
public :: R_GRP, R_OTH, R_USR, RWX_G, RWX_O, RWX_U, W_GRP, W_OTH, W_USR, X_GRP, X_OTH, X_USR
public :: ifmkdir
public :: filewrite
public :: splitpath

integer,parameter,public :: mode_t=int32
!integer(kind=mode_t),bind(c,name="S_IRGRP") :: R_GRP
!integer(kind=mode_t),bind(c,name="S_IROTH") :: R_OTH
!integer(kind=mode_t),bind(c,name="S_IRUSR") :: R_USR
!integer(kind=mode_t),bind(c,name="S_IRWXG") :: RWX_G
!integer(kind=mode_t),bind(c,name="S_IRWXO") :: RWX_O
!integer(kind=mode_t),bind(c,name="S_IRWXU") :: RWX_U
!integer(kind=mode_t),bind(c,name="S_IWGRP") :: W_GRP
!integer(kind=mode_t),bind(c,name="S_IWOTH") :: W_OTH
!integer(kind=mode_t),bind(c,name="S_IWUSR") :: W_USR
!integer(kind=mode_t),bind(c,name="S_IXGRP") :: X_GRP
!integer(kind=mode_t),bind(c,name="S_IXOTH") :: X_OTH
!integer(kind=mode_t),bind(c,name="S_IXUSR") :: X_USR

integer(kind=mode_t),parameter :: R_GRP=32_mode_t
integer(kind=mode_t),parameter :: R_OTH=4_mode_t
integer(kind=mode_t),parameter :: R_USR=256_mode_t
integer(kind=mode_t),parameter :: RWX_G=56_mode_t
integer(kind=mode_t),parameter :: RWX_O=7 _mode_t
integer(kind=mode_t),parameter :: RWX_U=448_mode_t
integer(kind=mode_t),parameter :: W_GRP=16_mode_t
integer(kind=mode_t),parameter :: W_OTH=2 _mode_t
integer(kind=mode_t),parameter :: W_USR=128_mode_t
integer(kind=mode_t),parameter :: X_GRP=8 _mode_t
integer(kind=mode_t),parameter :: X_OTH=1 _mode_t
integer(kind=mode_t),parameter :: X_USR=64 _mode_t

contains
    integer function get_os_type() result(r)
    ! Determine the OS type
    !
    ! Returns one of OS_LINUX, OS_MACOS, OS_WINDOWS.
    !
    ! Currently we use the $HOME and $HOMEPATH environment variables to determine
    ! the OS type. That is not 100% accurate in all cases, but it seems to be good
    ! enough for now. See the following issue for a more robust solution:
    !
    ! https://github.com/fortran-lang/fpm/issues/144
    !
    character(len=100) :: val
    integer stat
    ! Only Windows define $HOMEPATH by default and we test its value to improve the
    ! chances of it working even if a user defines $HOMEPATH on Linux or macOS.
    call get_environment_variable("HOMEPATH", val, status=stat)
    if (stat == 0 .and. val(1:7) == "\Users\") then
        r = OS_WINDOWS
        return
    end if

    ! We assume that $HOME=/home/... is Linux, $HOME=/Users/... is macOS, otherwise
    ! we assume Linux. This is only a heuristic and can easily fail.
    call get_environment_variable("HOME", val, status=stat)
    if (stat == 1) then
        print *, "$HOME does not exist"
        error stop
    end if
    if (stat /= 0) then
        print *, "get_environment_variable() failed"
        error stop
    end if
    if (val(1:6) == "/home/") then
        r = OS_LINUX
    else if (val(1:7) == "/Users/") then
        r = OS_MACOS
    else
        ! This will happen on HPC systems that typically do not use either /home nor
        ! /Users for $HOME. Those systems are typically Linux, so for now we simply
        ! set Linux here.
        r = OS_LINUX
    end if
    end function get_os_type
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function system_mkdir(dirname,mode) result(ierr)

! @(#) M_system::system_mkdir(3f): call mkdir(3c) to create empty directory

character(len=*),intent(in)       :: dirname
integer,intent(in)                :: mode
   integer                        :: c_mode
   integer(kind=c_int)            :: err
   integer                        :: ierr

interface
   function c_mkdir(c_path,c_mode) bind(c,name="mkdir") result(c_err)
      import c_char, c_int
      character(len=1,kind=c_char),intent(in) :: c_path(*)
      integer(c_int),intent(in),value         :: c_mode
      integer(c_int)                          :: c_err
   end function c_mkdir
end interface

   c_mode=mode
      err= c_mkdir(str2arr(trim(dirname)),c_mode)
   ierr=err                                          ! c_int to default integer kind
end function system_mkdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function str2arr(string) result (array)

!character(len=*),parameter::ident_32="@(#)M_system::str2arr(3fp): function copies string to null terminated char array"

character(len=*),intent(in)     :: string
character(len=1,kind=c_char)    :: array(len(string)+1)
   integer                      :: i

   do i = 1,len_trim(string)
      array(i) = string(i:i)
   enddo
   array(i:i)=c_null_char

end function str2arr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine system_chdir(path, err)

!character(len=*),parameter::ident_15="@(#)M_system::system_chdir(3f): call chdir(3c)"

character(len=*)               :: path
integer, optional, intent(out) :: err

interface
   integer(kind=c_int)  function c_chdir(c_path) bind(C,name="chdir")
      import c_char, c_int
      character(kind=c_char)   :: c_path(*)
   end function
end interface
   integer                     :: loc_err

   loc_err=c_chdir(str2arr(trim(path)))
   if(present(err))then
      err=loc_err
   endif
end subroutine system_chdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine system_perror(prefix)
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment

! @(#) M_system::system_perror(3f): call perror(3c) to display error message

character(len=*),intent(in) :: prefix
   integer                  :: ios

interface
  subroutine c_perror(c_prefix) bind (C,name="perror")
  import c_char
  character(kind=c_char) :: c_prefix(*)
  end subroutine c_perror
end interface

   flush(unit=ERROR_UNIT,iostat=ios)
   flush(unit=OUTPUT_UNIT,iostat=ios)
   flush(unit=INPUT_UNIT,iostat=ios)
   call c_perror(str2arr((trim(prefix))))
   !!call c_flush()

end subroutine system_perror
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine filewrite(filename,filedata)
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
! write filedata to file filename
character(len=*),intent(in)           :: filename
character(len=*),intent(in)           :: filedata(:)
integer                               :: lun, i, ios
character(len=256)                    :: message
   message=' '
   ios=0
   if(filename.ne.' ')then
    open(file=filename, &
    & newunit=lun, &
    & form='formatted', &      !  FORM      =  FORMATTED   |  UNFORMATTED
    & access='sequential', &   !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
    & action='write', &        !  ACTION    =  READ|WRITE  |  READWRITE
    & position='rewind', &     !  POSITION  =  ASIS        |  REWIND       |  APPEND
    & status='new', &          !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
    & iostat=ios, &
    & iomsg=message)
   else
      lun=stdout
      ios=0
   endif
   if(ios.ne.0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',filename,trim(message)
      error stop 1
   endif
   do i=1,size(filedata)                                                    ! write file
      write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
      if(ios.ne.0)then
         write(stderr,'(*(a,1x))')'*filewrite* error:',filename,trim(message)
         stop 4
      endif
   enddo
   close(unit=lun,iostat=ios,iomsg=message)                                 ! close file
   if(ios.ne.0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',trim(message)
      error stop 2
   endif
end subroutine filewrite
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine system_getcwd(output,ierr)

!$@(#) M_system::system_getcwd(3f):call getcwd(3c) to get pathname of current working directory

character(len=:),allocatable,intent(out) :: output
integer,intent(out)                      :: ierr
integer(kind=c_long),parameter           :: length=4097_c_long
character(kind=c_char,len=1)             :: buffer(length)
type(c_ptr)                              :: buffer2
interface
   function c_getcwd(buffer,size) bind(c,name="getcwd") result(buffer_result)
      import c_char, c_size_t, c_ptr
      character(kind=c_char) ,intent(out) :: buffer(*)
      integer(c_size_t),value,intent(in)  :: size
      type(c_ptr)                         :: buffer_result
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   buffer=' '
   buffer2=c_getcwd(buffer,length)
   if(.not.c_associated(buffer2))then
      output=''
      ierr=-1
   else
      output=trim(arr2str(buffer))
      ierr=0
   endif
end subroutine system_getcwd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
logical function exists(filename) result(r)
character(len=*), intent(in) :: filename
inquire(file=filename, exist=r)
end function
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental impure subroutine ifmkdir(dir)
character(len=*),intent(in) :: dir
if (.not. exists(dir)) then
   if( system_mkdir(dir, IANY([R_USR, W_USR, X_USR]) ) .ne. 0)then       ! make new directory or stop
      call system_perror('error: *fpm::system_mkdir*:'//dir)
      stop
   endif
endif
end subroutine ifmkdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function arr2str(array)  result (string)

!$@(#) M_system::arr2str(3fp): function copies null-terminated char array to string

character(len=1),intent(in)  :: array(:)
character(len=size(array))   :: string
integer                      :: i

   string=' '
   do i = 1,size(array)
      if(array(i).eq.char(0))then
         exit
      else
         string(i:i) = array(i)
      endif
   enddo

end function arr2str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     splitpath(3f) - [M_io] split a Unix pathname into components
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   splitpath(path,dir,name,basename,ext)
!!
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),intent(in)  :: path
!!    character(len=maxlen),intent(out),optional :: dir
!!    character(len=maxlen),intent(out),optional :: name
!!    character(len=maxlen),intent(out),optional :: basename
!!    character(len=maxlen),intent(out),optional :: ext
!!
!!##DESCRIPTION
!!    splitpath(3f) splits given pathname assuming a forward slash separates
!!    filename components and that the right-most period in the last leaf
!!    of the pathname is considered the beginning of an extension. If
!!    an extension is found it is left present in NAME but removed from
!!    BASENAME.
!!
!!    This routine does not check the system for the existence or type of
!!    the filename components; it merely parses a string.
!!
!!    Assumes leaf separator is a slash ('/') and that filename does not
!!    contain trailing spaces.
!!
!!##OPTIONS
!!    path      Path to be broken into components. It is assumed
!!
!!              o Forward slashes (/) separate pathname components.
!!              o the name '.' means "current directory"
!!              o the name '..' means "up one directory"
!!              o a pathname ending in a slash is a directory name
!!              o a slash starting the pathname represents the root
!!                directory.
!!              o trailing spaces are insignificant.
!!
!!    Using these rules helps to reduce incorrect parsing, but the
!!    routine is only intended for simple parsing of names of the form
!!    "[dir/]name[.extension].
!!
!!##RESULTS
!!    dir       Path of directories, including the trailing slash.
!!    name      Name of file leaf or, if no file is specified in path,
!!              name of the lowest directory.
!!    basename  NAME with any extension removed
!!    ext       File name extension, if any, including the leading period (.).
!!
!!    The path parameter can be a complete or partial file specification. The
!!    special name "." is assumed to mean the current directory, and the
!!    special name ".." is assumed to mean one directory above the current
!!    directory.
!!
!!##EXAMPLE
!!
!!   program demo_splitpath
!!
!!    use m_io, only : splitpath
!!    implicit none
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),parameter   :: file(*)=[&
!!       & 'dirs/name.ext  ', &
!!       & 'xx/IO/zz/NN.FF ', &
!!       & 'xx/IO/zz/NN    ', &
!!       & '/xx/IO/zz/NN   ', &
!!       & '/xx/IO/zz/     ', &
!!       & '/xx/IO/zz.A/   ', &
!!       & '/xx/IO/zz/.    ', &
!!       & '               ', &
!!       & './             ', &
!!       & '/              ', &
!!       & '/..            ', &
!!       & './..           ', &
!!       & 'name.          ', &
!!       & '.name          ', &
!!       & '.name.         ', &
!!       & '.              ', &
!!       & '..             ', &
!!       & '...            ']
!!
!!    character(len=maxlen)  :: dir
!!    character(len=maxlen)  :: name
!!    character(len=maxlen)  :: basename
!!    character(len=maxlen)  :: ext
!!    integer                :: i
!!    integer                :: longest
!!    longest=maxval(len_trim(file)) ! find longest filename
!!
!!    do i=1,size(file)
!!       call splitpath(file(i), dir, name, basename, ext)
!!       write(*,'(*("| ",a:))')  &
!!       & file(i)(:longest),     &
!!       & dir(:longest),         &
!!       & name(:longest),        &
!!       & basename(:longest),    &
!!       & ext(:longest)
!!    enddo
!!   end program demo_splitpath
!!
!!   Output
!!
!!    | dirs/name.ext | dirs          | name.ext      | name          | .ext
!!    | xx/IO/zz/NN.FF| xx/IO/zz      | NN.FF         | NN            | .FF
!!    | xx/IO/zz/NN   | xx/IO/zz      | NN            | NN            |
!!    | /xx/IO/zz/NN  | /xx/IO/zz     | NN            | NN            |
!!    | /xx/IO/zz/    | /xx/IO/zz     |               |               |
!!    | /xx/IO/zz.A/  | /xx/IO/zz.A   |               |               |
!!    | /xx/IO/zz/.   | /xx/IO/zz/.   |               |               |
!!    |               | .             |               |               |
!!    | ./            | .             |               |               |
!!    | /             | /             |               |               |
!!    | /..           | /             |               |               |
!!    | ./..          | ./..          |               |               |
!!    | name.         |               | name.         | name          | .
!!    | .name         |               | .name         | .name         |
!!    | .name.        |               | .name.        | .name         | .
!!    | .             | .             |               |               |
!!    | ..            |               |               |               |
!!    | ...           |               | ...           | ..            | .
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine splitpath(path,dir,name,basename,ext)
implicit none

! ident_9="@(#)M_io::splitpath(3f): split Unix pathname into components (dir,name,basename,extension)"

!===================================================================================================================================
character(len=*),intent(in)           :: path
character(len=:),intent(out),allocatable,optional :: dir
character(len=:),intent(out),allocatable,optional :: name
character(len=:),intent(out),allocatable,optional :: basename
character(len=:),intent(out),allocatable,optional :: ext
integer,parameter                     :: maxlen=4096
character(len=maxlen)                 :: dir_local
character(len=maxlen)                 :: name_local
character(len=maxlen)                 :: basename_local
character(len=maxlen)                 :: ext_local
character(len=len(path)+1)            :: path_local
integer                               :: where
integer                               :: i
integer                               :: iend
!===================================================================================================================================
   path_local=path                           ! initialize variables
   dir_local=''
   name_local=''
   basename_local=''
   ext_local=''
   iend=len_trim(path_local)
   LOCAL : block
!===================================================================================================================================
   if(iend.eq.0)then                         ! blank input path
      dir_local='.'
      exit LOCAL
   endif
!===================================================================================================================================
   if(path_local(iend:iend).eq.'/')then      ! assume entire name is a directory if it ends in a slash
      if(iend.gt.1)then
         dir_local=path_local(:iend-1)
      else                                   ! if just a slash it means root directory so leave it as slash
         dir_local=path_local
      endif
      exit LOCAL
   endif
!===================================================================================================================================
   TRIMSLASHES: do i=iend,1,-1               ! trim off trailing slashes even if duplicates
      if(path_local(i:i).eq.'/')then
         path_local(i:i)=' '
         iend=i-1
      else
         iend=i
         exit TRIMSLASHES
      endif
   enddo TRIMSLASHES

   if(iend.eq.0)then                         ! path composed entirely of slashes.
      dir_local='/'
      exit LOCAL
   endif
!===================================================================================================================================
   where=INDEX(path_local,'/',BACK=.true.)   ! find any right-most slash in remaining non-null name_local after trimming trailing slashes
   if(where.le.0)then                        ! no slash in path so everything left is name_local
      name_local=path_local(:iend)                 ! this is name_local unless '.' or '..'
   else                                      ! last slash found
      dir_local=path_local(:where-1)               ! split into directory
      name_local=path_local(where+1:iend)          ! this is name_local unless '.' or '..'
   endif
!===================================================================================================================================
   select case (name_local(1:3))                   ! special cases where name_local is a relative directory name_local '.' or '..'
   case('.  ')
      dir_local=path_local
      name_local=''
   case('.. ')
      if(dir_local.eq.'')then
         if(path_local(1:1).eq.'/')then
            dir_local='/'
         endif
      else
         dir_local=path_local
      endif
      name_local=''
   case default
   end select
!===================================================================================================================================
   if(name_local.eq.'.')then
      name_local=''
   endif
!===================================================================================================================================
   iend=len_trim(name_local)
   where=INDEX(name_local,'.',BACK=.true.)         ! find any extension
   if(where.gt.0.and.where.ne.1)then         ! only consider a non-blank extension name_local
      ext_local=name_local(where:)
      basename_local=name_local(:where-1)
   else
      basename_local=name_local
   endif
!===================================================================================================================================
   endblock LOCAL
   if(present(dir))dir=trim(dir_local)
   if(present(name))name=trim(name_local)
   if(present(basename))basename=trim(basename_local)
   if(present(ext))ext=trim(ext_local)
end subroutine splitpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module
