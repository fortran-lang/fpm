module test_filesystem
    use testsuite, only: new_unittest, unittest_t, error_t, test_failed
    use fpm_filesystem, only: canon_path, is_dir, mkdir, os_delete_dir, &
                              join_path, is_absolute_path, get_home, &
                              delete_file, read_lines, get_temp_filename
    use fpm_environment, only: OS_WINDOWS, get_os_type, os_is_unix
    implicit none
    private

    public :: collect_filesystem

contains

    !> Collect all exported unit tests
    subroutine collect_filesystem(tests)

        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest("canon-path", test_canon_path), &
            & new_unittest("create-delete-directory", test_mkdir_rmdir), &
            & new_unittest("test-is-absolute-path", test_is_absolute_path), &
            & new_unittest("test-get-home", test_get_home), &
            & new_unittest("test-crlf-lines", test_dir_with_crlf) &
            ]

    end subroutine collect_filesystem

    subroutine test_canon_path(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check_string(error, &
            & canon_path("git/project/src/origin"), "git/project/src/origin")
        if (allocated(error)) return

        call check_string(error,  &
            & canon_path("./project/src/origin"), "project/src/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("./project/src///origin/"), "project/src/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("../project/./src/origin/"), "../project/src/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/project//src/origin/"), "/project/src/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/project/src/../origin/"), "/project/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/project/src/../origin/.."), "/project")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/project/src//../origin/."), "/project/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("../project/src/./../origin/."), "../project/origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("../project/src/../../../origin/."), "../../origin")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/../.."), "/")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("././././././/////a/b/.///././////.///c/../../../"), ".")
        if (allocated(error)) return

        call check_string(error, &
            & canon_path("/./././././/////a/b/.///././////.///c/../../../"), "/")
        if (allocated(error)) return

    end subroutine test_canon_path

    !> Check a character variable against a reference value
    subroutine check_string(error, actual, expected)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Actual string value
        character(len=*), intent(in) :: actual

        !> Expected string value
        character(len=*), intent(in) :: expected

        if (actual /= expected) then
            call test_failed(error, &
                             "Character value mismatch "// &
                             "expected '"//expected//"' but got '"//actual//"'")
        end if

    end subroutine check_string

    subroutine test_mkdir_rmdir(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check_mkdir(error, join_path("tmpdir", "subdir"))
        if (allocated(error)) return

        call check_rmdir(error, "tmpdir")
        if (allocated(error)) return

    end subroutine test_mkdir_rmdir

    !> Create a directory and verify its existence
    subroutine check_mkdir(error, path)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Directory path
        character(len=*), intent(in) :: path

        ! Directory shouldn't exist before it's created
        if (is_dir(path)) then
            call test_failed(error, &
                             "Directory path "//path//" already exists before its creation")
            return
        end if

        ! Create directory
        call mkdir(path)

        ! Check that directory is indeed created
        if (.not. is_dir(path)) then
            call test_failed(error, &
                             "Directory path "//path//" cannot be created")
        end if

    end subroutine check_mkdir

    !> Create a directory and verify its existence
    subroutine check_rmdir(error, path)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Directory path
        character(len=*), intent(in) :: path

        ! Directory should exist before it's deleted
        if (.not. is_dir(path)) then
            call test_failed(error, &
                             "Directory path "//path//" doesn't exist before its deletion")
            return
        end if

        ! Delete directory
        call os_delete_dir(os_is_unix(), path)

        ! Check that directory is indeed deleted
        if (is_dir(path)) then
            call test_failed(error, &
                             "Directory path "//path//" cannot be deleted")
        end if

    end subroutine check_rmdir

    subroutine test_is_absolute_path(error)
        type(error_t), allocatable, intent(out) :: error

        ! Unix tests
        if (is_absolute_path('.', is_unix=.true.)) then
            call test_failed(error, "Path '.' isn't absolute")
            return
        end if

        if (is_absolute_path('abc', is_unix=.true.)) then
            call test_failed(error, "Path 'abc' isn't absolute")
            return
        end if

        if (is_absolute_path('~a', is_unix=.true.)) then
            call test_failed(error, "Path '~a' isn't absolute")
            return
        end if

        if (is_absolute_path('C:', is_unix=.true.)) then
            call test_failed(error, "Path 'C:' isn't absolute on Unix")
            return
        end if

        if (is_absolute_path('~', is_unix=.true.)) then
            call test_failed(error, "Path '~' isn't absolute")
            return
        end if

        if (is_absolute_path('~/', is_unix=.true.)) then
            call test_failed(error, "Path '~/' isn't absolute")
            return
        end if

        if (.not. is_absolute_path('/', is_unix=.true.)) then
            call test_failed(error, "Path '/' is absolute")
            return
        end if

        if (.not. is_absolute_path('/a', is_unix=.true.)) then
            call test_failed(error, "Path '/a' is absolute")
            return
        end if

        ! Windows tests
        if (is_absolute_path('abc', is_unix=.false.)) then
            call test_failed(error, "Path 'abc' isn't absolute")
            return
        end if

        if (is_absolute_path('..', is_unix=.false.)) then
            call test_failed(error, "Path '..' isn't absolute")
            return
        end if

        if (is_absolute_path('~', is_unix=.false.)) then
            call test_failed(error, "Path '~' isn't absolute")
            return
        end if

        if (is_absolute_path('/', is_unix=.false.)) then
            call test_failed(error, "Path '/' isn't absolute on Windows")
            return
        end if

        if (is_absolute_path('c/', is_unix=.false.)) then
            call test_failed(error, "Path 'c/' isn't absolute")
            return
        end if

        if (is_absolute_path('1:', is_unix=.false.)) then
            call test_failed(error, "Path '1:' isn't absolute")
            return
        end if

        if (is_absolute_path('C', is_unix=.false.)) then
            call test_failed(error, "Path 'C' isn't absolute")
            return
        end if

        if (.not. is_absolute_path('C:', is_unix=.false.)) then
            call test_failed(error, "Path 'C:' is absolute")
            return
        end if

        if (.not. is_absolute_path('x:', is_unix=.false.)) then
            call test_failed(error, "Path 'x:' is absolute")
            return
        end if

        if (.not. is_absolute_path('x:xyz', is_unix=.false.)) then
            call test_failed(error, "Path 'x:xyz' is absolute")
            return
        end if

    end subroutine test_is_absolute_path

    subroutine test_get_home(error)
        type(error_t), allocatable, intent(out) :: error
        character(len=:), allocatable :: home
        character(len=*), parameter :: letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

        call get_home(home, error)
        if (allocated(error)) return

        if (os_is_unix()) then
            if (home(1:1) /= '/') then
                call test_failed(error, "This doesn't seem to be the correct home path: '"//home//"'")
                return
            end if
        else
            if (index(letters, home(1:1)) == 0 .or. home(2:2) /= ':') then
                call test_failed(error, "This doesn't seem to be the correct home path: '"//home//"'")
                return
            end if
        end if

    end subroutine test_get_home
    
    ! On MS windows, 
    subroutine test_dir_with_crlf(error)
        type(error_t), allocatable, intent(out) :: error
        
        character,    parameter :: CR = achar(13)
        character,    parameter :: LF = new_line('A')
        character(*), parameter :: CRLF = CR//LF
        
        character(*), parameter :: test_lines = 'build.f90'//CRLF//&
                                                'dependency.f90'//CRLF//&
                                                'example.f90'//CRLF//&
                                                'executable.f90'//CRLF//&
                                                'fortran.f90'//CRLF
                                               
        type(string_t), allocatable :: lines(:)
        character(len=:), allocatable :: temp_file
        integer :: unit, i, ios
        
        temp_file = get_temp_filename()
        
        open(newunit=unit,file=temp_file,access='stream',action='write',iostat=ios)
        if (ios/=0) then 
            call test_failed(error, "cannot create temporary file")
            return
        end if
        
        write(unit,iostat=ios) test_lines
        if (ios/=0) then 
            call test_failed(error, "cannot write to temporary file")
            return
        end if
                
        close(unit,iostat=ios)
        if (ios/=0) then 
            call test_failed(error, "cannot close temporary file")
            return
        end if        
        
        lines = read_lines(temp_file) 
        
        if (.not.allocated(lines)) then 
            call test_failed(error, "Failed reading file with CRLF: no output")
            return
        end if
        
        if (size(lines)/=5) then 
            call test_failed(error, "Failed reading file with CRLF: wrong number of lines")
            return
        end if
        
        if (lines(1)/='build.f90') then 
            call test_failed(error, "Failed reading file with CRLF: at build.f90")
            return
        end if
        if (lines(2)/='dependency.f90') then 
            call test_failed(error, "Failed reading file with CRLF: at dependency.f90")
            return
        end if
        if (lines(3)/='example.f90') then 
            call test_failed(error, "Failed reading file with CRLF: at example.f90")
            return
        end if
        if (lines(4)/='executable.f90') then 
            call test_failed(error, "Failed reading file with CRLF: at executable.f90")
            return
        end if
        if (lines(5)/='fortran.f90') then 
            call test_failed(error, "Failed reading file with CRLF: at fortran.f90")
            return
        end if                                
        
        call delete_dile(temp_file)
        
    end subroutine test_dir_with_crlf
    

end module test_filesystem
