module fpm
use FPM_Strings
use environment, only: run, get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use FPM_Filesystem, only: number_of_rows, list_files, exists
implicit none
private
public :: cmd_build, cmd_install, cmd_new, cmd_run, cmd_test


contains


subroutine package_name(name)
character(:), allocatable, intent(out) :: name
! Currrently a heuristic. We should update this to read the name from fpm.toml
if (exists("src/fpm.f90")) then
    name = "fpm"
else
    name = "hello_world"
end if
end subroutine

subroutine cmd_build()
type(string_t), allocatable :: files(:)
character(:), allocatable :: basename, pkg_name, linking
integer :: i, n
print *, "# Building project"
call list_files("src", files)
linking = ""
do i = 1, size(files)
    if (str_ends_with(files(i)%s, ".f90")) then
        n = len(files(i)%s)
        basename = files(i)%s(1:n-4)
        call run("gfortran -c src/" // basename // ".f90 -o " // basename // ".o")
        linking = linking // " " // basename // ".o"
    end if
end do
call run("gfortran -c app/main.f90 -o main.o")
call package_name(pkg_name)
call run("gfortran main.o " // linking // " -o " // pkg_name)
end subroutine

subroutine cmd_install()
    print *, "fpm error: 'fpm install' not implemented."
    error stop 1
end subroutine

subroutine cmd_new()
    print *, "fpm error: 'fpm new' not implemented."
    error stop 1
end subroutine

subroutine cmd_run()
    print *, "fpm error: 'fpm run' not implemented."
    error stop 1
end subroutine

subroutine cmd_test()
    print *, "fpm error: 'fpm test' not implemented."
    error stop 1
end subroutine

end module fpm
