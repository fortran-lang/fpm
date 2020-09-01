module fpm
use fpm_strings
use fpm_environment, only: run, get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only: number_of_rows, list_files, exists
use fpm_sources
use fpm_backend
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
type(string_t), allocatable :: lib_files(:)
type(string_t), allocatable :: app_files(:)
type(string_t), allocatable :: files(:)
character(:), allocatable :: basename, pkg_name, linking
integer :: i, n

type(srcfile_t), allocatable :: sources(:)
character(:), allocatable :: file_parts(:)

print *, "# Building project"

call list_files("src", lib_files)
lib_files = [(string_t("src/"//lib_files(i)%s),i=1,size(lib_files))]

call list_files("app", app_files)
app_files = [(string_t("app/"//app_files(i)%s),i=1,size(app_files))]

files = [lib_files, app_files]

call scan_sources(files,sources)

call resolve_dependencies(sources)

linking = ""
do i=1,size(sources)

    if (sources(i)%unit_type == FPM_UNIT_MODULE .or. &
        sources(i)%unit_type == FPM_UNIT_SUBMODULE .or. &
        sources(i)%unit_type == FPM_UNIT_SUBPROGRAM .or. &
        sources(i)%unit_type == FPM_UNIT_CSOURCE) then
    
            call build_source(sources(i),linking)

    end if
     
end do

do i=1,size(sources)

    if (sources(i)%unit_type == FPM_UNIT_PROGRAM) then

        call split(sources(i)%file_name,file_parts,delimiters='\/.')
        basename = file_parts(size(file_parts)-1)
        
        call run("gfortran " // sources(i)%file_name // linking // " -o " // basename)

    end if

end do

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
