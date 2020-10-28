module fpm
use fpm_strings, only: string_t, str_ends_with, operator(.in.)
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings, fpm_new_settings, &
                      fpm_run_settings, fpm_install_settings, fpm_test_settings
use fpm_environment, only: run
use fpm_filesystem, only: is_dir, join_path, number_of_rows, list_files, exists, basename
use fpm_model, only: srcfile_ptr, srcfile_t, fpm_model_t, &
                    FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
                    FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST

use fpm_sources, only: add_executable_sources, add_sources_from_dir, &
                       resolve_module_dependencies
use fpm_manifest, only : get_package_data, default_executable, &
    default_library, package_t, default_test
use fpm_error, only : error_t, fatal_error
use fpm_manifest_test, only : test_t
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
use fpm_manifest_dependency, only: dependency_t
implicit none
private
public :: cmd_build, cmd_install, cmd_run, cmd_test

contains


recursive subroutine add_libsources_from_package(sources,package_list,package, &
                                                  package_root,dev_depends,error)
    ! Discover library sources in a package, recursively including dependencies
    !
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    type(string_t), allocatable, intent(inout) :: package_list(:)
    type(package_t), intent(in) :: package
    character(*), intent(in) :: package_root
    logical, intent(in) :: dev_depends
    type(error_t), allocatable, intent(out) :: error

    ! Add package library sources
    if (allocated(package%library)) then

        call add_sources_from_dir(sources, join_path(package_root,package%library%source_dir), &
                                    FPM_SCOPE_LIB, error=error)

        if (allocated(error)) then
            return
        end if

    end if

    ! Add library sources from dependencies
    if (allocated(package%dependency)) then

        call add_dependencies(package%dependency)

        if (allocated(error)) then
            return
        end if

    end if

    ! Add library sources from dev-dependencies
    if (dev_depends .and. allocated(package%dev_dependency)) then

        call add_dependencies(package%dev_dependency)

        if (allocated(error)) then
            return
        end if

    end if

    contains

    subroutine add_dependencies(dependency_list)
        type(dependency_t), intent(in) :: dependency_list(:)

        integer :: i
        type(string_t) :: dep_name
        type(package_t) :: dependency

        character(:), allocatable :: dependency_path

        do i=1,size(dependency_list)
            
            if (dependency_list(i)%name .in. package_list) then
                cycle
            end if

            if (allocated(dependency_list(i)%git)) then

                dependency_path = join_path('build','dependencies',dependency_list(i)%name)

                if (.not.exists(join_path(dependency_path,'fpm.toml'))) then
                    call dependency_list(i)%git%checkout(dependency_path, error)
                    if (allocated(error)) return
                end if

            else if (allocated(dependency_list(i)%path)) then
                
                dependency_path = join_path(package_root,dependency_list(i)%path)

            end if

            call get_package_data(dependency, &
                    join_path(dependency_path,"fpm.toml"), error)

            if (allocated(error)) then
                error%message = 'Error while parsing manifest for dependency package at:'//&
                                new_line('a')//join_path(dependency_path,"fpm.toml")//&
                                new_line('a')//error%message
                return
            end if

            if (.not.allocated(dependency%library) .and. &
                    exists(join_path(dependency_path,"src"))) then
                allocate(dependency%library)
                dependency%library%source_dir = "src"
            end if

            
            call add_libsources_from_package(sources,package_list,dependency, &
                package_root=dependency_path, &
                dev_depends=.false., error=error)
            
            if (allocated(error)) then
                error%message = 'Error while processing sources for dependency package "'//&
                                new_line('a')//dependency%name//'"'//&
                                new_line('a')//error%message
                return
            end if

            dep_name%s = dependency_list(i)%name
            package_list = [package_list, dep_name]

        end do

    end subroutine add_dependencies

end subroutine add_libsources_from_package


subroutine build_model(model, settings, package, error)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_t), intent(in) :: package
    type(error_t), allocatable, intent(out) :: error
    integer :: i

    type(string_t), allocatable :: package_list(:)

    model%package_name = package%name

    allocate(package_list(1))
    package_list(1)%s = package%name

    ! #TODO: Choose flags and output directory based on cli settings & manifest inputs
    model%fortran_compiler = 'gfortran'

    if(settings%release)then
        model%output_directory = 'build/gfortran_release'
        model%fortran_compile_flags=' &
            & -O3 &
            & -Wimplicit-interface &
            & -fPIC &
            & -fmax-errors=1 &
            & -ffast-math &
            & -funroll-loops ' // &
            & '-J'//join_path(model%output_directory,model%package_name)
    else
        model%output_directory = 'build/gfortran_debug'
        model%fortran_compile_flags = ' -Wall -Wextra -Wimplicit-interface  -fPIC -fmax-errors=1 -g '// &
                                      '-fbounds-check -fcheck-array-temporaries -fbacktrace '// &
                                      '-J'//join_path(model%output_directory,model%package_name)
    endif
    model%link_flags = ''

    ! Add sources from executable directories
    if (is_dir('app') .and. package%build_config%auto_executables) then
        call add_sources_from_dir(model%sources,'app', FPM_SCOPE_APP, &
                                   with_executables=.true., error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (is_dir('test') .and. package%build_config%auto_tests) then
        call add_sources_from_dir(model%sources,'test', FPM_SCOPE_TEST, &
                                   with_executables=.true., error=error)

        if (allocated(error)) then
            return
        endif

    end if
    if (allocated(package%executable)) then
        call add_executable_sources(model%sources, package%executable, FPM_SCOPE_APP, &
                                     auto_discover=package%build_config%auto_executables, &
                                     error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%test)) then
        call add_executable_sources(model%sources, package%test, FPM_SCOPE_TEST, &
                                     auto_discover=package%build_config%auto_tests, &
                                     error=error)

        if (allocated(error)) then
            return
        endif

    endif

    ! Add library sources, including local dependencies
    call add_libsources_from_package(model%sources,package_list,package, &
                                      package_root='.',dev_depends=.true.,error=error)
    if (allocated(error)) then
        return
    end if

    if(settings%list)then
        do i=1,size(model%sources)
            write(stderr,'(*(g0,1x))')'fpm::build<INFO>:file expected at',model%sources(i)%file_name, &
            & merge('exists        ','does not exist',exists(model%sources(i)%file_name) )
        enddo
        stop
    else
        call resolve_module_dependencies(model%sources,error)
    endif

end subroutine build_model


subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_t) :: package
type(fpm_model_t) :: model
type(error_t), allocatable :: error

call get_package_data(package, "fpm.toml", error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

! Populate library in case we find the default src directory
if (.not.allocated(package%library) .and. exists("src")) then
    allocate(package%library)
    call default_library(package%library)
end if

! Populate executable in case we find the default app
if (.not.allocated(package%executable) .and. &
     exists(join_path('app',"main.f90"))) then
    allocate(package%executable(1))
    call default_executable(package%executable(1), package%name)
end if

if (.not.(allocated(package%library) .or. allocated(package%executable))) then
    print '(a)', "Neither library nor executable found, there is nothing to do"
    error stop 1
end if

call build_model(model, settings, package, error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

call build_package(model)

end subroutine

subroutine cmd_install(settings)
type(fpm_install_settings), intent(in) :: settings
    print *, "fpm error: 'fpm install' not implemented."
    error stop 8
end subroutine cmd_install

subroutine cmd_run(settings)
type(fpm_run_settings), intent(in) :: settings
character(len=:),allocatable       :: release_name, cmd, fname
integer                            :: i, j
type(package_t)                    :: package
type(error_t), allocatable         :: error
character(len=:),allocatable       :: newwords(:)
logical,allocatable                :: foundit(:)
logical                            :: list
    call get_package_data(package, "fpm.toml", error)
    if (allocated(error)) then
        print '(a)', error%message
        stop
    endif
    release_name=trim(merge('gfortran_release','gfortran_debug  ',settings%release))
    newwords=[character(len=0) ::]
    ! Populate executable in case we find the default app directory
    if (.not.allocated(package%executable) .and. exists("app")) then
        allocate(package%executable(1))
        call default_executable(package%executable(1), package%name)
    endif
    if(size(settings%name).eq.0)then
        if ( .not.allocated(package%executable) ) then
            write(stderr,'(*(g0,1x))')'fpm::run<INFO>:no executables found in fpm.toml and no default app/ directory'
            stop
        endif
        allocate(foundit(size(package%executable)))
        do i=1,size(package%executable)
            fname=join_path('build',release_name,package%executable(i)%source_dir,package%executable(i)%name)
            newwords=[character(len=max(len(newwords),len(fname))) :: newwords,fname]
        enddo
        if(size(newwords).lt.1)then
            write(stderr,'(*(g0,1x))')'fpm::run<INFO>:no executables found in fpm.toml'
            stop
        endif
    else
        !*! expand names, duplicates are a problem??
        allocate(foundit(size(settings%name)))
        foundit=.false.
        FINDIT: do i=1,size(package%executable)
            do j=1,size(settings%name)
                if(settings%name(j).eq.package%executable(i)%name)then
                    fname=join_path('build',release_name,package%executable(i)%source_dir,package%executable(i)%name)
                    newwords=[character(len=max(len(newwords),len(fname))) :: newwords,fname]
                    foundit(j)=.true.
                endif
            enddo
        enddo FINDIT
        do i=1,size(settings%name)
            if(.not.foundit(i))then
                write(stderr,'(*(g0,1x))')'fpm::run<ERROR>:executable',trim(settings%name(i)),'not located'
            endif
        enddo
        if(allocated(foundit))deallocate(foundit)
    endif
    do i=1,size(newwords)
        !*! list is a new option for use with xargs, to move files to production area, valgrind, gdb, ls -l, ....
        !*! maybe add as --mask and could do --mask 'echo %xx' or --mask 'cp %XX /usr/local/bin/' an so on
        !*! default if blank would be filename uptodate|needs|updated|doesnotexist creation_date, ...
        !*! or maybe just list filenames so can pipe through xargs, and so on
        if(settings%list)then
            write(stderr,'(*(g0,1x))')'fpm::run<INFO>:executable expected at',newwords(i),&
            & merge('exists        ','does not exist',exists(newwords(i)))
            cycle
        endif
        cmd=newwords(i) // ' ' // settings%args
        if(exists(newwords(i)))then
            call run(cmd)
        else ! try to build -- once build works conditionally this should be an unconditional call
            call cmd_build(fpm_build_settings(release=settings%release,list=.false.))
            if(exists(newwords(i)))then
                call run(cmd)
            else
                write(stderr,*)'fpm::run<ERROR>',cmd,' not found'
            endif
        endif
    enddo
    deallocate(newwords)
end subroutine cmd_run


subroutine cmd_test(settings)
type(fpm_test_settings), intent(in) :: settings
character(len=:),allocatable       :: release_name, cmd, fname
integer                            :: i, j
type(package_t)                    :: package
type(error_t), allocatable         :: error
character(len=:),allocatable       :: newwords(:)
logical,allocatable                :: foundit(:)
logical                            :: list
    call get_package_data(package, "fpm.toml", error)
    if (allocated(error)) then
        print '(a)', error%message
        stop
    endif
    release_name=trim(merge('gfortran_release','gfortran_debug  ',settings%release))
    newwords=[character(len=0) ::]

    ! Populate test in case we find the default test directory
    if (.not.allocated(package%test) .and. exists("test")) then
        allocate(package%test(1))
        call default_test(package%test(1), package%name)
    endif
    if(size(settings%name).eq.0)then
        if ( .not.allocated(package%test) ) then
            write(stderr,'(*(g0,1x))')'fpm::run<INFO>:no tests found in fpm.toml and no default test/ directory'
            stop
        endif
        allocate(foundit(size(package%test)))
        do i=1,size(package%test)
            fname=join_path('build',release_name,package%test(i)%source_dir,package%test(i)%name)
            newwords=[character(len=max(len(newwords),len(fname))) :: newwords,fname]
        enddo
        if(size(newwords).lt.1)then
            write(stderr,'(*(g0,1x))')'fpm::run<INFO>:no tests found in fpm.toml'
            stop
        endif
    else
       !*! expand names, duplicates are a problem??
        allocate(foundit(size(settings%name)))
        foundit=.false.
        FINDIT: do i=1,size(package%test)
            do j=1,size(settings%name)
                if(settings%name(j).eq.package%test(i)%name)then
                    fname=join_path('build',release_name,package%test(i)%source_dir,package%test(i)%name)
                    newwords=[character(len=max(len(newwords),len(fname))) :: newwords,fname]
                    foundit(j)=.true.
                endif
            enddo
        enddo FINDIT
        do i=1,size(settings%name)
            if(.not.foundit(i))then
                write(stderr,'(*(g0,1x))')'fpm::run<ERROR>:test',trim(settings%name(i)),'not located'
            endif
        enddo
        if(allocated(foundit))deallocate(foundit)
    endif
    do i=1,size(newwords)
        !*! list is a new option for use with xargs, to move files to production area, valgrind, gdb, ls -l, ....
        !*! maybe add as --mask and could do --mask 'echo %xx' or --mask 'cp %XX /usr/local/bin/' an so on
        !*! default if blank would be filename uptodate|needs|updated|doesnotexist creation_date, ...
        !*! or maybe just list filenames so can pipe through xargs, and so on
        if(settings%list)then
            write(stderr,'(*(g0,1x))')'fpm::run<INFO>:test expected at',newwords(i),&
            & merge('exists        ','does not exist',exists(newwords(i)))
            cycle
        endif
        cmd=newwords(i) // ' ' // settings%args
        if(exists(newwords(i)))then
            call run(cmd)
        else ! try to build -- once build works conditionally this should be an unconditional call
            call cmd_build(fpm_build_settings(release=settings%release,list=.false.))
            if(exists(newwords(i)))then
                call run(cmd)
            else
                write(stderr,*)'fpm::run<ERROR>',cmd,' not found'
            endif
        endif
    enddo
    deallocate(newwords)
end subroutine cmd_test


end module fpm
