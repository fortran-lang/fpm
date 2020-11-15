module fpm
use fpm_strings, only: string_t, str_ends_with, operator(.in.)
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings, fpm_new_settings, &
                      fpm_run_settings, fpm_install_settings, fpm_test_settings
use fpm_environment, only: run
use fpm_filesystem, only: is_dir, join_path, number_of_rows, list_files, exists, basename
use fpm_model, only: fpm_model_t, srcfile_t, build_target_t, &
                    FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
                    FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST, &
                    FPM_TARGET_EXECUTABLE

use fpm_sources, only: add_executable_sources, add_sources_from_dir
use fpm_targets, only: targets_from_sources, resolve_module_dependencies
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
public :: cmd_build, cmd_install, cmd_run

contains


recursive subroutine add_libsources_from_package(sources,link_libraries,package_list,package, &
                                                  package_root,dev_depends,error)
    ! Discover library sources in a package, recursively including dependencies
    !
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    type(string_t), allocatable, intent(inout) :: link_libraries(:)
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

            
            call add_libsources_from_package(sources,link_libraries,package_list,dependency, &
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
            if (allocated(dependency%build_config%link)) then
                link_libraries = [link_libraries, dependency%build_config%link]
            end if

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
    if (allocated(package%build_config%link)) then
        model%link_libraries = package%build_config%link
    else
        allocate(model%link_libraries(0))
    end if

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
    call add_libsources_from_package(model%sources,model%link_libraries,package_list,package, &
                                      package_root='.',dev_depends=.true.,error=error)
    if (allocated(error)) then
        return
    end if

    call targets_from_sources(model,model%sources)

    do i = 1, size(model%link_libraries)
        model%link_flags = model%link_flags // " -l" // model%link_libraries(i)%s
    end do

    call resolve_module_dependencies(model%targets,error)

end subroutine build_model

!> Apply package defaults
subroutine package_defaults(package)
    type(package_t), intent(inout) :: package

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

    ! Populate test in case we find the default test directory
    if (.not.allocated(package%test) .and. &
         exists(join_path("test","main.f90"))) then
        allocate(package%test(1))
        call default_test(package%test(1), package%name)
    endif

    if (.not.(allocated(package%library) .or. allocated(package%executable))) then
        print '(a)', "Neither library nor executable found, there is nothing to do"
        error stop 1
    end if

end subroutine

subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_t) :: package
type(fpm_model_t) :: model
type(error_t), allocatable :: error

integer :: i

call get_package_data(package, "fpm.toml", error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

call package_defaults(package)

call build_model(model, settings, package, error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

if(settings%list)then
    do i=1,size(model%targets)
        write(stderr,*) model%targets(i)%ptr%output_file
    enddo
else
    call build_package(model)
endif

end subroutine

subroutine cmd_install(settings)
type(fpm_install_settings), intent(in) :: settings
    print *, "fpm error: 'fpm install' not implemented."
    error stop 8
end subroutine cmd_install

subroutine cmd_run(settings,test)
    class(fpm_run_settings), intent(in) :: settings
    logical, intent(in) :: test

    integer, parameter :: LINE_WIDTH = 80
    integer :: i, j, col_width, nCol
    logical :: found(size(settings%name))
    type(error_t), allocatable :: error
    type(package_t) :: package
    type(fpm_model_t) :: model
    type(string_t) :: exe_cmd
    type(string_t), allocatable :: executables(:)
    type(build_target_t), pointer :: exe_target
    type(srcfile_t), pointer :: exe_source

    call get_package_data(package, "fpm.toml", error)
    if (allocated(error)) then
        print '(a)', error%message
        error stop 1
    end if


    call package_defaults(package)

    call build_model(model, settings%fpm_build_settings, package, error)
    if (allocated(error)) then
        print '(a)', error%message
        error stop 1
    end if

    ! Enumerate executable targets to run
    col_width = -1
    found(:) = .false.
    allocate(executables(0))
    do i=1,size(model%targets)

        exe_target => model%targets(i)%ptr

        if (exe_target%target_type == FPM_TARGET_EXECUTABLE .and. &
             allocated(exe_target%dependencies)) then

            exe_source => exe_target%dependencies(1)%ptr%source

            if (exe_source%unit_scope == &
                merge(FPM_SCOPE_TEST,FPM_SCOPE_APP,test)) then 

                col_width = max(col_width,len(basename(exe_target%output_file))+2)

                if (size(settings%name) == 0) then

                    exe_cmd%s = exe_target%output_file
                    executables = [executables, exe_cmd]

                else

                    do j=1,size(settings%name)

                        if (trim(settings%name(j))==exe_source%exe_name) then
                            
                            found(j) = .true.
                            exe_cmd%s = exe_target%output_file
                            executables = [executables, exe_cmd]

                        end if

                    end do

                end if
                
            end if

        end if

    end do

    ! Check if any apps/tests were found
    if (col_width < 0) then
        if (test) then
            write(stderr,*) 'No tests to run'
        else
            write(stderr,*) 'No executables to run'
        end if
        stop
    end if

    ! Check all names are valid
    if (any(.not.found)) then

        write(stderr,'(A)',advance="no")'fpm::run<ERROR> specified names '
        do j=1,size(settings%name)
            if (.not.found(j)) write(stderr,'(A)',advance="no") '"'//trim(settings%name(j))//'" '
        end do
        write(stderr,'(A)') 'not found.'
        write(stderr,*)

        j = 1
        nCol = LINE_WIDTH/col_width
        write(stderr,*) 'Available names:'
        do i=1,size(model%targets)

            exe_target => model%targets(i)%ptr
    
            if (exe_target%target_type == FPM_TARGET_EXECUTABLE .and. &
                allocated(exe_target%dependencies)) then

                exe_source => exe_target%dependencies(1)%ptr%source

                if (exe_source%unit_scope == &
                    merge(FPM_SCOPE_TEST,FPM_SCOPE_APP,test)) then 

                    write(stderr,'(A)',advance=(merge("yes","no ",modulo(j,nCol)==0))) &
                                        & [character(len=col_width) :: basename(exe_target%output_file)]
                    j = j + 1

                end if

            end if

        end do

        write(stderr,*)
        stop 1

    end if

    ! NB. To be replaced after incremental rebuild is implemented
    if (.not.settings%list .and. &
         any([(.not.exists(executables(i)%s),i=1,size(executables))])) then

        call build_package(model)

    end if

    do i=1,size(executables)
        if (settings%list) then
            write(stderr,*) executables(i)%s
        else
            
            if (exists(executables(i)%s)) then
	        if(settings%runner .ne. ' ')then
                   call run(settings%runner//' '//executables(i)%s//" "//settings%args)
		else
                   call run(executables(i)%s//" "//settings%args)
		endif
            else
                write(stderr,*)'fpm::run<ERROR>',executables(i)%s,' not found'
                stop 1
            end if

        end if
    end do

end subroutine cmd_run

end module fpm
