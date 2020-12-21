module fpm
use fpm_strings, only: string_t, operator(.in.)
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings, fpm_new_settings, &
                      fpm_run_settings, fpm_install_settings, fpm_test_settings
use fpm_dependency, only : new_dependency_tree
use fpm_environment, only: run
use fpm_filesystem, only: is_dir, join_path, number_of_rows, list_files, exists, basename
use fpm_model, only: fpm_model_t, srcfile_t, build_target_t, &
                    FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, FPM_SCOPE_DEP, &
                    FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST, &
                    FPM_TARGET_EXECUTABLE, FPM_TARGET_ARCHIVE
use fpm_compiler, only: add_compile_flag_defaults


use fpm_sources, only: add_executable_sources, add_sources_from_dir
use fpm_targets, only: targets_from_sources, resolve_module_dependencies, &
                        resolve_target_linking
use fpm_manifest, only : get_package_data, package_config_t
use fpm_error, only : error_t, fatal_error
use fpm_manifest_test, only : test_config_t
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
use fpm_manifest_dependency, only: dependency_config_t
implicit none
private
public :: cmd_build, cmd_run
public :: build_model

contains


subroutine build_model(model, settings, package, error)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_config_t), intent(in) :: package
    type(error_t), allocatable, intent(out) :: error

    integer :: i
    type(package_config_t) :: dependency
    character(len=:), allocatable :: manifest, lib_dir

    if(settings%verbose)then
       write(*,*)'<INFO>BUILD_NAME:',settings%build_name
       write(*,*)'<INFO>COMPILER:  ',settings%compiler
    endif

    model%package_name = package%name

    if (allocated(package%build%link)) then
        model%link_libraries = package%build%link
    else
        allocate(model%link_libraries(0))
    end if

    call new_dependency_tree(model%deps, cache=join_path("build", "cache.toml"))
    call model%deps%add(package, error)
    if (allocated(error)) return

    if(settings%compiler.eq.'')then
        model%fortran_compiler = 'gfortran'
    else
        model%fortran_compiler = settings%compiler
    endif

    model%output_directory = join_path('build',basename(model%fortran_compiler)//'_'//settings%build_name)

    call add_compile_flag_defaults(settings%build_name, basename(model%fortran_compiler), model)

    model%link_flags = ''

    allocate(model%packages(model%deps%ndep))

    ! Add sources from executable directories
    if (is_dir('app') .and. package%build%auto_executables) then
        call add_sources_from_dir(model%packages(1)%sources,'app', FPM_SCOPE_APP, &
                                   with_executables=.true., error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (is_dir('example') .and. package%build%auto_examples) then
        call add_sources_from_dir(model%packages(1)%sources,'example', FPM_SCOPE_EXAMPLE, &
                                   with_executables=.true., error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (is_dir('test') .and. package%build%auto_tests) then
        call add_sources_from_dir(model%packages(1)%sources,'test', FPM_SCOPE_TEST, &
                                   with_executables=.true., error=error)

        if (allocated(error)) then
            return
        endif

    end if
    if (allocated(package%executable)) then
        call add_executable_sources(model%packages(1)%sources, package%executable, FPM_SCOPE_APP, &
                                     auto_discover=package%build%auto_executables, &
                                     error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%example)) then
        call add_executable_sources(model%packages(1)%sources, package%example, FPM_SCOPE_EXAMPLE, &
                                     auto_discover=package%build%auto_examples, &
                                     error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%test)) then
        call add_executable_sources(model%packages(1)%sources, package%test, FPM_SCOPE_TEST, &
                                     auto_discover=package%build%auto_tests, &
                                     error=error)

        if (allocated(error)) then
            return
        endif

    endif

    do i = 1, model%deps%ndep
        associate(dep => model%deps%dep(i))
            manifest = join_path(dep%proj_dir, "fpm.toml")

            call get_package_data(dependency, manifest, error, &
                apply_defaults=.true.)
            if (allocated(error)) exit

            model%packages(i)%name = dependency%name

            if (allocated(dependency%library)) then
                lib_dir = join_path(dep%proj_dir, dependency%library%source_dir)
                call add_sources_from_dir(model%packages(i)%sources, lib_dir, FPM_SCOPE_LIB, &
                    error=error)
                if (allocated(error)) exit
            end if

            if (allocated(dependency%build%link)) then
                model%link_libraries = [model%link_libraries, dependency%build%link]
            end if
        end associate
    end do
    if (allocated(error)) return

    call targets_from_sources(model)

    do i = 1, size(model%link_libraries)
        model%link_flags = model%link_flags // " -l" // model%link_libraries(i)%s
    end do

    if (model%targets(1)%ptr%target_type == FPM_TARGET_ARCHIVE) then
        model%library_file = model%targets(1)%ptr%output_file
    end if

    call resolve_module_dependencies(model%targets,error)

    call resolve_target_linking(model%targets)

end subroutine build_model


subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_config_t) :: package
type(fpm_model_t) :: model
type(error_t), allocatable :: error

integer :: i

call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

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

subroutine cmd_run(settings,test)
    class(fpm_run_settings), intent(in) :: settings
    logical, intent(in) :: test

    integer, parameter :: LINE_WIDTH = 80
    integer :: i, j, col_width, nCol
    logical :: found(size(settings%name))
    type(error_t), allocatable :: error
    type(package_config_t) :: package
    type(fpm_model_t) :: model
    type(string_t) :: exe_cmd
    type(string_t), allocatable :: executables(:)
    type(build_target_t), pointer :: exe_target
    type(srcfile_t), pointer :: exe_source
    integer :: run_scope

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    if (allocated(error)) then
        print '(a)', error%message
        error stop 1
    end if

    call build_model(model, settings%fpm_build_settings, package, error)
    if (allocated(error)) then
        print '(a)', error%message
        error stop 1
    end if

    if (test) then
       run_scope = FPM_SCOPE_TEST
    else
       run_scope = merge(FPM_SCOPE_EXAMPLE, FPM_SCOPE_APP, settings%example)
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

            if (exe_source%unit_scope == run_scope) then

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

                if (exe_source%unit_scope == run_scope) then

                    write(stderr,'(A)',advance=(merge("yes","no ",modulo(j,nCol)==0))) &
                                        & [character(len=col_width) :: basename(exe_target%output_file)]
                    j = j + 1

                end if

            end if

        end do

        write(stderr,*)
        stop 1

    end if

    call build_package(model)

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
