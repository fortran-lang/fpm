module fpm
use fpm_strings, only: string_t, operator(.in.), glob, join, string_cat
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings, fpm_new_settings, &
                      fpm_run_settings, fpm_install_settings, fpm_test_settings
use fpm_dependency, only : new_dependency_tree
use fpm_environment, only: run
use fpm_filesystem, only: is_dir, join_path, number_of_rows, list_files, exists, basename
use fpm_model, only: fpm_model_t, srcfile_t, show_model, &
                    FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, FPM_SCOPE_DEP, &
                    FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST
use fpm_compiler, only: get_module_flags, is_unknown_compiler


use fpm_sources, only: add_executable_sources, add_sources_from_dir
use fpm_targets, only: targets_from_sources, resolve_module_dependencies, &
                        resolve_target_linking, build_target_t, build_target_ptr, &
                        FPM_TARGET_EXECUTABLE, FPM_TARGET_ARCHIVE
use fpm_manifest, only : get_package_data, package_config_t
use fpm_error, only : error_t, fatal_error
use fpm_manifest_test, only : test_config_t
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
use fpm_manifest_dependency, only: dependency_config_t
use, intrinsic :: iso_fortran_env, only: error_unit
implicit none
private
public :: cmd_build, cmd_run
public :: build_model, check_modules_for_duplicates

contains


subroutine build_model(model, settings, package, error)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_config_t), intent(in) :: package
    type(error_t), allocatable, intent(out) :: error

    integer :: i, j
    type(package_config_t) :: dependency
    character(len=:), allocatable :: manifest, lib_dir

    logical :: duplicates_found = .false.
    type(string_t) :: include_dir

    model%package_name = package%name

    allocate(model%include_dirs(0))
    allocate(model%link_libraries(0))

    call new_dependency_tree(model%deps, cache=join_path("build", "cache.toml"))
    call model%deps%add(package, error)
    if (allocated(error)) return

    if(settings%compiler.eq.'')then
        model%fortran_compiler = 'gfortran'
    else
        model%fortran_compiler = settings%compiler
    endif

    if (is_unknown_compiler(model%fortran_compiler)) then
        write(*, '(*(a:,1x))') &
            "<WARN>", "Unknown compiler", model%fortran_compiler, "requested!", &
            "Defaults for this compiler might be incorrect"
    end if
    model%output_directory = join_path('build',basename(model%fortran_compiler)//'_'//settings%build_name)

    call get_module_flags(model%fortran_compiler, &
        & join_path(model%output_directory,model%package_name), &
        & model%fortran_compile_flags)
    model%fortran_compile_flags = settings%flag // model%fortran_compile_flags

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
            if (.not.allocated(model%packages(i)%sources)) allocate(model%packages(i)%sources(0))

            if (allocated(dependency%library)) then
                
                if (allocated(dependency%library%source_dir)) then
                    lib_dir = join_path(dep%proj_dir, dependency%library%source_dir)
                    if (is_dir(lib_dir)) then
                        call add_sources_from_dir(model%packages(i)%sources, lib_dir, FPM_SCOPE_LIB, &
                            error=error)
                        if (allocated(error)) exit
                    end if
                end if

                if (allocated(dependency%library%include_dir)) then
                    do j=1,size(dependency%library%include_dir)
                        include_dir%s = join_path(dep%proj_dir, dependency%library%include_dir(j)%s)
                        if (is_dir(include_dir%s)) then
                            model%include_dirs = [model%include_dirs, include_dir]
                        end if
                    end do
                end if
                
            end if

            if (allocated(dependency%build%link)) then
                model%link_libraries = [model%link_libraries, dependency%build%link]
            end if
        end associate
    end do
    if (allocated(error)) return

    if (settings%verbose) then
        write(*,*)'<INFO> BUILD_NAME: ',settings%build_name
        write(*,*)'<INFO> COMPILER:  ',settings%compiler
        write(*,*)'<INFO> COMPILER OPTIONS:  ', model%fortran_compile_flags 
        write(*,*)'<INFO> INCLUDE DIRECTORIES:  [', string_cat(model%include_dirs,','),']' 
     end if

    ! Check for duplicate modules
    call check_modules_for_duplicates(model, duplicates_found)
    if (duplicates_found) then
      error stop 'Error: One or more duplicate module names found.'
    end if
end subroutine build_model

! Check for duplicate modules
subroutine check_modules_for_duplicates(model, duplicates_found) 
    type(fpm_model_t), intent(in) :: model
    integer :: maxsize
    integer :: i,j,k,l,m,modi
    type(string_t), allocatable :: modules(:)
    logical :: duplicates_found
    ! Initialise the size of array
    maxsize = 0
    ! Get number of modules provided by each source file of every package
    do i=1,size(model%packages)
      do j=1,size(model%packages(i)%sources)
        if (allocated(model%packages(i)%sources(j)%modules_provided)) then
          maxsize = maxsize + size(model%packages(i)%sources(j)%modules_provided)
        end if
      end do
    end do
    ! Allocate array to contain distinct names of modules
    allocate(modules(maxsize))

    ! Initialise index to point at start of the newly allocated array
    modi = 1

    ! Loop through modules provided by each source file of every package
    ! Add it to the array if it is not already there
    ! Otherwise print out warning about duplicates
    do k=1,size(model%packages)
      do l=1,size(model%packages(k)%sources)
        if (allocated(model%packages(k)%sources(l)%modules_provided)) then
          do m=1,size(model%packages(k)%sources(l)%modules_provided)
            if (model%packages(k)%sources(l)%modules_provided(m)%s.in.modules(:modi-1)) then
              write(error_unit, *) "Warning: Module ",model%packages(k)%sources(l)%modules_provided(m)%s, &
                " in ",model%packages(k)%sources(l)%file_name," is a duplicate"
              duplicates_found = .true.
            else
              modules(modi) = model%packages(k)%sources(l)%modules_provided(m)
              modi = modi + 1
            end if
          end do
        end if
      end do
    end do
end subroutine check_modules_for_duplicates

subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_config_t) :: package
type(fpm_model_t) :: model
type(build_target_ptr), allocatable :: targets(:)
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

call targets_from_sources(targets,model,error)
if (allocated(error)) then
    print '(a)', error%message
    error stop 1
end if

if(settings%list)then
    do i=1,size(targets)
        write(stderr,*) targets(i)%ptr%output_file
    enddo
else if (settings%show_model) then
    call show_model(model)
else
    call build_package(targets,model)
endif

end subroutine

subroutine cmd_run(settings,test)
    class(fpm_run_settings), intent(in) :: settings
    logical, intent(in) :: test

    integer :: i, j, col_width
    logical :: found(size(settings%name))
    type(error_t), allocatable :: error
    type(package_config_t) :: package
    type(fpm_model_t) :: model
    type(build_target_ptr), allocatable :: targets(:)
    type(string_t) :: exe_cmd
    type(string_t), allocatable :: executables(:)
    type(build_target_t), pointer :: exe_target
    type(srcfile_t), pointer :: exe_source
    integer :: run_scope
    character(len=:),allocatable :: line
    logical :: toomany

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

    call targets_from_sources(targets,model,error)
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
    do i=1,size(targets)

        exe_target => targets(i)%ptr

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

                        if (glob(trim(exe_source%exe_name),trim(settings%name(j)))) then

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
    ! or no name and found more than one file
    toomany= size(settings%name).eq.0 .and. size(executables).gt.1 
    if ( any(.not.found) &
    & .or. &
    & ( (toomany .and. .not.test) .or.  (toomany .and. settings%runner .ne. '') ) &
    & .and. &
    & .not.settings%list) then
        line=join(settings%name)
        if(line.ne.'.')then ! do not report these special strings
           if(any(.not.found))then
              write(stderr,'(A)',advance="no")'fpm::run<ERROR> specified names '
              do j=1,size(settings%name)
                  if (.not.found(j)) write(stderr,'(A)',advance="no") '"'//trim(settings%name(j))//'" '
              end do
              write(stderr,'(A)') 'not found.'
              write(stderr,*)
           else if(settings%verbose)then
              write(stderr,'(A)',advance="yes")'<INFO>when more than one executable is available'
              write(stderr,'(A)',advance="yes")'      program names must be specified.'
           endif
        endif

        call compact_list_all()

        if(line.eq.'.' .or. line.eq.' ')then ! do not report these special strings
           stop
        else
           stop 1
        endif

    end if

    call build_package(targets,model)

    if (settings%list) then
         call compact_list()
    else

        do i=1,size(executables)
            if (exists(executables(i)%s)) then
                if(settings%runner .ne. ' ')then
                    call run(settings%runner//' '//executables(i)%s//" "//settings%args,echo=settings%verbose)
                else
                    call run(executables(i)%s//" "//settings%args,echo=settings%verbose)
                endif
            else
                write(stderr,*)'fpm::run<ERROR>',executables(i)%s,' not found'
                stop 1
            end if
        end do
    endif
    contains 
    subroutine compact_list_all()
    integer, parameter :: LINE_WIDTH = 80
    integer :: i, j, nCol
        j = 1
        nCol = LINE_WIDTH/col_width
        write(stderr,*) 'Available names:'
        do i=1,size(targets)

            exe_target => targets(i)%ptr

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
    end subroutine compact_list_all

    subroutine compact_list()
    integer, parameter :: LINE_WIDTH = 80
    integer :: i, j, nCol
        j = 1
        nCol = LINE_WIDTH/col_width
        write(stderr,*) 'Matched names:'
        do i=1,size(executables)
            write(stderr,'(A)',advance=(merge("yes","no ",modulo(j,nCol)==0))) &
             & [character(len=col_width) :: basename(executables(i)%s)]
            j = j + 1
        enddo
        write(stderr,*)
    end subroutine compact_list

end subroutine cmd_run

end module fpm
