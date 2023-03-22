module fpm
use fpm_strings, only: string_t, operator(.in.), glob, join, string_cat, &
                      lower, str_ends_with, is_fortran_name, str_begins_with_str, &
                      is_valid_module_name, len_trim
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings, fpm_new_settings, &
                      fpm_run_settings, fpm_install_settings, fpm_test_settings, &
                      fpm_clean_settings
use fpm_dependency, only : new_dependency_tree
use fpm_environment, only: get_env
use fpm_filesystem, only: is_dir, join_path, list_files, exists, &
                   basename, filewrite, mkdir, run, os_delete_dir
use fpm_model, only: fpm_model_t, srcfile_t, show_model, &
                    FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, FPM_SCOPE_DEP, &
                    FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST
use fpm_compiler, only: new_compiler, new_archiver, set_cpp_preprocessor_flags


use fpm_sources, only: add_executable_sources, add_sources_from_dir
use fpm_targets, only: targets_from_sources, &
                        resolve_target_linking, build_target_t, build_target_ptr, &
                        FPM_TARGET_EXECUTABLE, FPM_TARGET_ARCHIVE
use fpm_manifest, only : get_package_data, package_config_t
use fpm_error, only : error_t, fatal_error, fpm_stop
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
use iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_associated, c_f_pointer
implicit none
private
public :: cmd_build, cmd_run, cmd_clean
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
    character(len=:), allocatable :: manifest, lib_dir, flags, cflags, cxxflags, ldflags
    character(len=:), allocatable :: version
    logical :: has_cpp

    logical :: duplicates_found = .false.
    type(string_t) :: include_dir

    model%package_name = package%name

    allocate(model%include_dirs(0))
    allocate(model%link_libraries(0))
    allocate(model%external_modules(0))

    call new_dependency_tree(model%deps, cache=join_path("build", "cache.toml"))
    call model%deps%add(package, error)
    if (allocated(error)) return

    ! Update dependencies where needed
    call model%deps%update(error)
    if (allocated(error)) return

    ! build/ directory should now exist
    if (.not.exists("build/.gitignore")) then
      call filewrite(join_path("build", ".gitignore"),["*"])
    end if

    call new_compiler(model%compiler, settings%compiler, settings%c_compiler, &
        & settings%cxx_compiler, echo=settings%verbose, verbose=settings%verbose)
    call new_archiver(model%archiver, settings%archiver, &
        & echo=settings%verbose, verbose=settings%verbose)

    if (settings%flag == '') then
        flags = model%compiler%get_default_flags(settings%profile == "release")
    else
        flags = settings%flag
        select case(settings%profile)
        case("release", "debug")
            flags = flags // model%compiler%get_default_flags(settings%profile == "release")
        end select
    end if

    cflags = trim(settings%cflag)
    cxxflags = trim(settings%cxxflag)
    ldflags = trim(settings%ldflag)

    if (model%compiler%is_unknown()) then
        write(*, '(*(a:,1x))') &
            "<WARN>", "Unknown compiler", model%compiler%fc, "requested!", &
            "Defaults for this compiler might be incorrect"
    end if
    model%build_prefix = join_path("build", basename(model%compiler%fc))

    model%include_tests = settings%build_tests
    model%enforce_module_names = package%build%module_naming
    model%module_prefix = package%build%module_prefix

    allocate(model%packages(model%deps%ndep))

    has_cpp = .false.
    do i = 1, model%deps%ndep
        associate(dep => model%deps%dep(i))
            manifest = join_path(dep%proj_dir, "fpm.toml")

            call get_package_data(dependency, manifest, error, &
                apply_defaults=.true.)
            if (allocated(error)) exit

            model%packages(i)%name = dependency%name
            call package%version%to_string(version)
            model%packages(i)%version = version

            if (allocated(dependency%preprocess)) then
                do j = 1, size(dependency%preprocess)
                    if (dependency%preprocess(j)%name == "cpp") then
                        if (.not. has_cpp) has_cpp = .true.
                        if (allocated(dependency%preprocess(j)%macros)) then
                        model%packages(i)%macros = dependency%preprocess(j)%macros
                        end if
                    else
                        write(stderr, '(a)') 'Warning: Preprocessor ' // package%preprocess(i)%name // &
                            ' is not supported; will ignore it'
                    end if
                end do
            end if

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

            if (allocated(dependency%build%external_modules)) then
                model%external_modules = [model%external_modules, dependency%build%external_modules]
            end if

            ! Copy naming conventions from this dependency's manifest
            model%packages(i)%enforce_module_names = dependency%build%module_naming
            model%packages(i)%module_prefix        = dependency%build%module_prefix

        end associate
    end do
    if (allocated(error)) return

    if (has_cpp) call set_cpp_preprocessor_flags(model%compiler%id, flags)
    model%fortran_compile_flags = flags
    model%c_compile_flags = cflags
    model%cxx_compile_flags = cxxflags
    model%link_flags = ldflags

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


    if (settings%verbose) then
        write(*,*)'<INFO> BUILD_NAME: ',model%build_prefix
        write(*,*)'<INFO> COMPILER:  ',model%compiler%fc
        write(*,*)'<INFO> C COMPILER:  ',model%compiler%cc
        write(*,*)'<INFO> CXX COMPILER: ',model%compiler%cxx
        write(*,*)'<INFO> COMPILER OPTIONS:  ', model%fortran_compile_flags
        write(*,*)'<INFO> C COMPILER OPTIONS:  ', model%c_compile_flags
        write(*,*)'<INFO> CXX COMPILER OPTIONS: ', model%cxx_compile_flags
        write(*,*)'<INFO> LINKER OPTIONS:  ', model%link_flags
        write(*,*)'<INFO> INCLUDE DIRECTORIES:  [', string_cat(model%include_dirs,','),']'
    end if

    ! Check for invalid module names
    call check_module_names(model, error)
    if (allocated(error)) return

    ! Check for duplicate modules
    call check_modules_for_duplicates(model, duplicates_found)
    if (duplicates_found) then
        call fpm_stop(1,'*build_model*:Error: One or more duplicate module names found.')
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
              write(stderr, *) "Warning: Module ",model%packages(k)%sources(l)%modules_provided(m)%s, &
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

! Check names of all modules in this package and its dependencies
subroutine check_module_names(model, error)
    type(fpm_model_t), intent(in) :: model
    type(error_t), allocatable, intent(out) :: error
    integer :: i,j,k,l,m
    logical :: valid,errors_found,enforce_this_file
    type(string_t) :: package_name,module_name,package_prefix

    errors_found = .false.

    ! Loop through modules provided by each source file of every package
    ! Add it to the array if it is not already there
    ! Otherwise print out warning about duplicates
    do k=1,size(model%packages)

        package_name = string_t(model%packages(k)%name)

        ! Custom prefix is taken from each dependency's manifest
        if (model%packages(k)%enforce_module_names) then
            package_prefix = model%packages(k)%module_prefix
        else
            package_prefix = string_t("")
        end if

        ! Warn the user if some of the dependencies have loose naming
        if (model%enforce_module_names .and. .not.model%packages(k)%enforce_module_names) then
           write(stderr, *) "Warning: Dependency ",package_name%s // &
                            " does not enforce module naming, but project does. "
        end if

        do l=1,size(model%packages(k)%sources)

            ! Module naming is not enforced in test modules
            enforce_this_file =  model%enforce_module_names .and. &
                                 model%packages(k)%sources(l)%unit_scope/=FPM_SCOPE_TEST

            if (allocated(model%packages(k)%sources(l)%modules_provided)) then

                do m=1,size(model%packages(k)%sources(l)%modules_provided)

                    module_name = model%packages(k)%sources(l)%modules_provided(m)

                    valid = is_valid_module_name(module_name, &
                                                 package_name, &
                                                 package_prefix, &
                                                 enforce_this_file)

                    if (.not.valid) then

                        if (enforce_this_file) then

                            if (len_trim(package_prefix)>0) then

                            write(stderr, *) "ERROR: Module ",module_name%s, &
                                             " in ",model%packages(k)%sources(l)%file_name, &
                                             " does not match its package name ("//package_name%s// &
                                             ") or custom prefix ("//package_prefix%s//")."
                            else

                            write(stderr, *) "ERROR: Module ",module_name%s, &
                                             " in ",model%packages(k)%sources(l)%file_name, &
                                             " does not match its package name ("//package_name%s//")."

                            endif

                        else

                            write(stderr, *) "ERROR: Module ",module_name%s, &
                                             " in ",model%packages(k)%sources(l)%file_name, &
                                             " has an invalid Fortran name. "

                        end if

                        errors_found = .true.

                    end if
                end do
            end if
        end do
    end do

    if (errors_found) then

        if (model%enforce_module_names) &
            write(stderr, *) "       Hint: Try disabling module naming in the manifest: [build] module-naming=false . "

        call fatal_error(error,"The package contains invalid module names. "// &
                               "Naming conventions "//merge('are','not',model%enforce_module_names)// &
                               " being requested.")
    end if

end subroutine check_module_names

subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_config_t) :: package
type(fpm_model_t) :: model
type(build_target_ptr), allocatable :: targets(:)
type(error_t), allocatable :: error

integer :: i

call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build*:package error:'//error%message)
end if

call build_model(model, settings, package, error)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build*:model error:'//error%message)
end if

call targets_from_sources(targets, model, settings%prune, error)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build*:target error:'//error%message)
end if

if(settings%list)then
    do i=1,size(targets)
        write(stderr,*) targets(i)%ptr%output_file
    enddo
else if (settings%show_model) then
    call show_model(model)
else
    call build_package(targets,model,verbose=settings%verbose)
endif

end subroutine cmd_build

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
    integer :: run_scope,firsterror
    integer, allocatable :: stat(:)
    character(len=:),allocatable :: line
    logical :: toomany

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run*:package error:'//error%message)
    end if

    call build_model(model, settings%fpm_build_settings, package, error)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run*:model error:'//error%message)
    end if

    call targets_from_sources(targets, model, settings%prune, error)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run*:targets error:'//error%message)
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
            call fpm_stop(0,'No tests to run')
        else
            call fpm_stop(0,'No executables to run')
        end if
    end if

    ! Check all names are valid
    ! or no name and found more than one file
    toomany= size(settings%name)==0 .and. size(executables)>1
    if ( any(.not.found) &
    & .or. &
    & ( (toomany .and. .not.test) .or.  (toomany .and. settings%runner /= '') ) &
    & .and. &
    & .not.settings%list) then
        line=join(settings%name)
        if(line/='.')then ! do not report these special strings
           if(any(.not.found))then
              write(stderr,'(A)',advance="no")'<ERROR>*cmd_run*:specified names '
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

        if(line=='.' .or. line==' ')then ! do not report these special strings
           call fpm_stop(0,'')
        else
           call fpm_stop(1,'')
        endif

    end if

    call build_package(targets,model,verbose=settings%verbose)

    if (settings%list) then
         call compact_list()
    else

        allocate(stat(size(executables)))
        do i=1,size(executables)
            if (exists(executables(i)%s)) then
                if(settings%runner /= ' ')then
                    if(.not.allocated(settings%args))then
                       call run(settings%runner//' '//executables(i)%s, &
                             echo=settings%verbose, exitstat=stat(i))
                    else
                       call run(settings%runner//' '//executables(i)%s//" "//settings%args, &
                             echo=settings%verbose, exitstat=stat(i))
                    endif
                else
                    if(.not.allocated(settings%args))then
                       call run(executables(i)%s,echo=settings%verbose, exitstat=stat(i))
                    else
                       call run(executables(i)%s//" "//settings%args,echo=settings%verbose, &
                             exitstat=stat(i))
                    endif
                endif
            else
                call fpm_stop(1,'*cmd_run*:'//executables(i)%s//' not found')
            end if
        end do

        if (any(stat /= 0)) then
            do i=1,size(stat)
                if (stat(i) /= 0) then
                    write(stderr,'(*(g0:,1x))') '<ERROR> Execution for object "',basename(executables(i)%s),&
                                                '" returned exit code ',stat(i)
                end if
            end do
            firsterror = findloc(stat/=0,value=.true.,dim=1)
            call fpm_stop(stat(firsterror),'*cmd_run*:stopping due to failed executions')
        end if

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
                        & [character(len=col_width) :: basename(exe_target%output_file, suffix=.false.)]
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
                & [character(len=col_width) :: basename(executables(i)%s, suffix=.false.)]
            j = j + 1
        enddo
        write(stderr,*)
    end subroutine compact_list

end subroutine cmd_run

subroutine delete_skip(unix)
    !> delete directories in the build folder, skipping dependencies
    logical, intent(in) :: unix
    character(len=:), allocatable :: dir
    type(string_t), allocatable :: files(:)
    integer :: i
    call list_files('build', files, .false.)
    do i = 1, size(files)
        if (is_dir(files(i)%s)) then
            dir = files(i)%s
            if (.not.str_ends_with(dir,'dependencies')) call os_delete_dir(unix, dir)
        end if
    end do
end subroutine delete_skip

subroutine cmd_clean(settings)
    !> fpm clean called
    class(fpm_clean_settings), intent(in) :: settings
    ! character(len=:), allocatable :: dir
    ! type(string_t), allocatable :: files(:)
    character(len=1) :: response
    if (is_dir('build')) then
        ! remove the entire build directory
        if (settings%clean_call) then
            call os_delete_dir(settings%unix, 'build')
            return
        end if
        ! remove the build directory but skip dependencies
        if (settings%clean_skip) then
            call delete_skip(settings%unix)
            return
        end if
        ! prompt to remove the build directory but skip dependencies
        write(stdout, '(A)', advance='no') "Delete build, excluding dependencies (y/n)? "
        read(stdin, '(A1)') response
        if (lower(response) == 'y') call delete_skip(settings%unix)
    else
        write (stdout, '(A)') "fpm: No build directory found."
    end if
end subroutine cmd_clean

end module fpm
