module fpm
use fpm_strings, only: string_t, operator(.in.), glob, join, string_cat, &
                      lower, str_ends_with, is_fortran_name, str_begins_with_str, &
                      is_valid_module_name, len_trim
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings, fpm_new_settings, &
                      fpm_run_settings, fpm_install_settings, fpm_test_settings, &
                      fpm_clean_settings
use fpm_dependency, only : new_dependency_tree
use fpm_filesystem, only: is_dir, join_path, list_files, exists, &
                   basename, filewrite, mkdir, run, os_delete_dir, delete_file
use fpm_model, only: fpm_model_t, srcfile_t, show_model, &
                    FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, FPM_SCOPE_DEP, &
                    FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST
use fpm_compiler, only: new_compiler, new_archiver, set_cpp_preprocessor_flags, &
                        id_intel_classic_nix,id_intel_classic_mac,id_intel_llvm_nix, &
                        id_intel_llvm_unknown


use fpm_sources, only: add_executable_sources, add_sources_from_dir
use fpm_targets, only: targets_from_sources, build_target_t, build_target_ptr, &
                        FPM_TARGET_EXECUTABLE, get_library_dirs, filter_executable_targets
use fpm_manifest, only : get_package_data, package_config_t
use fpm_manifest_platform, only: platform_config_t
use fpm_meta, only : resolve_metapackages
use fpm_error, only : error_t, fatal_error, fpm_stop
use fpm_toml, only: name_is_json
use, intrinsic :: iso_fortran_env, only : stdin => input_unit, &
                                        & stdout => output_unit, &
                                        & stderr => error_unit
use iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_associated, c_f_pointer
use fpm_environment, only: os_is_unix, get_os_type, OS_WINDOWS, OS_MACOS, get_env, set_env, delete_env
use fpm_settings, only: fpm_global_settings, get_global_settings

implicit none
private
public :: cmd_build, cmd_run, cmd_clean
public :: build_model, check_modules_for_duplicates, new_compiler_flags

contains

!> Constructs a valid fpm model from command line settings and the toml manifest.
subroutine build_model(model, settings, package_config, error)
    type(fpm_model_t), intent(out) :: model
    class(fpm_build_settings), intent(inout) :: settings
    type(package_config_t), intent(inout), target :: package_config
    type(error_t), allocatable, intent(out) :: error

    integer :: i, j
    type(package_config_t), allocatable, target  :: package, dependency_config, dependency
    type(package_config_t), pointer :: manifest
    type(platform_config_t), allocatable, target :: target_platform
    character(len=:), allocatable :: file_name, lib_dir
    logical :: has_cpp
    logical :: duplicates_found, auto_exe, auto_example, auto_test
    type(string_t) :: include_dir
    
    ! Large variables -> safer on heap
    allocate(package,dependency_config,dependency,target_platform)

    model%package_name = package_config%name

    ! Set target OS to current OS (may be extended for cross-compilation in the future)
    model%target_os = get_os_type()

    allocate(model%include_dirs(0))
    allocate(model%link_libraries(0))
    allocate(model%external_modules(0))
    
    call new_compiler(model%compiler, settings%compiler, settings%c_compiler, &
        & settings%cxx_compiler, echo=settings%verbose, verbose=settings%verbose)
    call new_archiver(model%archiver, settings%archiver, &
        & echo=settings%verbose, verbose=settings%verbose)

    if (model%compiler%is_unknown()) then
        write(*, '(*(a:,1x))') &
            "<WARN>", "Unknown compiler", model%compiler%fc, "requested!", &
            "Defaults for this compiler might be incorrect"
    end if
    
    ! Extract the target platform for this build
    target_platform = model%target_platform()

    model%build_dir            = settings%build_dir
    model%build_prefix         = join_path(settings%build_dir, basename(model%compiler%fc))
    model%include_tests        = settings%build_tests

    ! Extract the current package configuration request
    package = package_config%export_config(target_platform,settings%features,settings%profile, &
                                           settings%verbose,error)
    if (allocated(error)) return

    ! Print enabled features/profile in verbose mode
    if (settings%verbose) then
        if (allocated(settings%features)) then  
            if (size(settings%features)>0) print *, '+ features: [', string_cat(settings%features,','),']'
        end if
        if (allocated(settings%profile)) then
            print *, '+ profile: ', settings%profile
        end if
    end if

    ! Initialize compiler flags using the feature-enabled package configuration
    call new_compiler_flags(model, settings, package)

    ! Resolve meta-dependencies into the package and the model
    ! This must happen BEFORE the dependency tree is built, because metapackages
    ! add dependencies (e.g., stdlib adds stdlib and test-drive as dev-dependencies)
    call resolve_metapackages(model,package,settings,error)
    if (allocated(error)) return

    if (allocated(package%build)) then
        model%enforce_module_names = package%build%module_naming
        model%module_prefix        = package%build%module_prefix
    endif

    ! Create dependencies
    call new_dependency_tree(model%deps, cache=join_path(settings%build_dir, "cache.toml"), &
    & path_to_config=settings%path_to_config, build_dir=settings%build_dir)

    ! Build and resolve model dependencies
    call model%deps%add(package, error)
    if (allocated(error)) return

    ! Update dependencies where needed
    call model%deps%update(error)
    if (allocated(error)) return

    ! Resolve metapackages from dependencies (if any)
    ! This second call merges metapackage requests from all resolved dependencies
    call resolve_metapackages(model,package,settings,error)
    if (allocated(error)) return

    ! build directory should now exist
    if (.not.exists(join_path(settings%build_dir, ".gitignore"))) then
      call filewrite(join_path(settings%build_dir, ".gitignore"),["*"])
    end if

    allocate(model%packages(model%deps%ndep))
    
    ! The current configuration may not have preprocessing, but some of its features may. 
    ! This means there will be directives that need to be considered even if not currently 
    ! active. Turn preprocessing on even in this case
    has_cpp = package_config%has_cpp() .or. package%has_cpp()

    do i = 1, model%deps%ndep
        associate(dep => model%deps%dep(i))
            file_name = join_path(dep%proj_dir, "fpm.toml")

            ! The main package manifest should not be reloaded, because it may have been 
            ! affected by model dependencies and metapackages
            if (i==1) then 
                manifest => package
            else
                
                ! Extract this dependency config
                call get_package_data(dependency_config, file_name, error, apply_defaults=.true.)
                if (allocated(error)) exit          
                
                ! Adapt it to the current profile/platform
                dependency = dependency_config%export_config(target_platform, &
                                                             dep%features,dep%profile,verbose=.false.,error=error)
                if (allocated(error)) exit
                
                manifest => dependency
            end if            
            

            model%packages(i)%name     = manifest%name
            model%packages(i)%features = manifest%fortran
            model%packages(i)%version  = manifest%version

            !> Add this dependency's manifest macros
            if (allocated(manifest%preprocess)) then
                do j = 1, size(manifest%preprocess)
                    call model%packages(i)%preprocess%add_config(manifest%preprocess(j))
                end do
            end if

            !> Add this dependency's package-level macros
            if (allocated(dep%preprocess)) then
                do j = 1, size(dep%preprocess)
                    call model%packages(i)%preprocess%add_config(dep%preprocess(j))
                end do
            end if

            if (model%packages(i)%preprocess%is_cpp()) has_cpp = .true.

            if (.not.allocated(model%packages(i)%sources)) allocate(model%packages(i)%sources(0))

            if (allocated(manifest%library)) then

                if (allocated(manifest%library%source_dir)) then
                    lib_dir = join_path(dep%proj_dir, manifest%library%source_dir)
                    if (is_dir(lib_dir)) then
                        call add_sources_from_dir(model%packages(i)%sources, lib_dir, FPM_SCOPE_LIB, &
                            with_f_ext=model%packages(i)%preprocess%suffixes, error=error, &
                            preprocess=model%packages(i)%preprocess)
                        if (allocated(error)) exit
                    end if
                end if

                if (allocated(manifest%library%include_dir)) then
                    do j=1,size(manifest%library%include_dir)
                        include_dir%s = join_path(dep%proj_dir, manifest%library%include_dir(j)%s)
                        if (is_dir(include_dir%s)) then
                            model%include_dirs = [model%include_dirs, include_dir]
                        end if
                    end do
                end if

            end if
            
            if (allocated(manifest%build)) then 

                if (allocated(manifest%build%link)) then
                    model%link_libraries = [model%link_libraries, manifest%build%link]
                end if

                if (allocated(manifest%build%external_modules)) then
                    model%external_modules = [model%external_modules, manifest%build%external_modules]
                end if

                ! Copy naming conventions from this dependency's manifest
                model%packages(i)%enforce_module_names = manifest%build%module_naming
                model%packages(i)%module_prefix        = manifest%build%module_prefix
            
            endif

        end associate
    end do
    if (allocated(error)) return

    ! Add optional flags
    if (has_cpp) call set_cpp_preprocessor_flags(model%compiler%id, model%fortran_compile_flags)

    ! Add sources from executable directories
    
    if (allocated(package%build)) then 
        auto_exe = package%build%auto_executables
        auto_example = package%build%auto_examples
        auto_test = package%build%auto_tests
    else
        auto_exe = .true.
        auto_example = .true.
        auto_test = .true.
    endif
    
    if (is_dir('app') .and. auto_exe) then
        call add_sources_from_dir(model%packages(1)%sources,'app', FPM_SCOPE_APP, &
                                   with_executables=.true., with_f_ext=model%packages(1)%preprocess%suffixes,&
                                   error=error,preprocess=model%packages(1)%preprocess)

        if (allocated(error)) then
            return
        end if

    end if
    if (is_dir('example') .and. auto_example) then
        call add_sources_from_dir(model%packages(1)%sources,'example', FPM_SCOPE_EXAMPLE, &
                                  with_executables=.true., &
                                  with_f_ext=model%packages(1)%preprocess%suffixes,error=error,&
                                  preprocess=model%packages(1)%preprocess)

        if (allocated(error)) then
            return
        end if

    end if
    if (is_dir('test') .and. auto_test) then
        call add_sources_from_dir(model%packages(1)%sources,'test', FPM_SCOPE_TEST, &
                                  with_executables=.true., &
                                  with_f_ext=model%packages(1)%preprocess%suffixes,error=error,&
                                  preprocess=model%packages(1)%preprocess)

        if (allocated(error)) then
            return
        endif

    end if
    if (allocated(package%executable)) then
        call add_executable_sources(model%packages(1)%sources, package%executable, FPM_SCOPE_APP, &
                                     auto_discover=auto_exe, &
                                     with_f_ext=model%packages(1)%preprocess%suffixes, &
                                     error=error,preprocess=model%packages(1)%preprocess)

        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%example)) then
        call add_executable_sources(model%packages(1)%sources, package%example, FPM_SCOPE_EXAMPLE, &
                                     auto_discover=auto_example, &
                                     with_f_ext=model%packages(1)%preprocess%suffixes, &
                                     error=error,preprocess=model%packages(1)%preprocess)

        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%test)) then
        call add_executable_sources(model%packages(1)%sources, package%test, FPM_SCOPE_TEST, &
                                     auto_discover=auto_test, &
                                     with_f_ext=model%packages(1)%preprocess%suffixes, &
                                     error=error,preprocess=model%packages(1)%preprocess)

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
    duplicates_found = .false.
    call check_modules_for_duplicates(model, duplicates_found)
    if (duplicates_found) then
        call fpm_stop(1,'*build_model*:Error: One or more duplicate module names found.')
    end if
end subroutine build_model

!> Helper: safely get string from either CLI or package, with fallback
pure function assemble_flags(cli_flag, package_flag, fallback) result(flags)
    character(len=*), optional, intent(in) :: cli_flag, package_flag, fallback
    character(len=:), allocatable :: flags
    
    allocate(character(len=0) :: flags)

    if (present(cli_flag))     flags = flags // ' ' // trim(cli_flag)
    if (present(package_flag)) flags = flags // ' ' // trim(package_flag)
    if (present(fallback))     flags = flags // ' ' // trim(fallback)

end function assemble_flags

!> Initialize model compiler flags from CLI settings and package configuration
subroutine new_compiler_flags(model, settings, package)
    type(fpm_model_t), intent(inout) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_config_t), intent(in) :: package

    logical :: release_request, debug_request, need_defaults
    character(len=:), allocatable :: fallback
    
    ! Default: "debug" if not requested
    release_request = .false.
    debug_request   = .not.allocated(settings%profile)
    if (allocated(settings%profile)) release_request = settings%profile == "release"
    if (allocated(settings%profile)) debug_request   = settings%profile == "debug"    
    
    need_defaults = release_request .or. debug_request
    
    ! Backward-compatible: if debug/release requested, but a user-defined profile is not defined,
    ! apply fpm compiler defaults
    if (need_defaults) then 
        
        need_defaults   =      (release_request .and. package%find_profile("release")<=0) &
                          .or. (debug_request .and. package%find_profile("debug")<=0)
        
    end if
    
    ! Fix: Always include compiler default flags for Intel ifx -fPIC issue
    if (need_defaults) then 
        
        fallback = model%compiler%get_default_flags(release_request)
        
    elseif (any(model%compiler%id==[id_intel_classic_mac, &
                                    id_intel_classic_nix, &
                                    id_intel_llvm_nix, &
                                    id_intel_llvm_unknown])) then

        ! Intel compilers need -fPIC for shared libraries (except Windows)
        fallback = " -fPIC"
        
    else
        
        if (allocated(fallback)) deallocate(fallback) ! trigger .not.present
        
    endif
        
    model%fortran_compile_flags = assemble_flags(settings%flag,    package%flags, fallback)
    model%c_compile_flags       = assemble_flags(settings%cflag,   package%c_flags)
    model%cxx_compile_flags     = assemble_flags(settings%cxxflag, package%cxx_flags)
    model%link_flags            = assemble_flags(settings%ldflag,  package%link_time_flags)

end subroutine new_compiler_flags

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
    integer :: k,l,m
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
type(fpm_build_settings), intent(inout) :: settings

type(package_config_t), allocatable :: package
type(fpm_model_t), allocatable :: model
type(build_target_ptr), allocatable :: targets(:)
type(error_t), allocatable :: error

integer :: i

! Large variables -> safer on heap
allocate(package, model)

call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build* Package error: '//error%message)
end if

call build_model(model, settings, package, error)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build* Model error: '//error%message)
end if

call targets_from_sources(targets, model, settings%prune, package%library, error)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build* Target error: '//error%message)
end if

!> Dump model to file
if (len_trim(settings%dump)>0) then
    call model%dump(trim(settings%dump),error,json=name_is_json(trim(settings%dump)))
    if (allocated(error)) call fpm_stop(1,'*cmd_build* Model dump error: '//error%message)
endif

if(settings%list)then
    do i=1,size(targets)
        write(stderr,*) targets(i)%ptr%output_file
    enddo
endif
if (settings%show_model) then
    call show_model(model)
else
    call build_package(targets,model,verbose=settings%verbose,dry_run=settings%list)
endif

end subroutine cmd_build

subroutine cmd_run(settings,test)
    class(fpm_run_settings), intent(inout) :: settings
    logical, intent(in) :: test

    integer :: i, j, col_width
    logical :: found(size(settings%name))
    type(error_t), allocatable :: error
    type(package_config_t), allocatable :: package
    type(fpm_model_t), allocatable :: model
    type(build_target_ptr), allocatable :: targets(:)
    type(string_t) :: exe_cmd
    type(string_t), allocatable :: executables(:)
    type(build_target_t), pointer :: exe_target
    type(srcfile_t), pointer :: exe_source
    integer :: run_scope,firsterror
    integer, allocatable :: stat(:),target_ID(:)
    character(len=:),allocatable :: line,run_cmd,library_path
    
    ! Large variables -> safer on heap
    allocate(package,model)

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run* Package error: '//error%message)
    end if

    call build_model(model, settings, package, error)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run* Model error: '//error%message)
    end if

    call targets_from_sources(targets, model, settings%prune, package%library, error)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run* Targets error: '//error%message)
    end if

    if (test) then
       run_scope = FPM_SCOPE_TEST
    else
       run_scope = merge(FPM_SCOPE_EXAMPLE, FPM_SCOPE_APP, settings%example)
    end if

    ! Enumerate executable targets to run
    col_width = -1
    found(:) = .false.
    allocate(executables(size(targets)),target_ID(size(targets)))
    enumerate: do i=1,size(targets)
        exe_target => targets(i)%ptr
        if (should_be_run(settings,run_scope,exe_target)) then  
            
            exe_source => exe_target%dependencies(1)%ptr%source
                
            col_width = max(col_width,len(basename(exe_target%output_file))+2)
            
            ! Priority by name ID, or 0 if no name present (run first)
            j              = settings%name_ID(exe_source%exe_name)
            target_ID(i)   = j
            if (j>0) found(j) = .true.
            
            exe_cmd%s      = exe_target%output_file
            executables(i) = exe_cmd
            
        else
            target_ID(i)   = huge(target_ID(i))
        endif
    end do enumerate
    
    ! sort executables by ascending name ID, resize
    call sort_executables(target_ID,executables)
    
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
    if ( any(.not.found) ) then
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

    call build_package(targets,model,verbose=settings%verbose,dry_run=settings%list)

    if (settings%list) then
         call compact_list()
    else

        ! Save current library path and set a new one that includes the local 
        ! dynamic library folders
        library_path = save_library_path()                
        call set_library_path(model, targets, error)
        if (allocated(error)) call fpm_stop(1, '*cmd_run* Run error: '//error%message)

        allocate(stat(size(executables)))
        do i=1,size(executables)
            if (exists(executables(i)%s)) then
                
                ! Prepare command line
                                              run_cmd = executables(i)%s
                if (settings%runner/=' ')     run_cmd = settings%runner_command()//' '//run_cmd
                if (allocated(settings%args)) run_cmd = run_cmd//" "//settings%args
                
                ! System Integrity Protection will not propagate the .dylib environment variables
                ! to the child process: add paths manually
                if (get_os_type()==OS_MACOS)  run_cmd = "env DYLD_LIBRARY_PATH=" // &
                                                         get_env("DYLD_LIBRARY_PATH","") // &
                                                         " " // run_cmd

                call run(run_cmd,echo=settings%verbose,exitstat=stat(i))                
                
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
        
        ! Restore original library path
        call restore_library_path(library_path, error)
        if (allocated(error)) call fpm_stop(1, '*cmd_run* Environment error: '//error%message)                         

    end if

    contains

    subroutine compact_list_all()
    integer, parameter :: LINE_WIDTH = 80
    integer :: ii, jj, nCol
        jj = 1
        nCol = LINE_WIDTH/col_width
        write(stderr,*) 'Available names:'
        do ii=1,size(targets)

            exe_target => targets(ii)%ptr

            if (exe_target%target_type == FPM_TARGET_EXECUTABLE .and. &
                allocated(exe_target%dependencies)) then

                exe_source => exe_target%dependencies(1)%ptr%source

                if (exe_source%unit_scope == run_scope) then
                    write(stderr,'(A)',advance=(merge("yes","no ",modulo(jj,nCol)==0))) &
                        & [character(len=col_width) :: basename(exe_target%output_file, suffix=.false.)]
                    jj = jj + 1
                end if
            end if
        end do
        write(stderr,*)
    end subroutine compact_list_all

    subroutine compact_list()
    integer, parameter :: LINE_WIDTH = 80
    integer :: ii, jj, nCol
        jj = 1
        nCol = LINE_WIDTH/col_width
        write(stderr,*) 'Matched names:'
        do ii=1,size(executables)
            write(stderr,'(A)',advance=(merge("yes","no ",modulo(jj,nCol)==0))) &
                & [character(len=col_width) :: basename(executables(ii)%s, suffix=.false.)]
            jj = jj + 1
        end do
        write(stderr,*)
    end subroutine compact_list

end subroutine cmd_run

subroutine delete_skip(is_unix)
    !> delete directories in the build folder, skipping dependencies
    logical, intent(in) :: is_unix
    character(len=:), allocatable :: dir
    type(string_t), allocatable :: files(:)
    integer :: i
    call list_files('build', files, .false.)
    do i = 1, size(files)
        if (is_dir(files(i)%s)) then
            dir = files(i)%s
            if (.not.str_ends_with(dir,'dependencies')) call os_delete_dir(is_unix, dir)
        end if
    end do
end subroutine delete_skip

!> Delete targets of a specific scope with given description
subroutine delete_targets_by_scope(targets, scope, scope_name, deleted_any)
    type(build_target_ptr), intent(in) :: targets(:)
    integer, intent(in) :: scope
    character(len=*), intent(in) :: scope_name
    logical, intent(inout) :: deleted_any

    type(string_t), allocatable :: scope_targets(:)
    integer :: i

    call filter_executable_targets(targets, scope, scope_targets)
    if (size(scope_targets) > 0) then
        do i = 1, size(scope_targets)
            if (exists(scope_targets(i)%s)) then
                write(stdout, '(A,A,A,A)') "<INFO> Deleted ", scope_name, " target: ", basename(scope_targets(i)%s)
                call delete_file(scope_targets(i)%s)
                deleted_any = .true.
            end if
        end do
    end if
end subroutine delete_targets_by_scope

!> Delete build artifacts for specific target types (test, apps, examples)
subroutine delete_targets(settings, error)
    class(fpm_clean_settings), intent(inout) :: settings
    type(error_t), allocatable, intent(out) :: error

    type(package_config_t), allocatable :: package
    type(fpm_model_t), allocatable :: model
    type(build_target_ptr), allocatable :: targets(:)
    logical :: deleted_any
    
    ! Large variables -> safer on heap
    allocate(package,model)

    ! Get package configuration
    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    if (allocated(error)) return
    
    ! Build the model to understand targets
    call build_model(model, settings, package, error)
    if (allocated(error)) return

    ! Get the exact targets
    call targets_from_sources(targets, model, settings%prune, package%library, error)
    if (allocated(error)) return

    deleted_any = .false.

    ! Delete targets by scope using the original approach
    if (settings%clean_test) then
        call delete_targets_by_scope(targets, FPM_SCOPE_TEST, "test", deleted_any)
    end if

    if (settings%clean_apps) then
        call delete_targets_by_scope(targets, FPM_SCOPE_APP, "app", deleted_any)
    end if

    if (settings%clean_examples) then
        call delete_targets_by_scope(targets, FPM_SCOPE_EXAMPLE, "example", deleted_any)
    end if

    if (.not. deleted_any) then
        write(stdout, '(A)') "No matching build targets found to delete."
    end if

end subroutine delete_targets

!> Delete the build directory including or excluding dependencies. Can be used
!> to clear the registry cache.
subroutine cmd_clean(settings)
    !> Settings for the clean command.
    class(fpm_clean_settings), intent(inout) :: settings

    character :: user_response
    type(fpm_global_settings) :: global_settings
    type(error_t), allocatable :: error

    ! Clear registry cache
    if (settings%registry_cache) then
        call get_global_settings(global_settings, error) 
        if (allocated(error)) return

        call os_delete_dir(os_is_unix(), global_settings%registry_settings%cache_path)
    end if

    ! Handle target-specific cleaning
    if (any([settings%clean_test, settings%clean_apps, settings%clean_examples])) then
        if (.not. is_dir('build')) then
            write (stdout, '(A)') "fpm: No build directory found."
            return
        end if
        call delete_targets(settings, error)
        if (allocated(error)) then
            write(stderr, '(A)') 'Error: ' // error%message
            return
        end if
        return
    end if

    if (is_dir('build')) then
        ! Remove the entire build directory
        if (settings%clean_all) then
            call os_delete_dir(os_is_unix(), 'build'); return
        ! Remove the build directory but skip dependencies
        else if (settings%clean_skip) then
            call delete_skip(os_is_unix()); return
        end if

        ! Prompt to remove the build directory but skip dependencies
        write(stdout, '(A)', advance='no') "Delete build, excluding dependencies (y/n)? "
        read(stdin, '(A1)') user_response
        if (lower(user_response) == 'y') call delete_skip(os_is_unix())
    else
        write (stdout, '(A)') "fpm: No build directory found."
    end if
end subroutine cmd_clean

!> Sort executables by namelist ID, and trim unused values
pure subroutine sort_executables(target_ID,executables)
    integer, allocatable, intent(inout) :: target_ID(:)
    type(string_t), allocatable, intent(inout) :: executables(:)
    
    integer :: i,j,n,used
    
    n = size(target_ID)
    used = 0
    
    sort: do i=1,n
       do j=i+1,n
          if (target_ID(j)<target_ID(i)) &
          call swap(target_ID(i),target_ID(j),executables(i),executables(j))
       end do
       if (target_ID(i)<huge(target_ID(i))) used = i
    end do sort   
    
    if (used>0 .and. used<n) then 
        target_ID = target_ID(1:used)
        executables = executables(1:used)
    end if
    
    contains
    
    elemental subroutine swap(t1,t2,e1,e2)
       integer, intent(inout) :: t1,t2
       type(string_t), intent(inout) :: e1,e2
       integer :: tmp
       type(string_t) :: etmp
       
       tmp = t1
       t1  = t2
       t2  = tmp 
       etmp = e1
       e1  = e2
       e2  = etmp        
    end subroutine swap
    
end subroutine sort_executables

!> Check if an executable should be run 
logical function should_be_run(settings,run_scope,exe_target)
    class(fpm_run_settings), intent(in) :: settings
    integer, intent(in) :: run_scope
    type(build_target_t), intent(in) :: exe_target
    
    if (exe_target%is_executable_target(run_scope)) then
        
        associate(exe_source => exe_target%dependencies(1)%ptr%source)
            
            if (exe_source%unit_scope/=run_scope) then 
                
                ! Other scope
                should_be_run = .false.
                
            elseif (size(settings%name) == 0 .or. settings%list) then

                ! Run all or list all
                should_be_run = .true.

            else

                ! Is found in list
                should_be_run = settings%name_ID(exe_source%exe_name)>0
                
            end if            
            
        end associate
                        
    else
        
        !> Invalid target
        should_be_run = .false.
        
    endif
    
end function should_be_run

!> Save the current runtime library path (e.g., PATH or LD_LIBRARY_PATH)
function save_library_path() result(path)
    character(len=:), allocatable :: path

    select case (get_os_type())
    case (OS_WINDOWS)
        path = get_env("PATH", default="")
    case (OS_MACOS)
        path = get_env("DYLD_LIBRARY_PATH", default="")
    case default ! UNIX/Linux
        path = get_env("LD_LIBRARY_PATH", default="")
    end select
end function save_library_path

!> Set the runtime library path for the current process (used for subprocesses)
subroutine set_library_path(model, targets, error)
    type(fpm_model_t), intent(in) :: model
    type(build_target_ptr), intent(inout), target :: targets(:)
    type(error_t), allocatable, intent(out) :: error
     
    type(string_t), allocatable :: shared_lib_dirs(:)
    character(len=:), allocatable :: new_path, sep, current
    logical :: success
    integer :: i

    ! Get library directories
    call get_library_dirs(model, targets, shared_lib_dirs)

    ! Select platform-specific separator
    select case (get_os_type())
    case (OS_WINDOWS)
        sep = ";"
    case default
        sep = ":"
    end select

    ! Join the directories into a path string
    new_path = ""
    do i = 1, size(shared_lib_dirs)
        if (i > 1) new_path = new_path // sep
        new_path = new_path // shared_lib_dirs(i)%s
    end do    
    
    ! Get current library path
    current = save_library_path()

    ! Set the appropriate environment variable
    select case (get_os_type())
    case (OS_WINDOWS)
        success = set_env("PATH", new_path // sep // current)
    case (OS_MACOS)
        success = set_env("DYLD_LIBRARY_PATH", new_path // sep // current)
    case default ! UNIX/Linux
        success = set_env("LD_LIBRARY_PATH", new_path // sep // current)
    end select
    
    if (.not.success) call fatal_error(error,"Cannot set library path: "//new_path)

end subroutine set_library_path


!> Restore a previously saved runtime library path
subroutine restore_library_path(saved_path, error)
    character(*), intent(in) :: saved_path
    type(error_t), allocatable, intent(out) :: error
    logical :: success

    select case (get_os_type())
    case (OS_WINDOWS)
        success = set_env("PATH", saved_path)
    case (OS_MACOS)
        success = set_env("DYLD_LIBRARY_PATH", saved_path)
    case default ! UNIX/Linux
        success = set_env("LD_LIBRARY_PATH", saved_path)
    end select
    
    if (.not.success) call fatal_error(error, "Cannot restore library path: "//saved_path)

end subroutine restore_library_path

end module fpm
