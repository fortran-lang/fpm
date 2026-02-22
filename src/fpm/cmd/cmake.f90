!> Implementation of the `fpm generate --cmake` command
module fpm_cmd_cmake
    use fpm_command_line, only: fpm_generate_settings
    use fpm_error, only: error_t, fatal_error, fpm_stop
    use fpm_filesystem, only: dirname, is_dir, fileopen, fileclose, unix_path
    use fpm_cmake_check, only: compute_manifest_hash, compute_project_hash
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_manifest_library, only: library_config_t
    use fpm_manifest_preprocess, only: preprocess_config_t
    use fpm_manifest_fortran, only: fortran_config_t
    use fpm_manifest_executable, only: executable_config_t
    use fpm_manifest_test, only: test_config_t
    use fpm_model, only: fpm_model_t, srcfile_t, FPM_SCOPE_LIB, FPM_SCOPE_APP, &
                         FPM_SCOPE_TEST, FPM_UNIT_PROGRAM, &
                         FPM_UNIT_MODULE, FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                         FPM_UNIT_CSOURCE, FPM_UNIT_CPPSOURCE, FPM_UNIT_CHEADER
    use fpm, only: build_model
    use fpm_compiler, only: get_macros, compiler_enum, &
                        id_gcc, id_f95, id_caf, id_nvhpc, id_flang, id_amdflang, &
                        id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows, &
                        id_intel_llvm_nix, id_intel_llvm_windows, &
                        id_nag, id_lfortran
    use fpm_strings, only: string_t, lower, str_ends_with, str_begins_with_str, fnv_1a, resize
    use fpm_sources, only: fortran_suffixes, c_source_suffixes, cpp_source_suffixes, c_header_suffixes
    use fpm_hash_table, only: string_hash_map_t
    use fpm_targets, only: targets_from_sources, build_target_ptr
    use fpm_versioning, only: version_t
    use shlex_module, only: shlex_split => split
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, int64
    implicit none
    private
    public :: cmd_generate
    public :: categorize_link_flag, extract_path, extract_libname
    public :: detect_target_language, is_fortran_source
    public :: get_fortran_format_string, link_flags_t
    public :: generate_cmake
    ! Public constants
    public :: LINK_FLAG_UNKNOWN, LINK_FLAG_OPTION, LINK_FLAG_LIBDIR, LINK_FLAG_LIBNAME
    public :: TARGET_LANG_FORTRAN, TARGET_LANG_C, TARGET_LANG_CXX
    public :: CMAKE_VISIBILITY_PUBLIC, CMAKE_VISIBILITY_PRIVATE, CMAKE_VISIBILITY_INTERFACE
    public :: FORTRAN_FORMAT_FREE, FORTRAN_FORMAT_FIXED
    public :: CMAKE_MINIMUM_VERSION, CMAKE_EXE_SUFFIX

    !> Builder type for efficient line-by-line array construction
    !> Uses exponential growth (1.5x) to achieve O(n) amortized append complexity
    type :: line_builder_t
        type(string_t), allocatable :: lines(:)
        integer :: size = 0      ! Number of lines currently stored
        integer :: capacity = 0  ! Total allocated capacity
    contains
        procedure :: append => builder_append_line
        procedure :: finalize => builder_finalize
    end type line_builder_t

    !> Type to hold dependency information
    type :: dependency_info_t
        character(:), allocatable :: name
        character(:), allocatable :: path
        logical :: has_cmake
        logical :: is_dev_dependency  ! True if this is a dev-dependency
        type(string_t), allocatable :: sources(:)
        type(string_t), allocatable :: depends_on(:)  ! Names of this dependency's own dependencies
        type(preprocess_config_t), allocatable :: preprocess(:)  ! Preprocessing configuration
        type(version_t) :: version  ! Package version
    end type dependency_info_t

    !> Type to hold categorized link flags
    type :: link_flags_t
        type(string_t), allocatable :: linker_options(:)   ! -Wl,*, -pthread, -framework, etc.
        type(string_t), allocatable :: library_dirs(:)     ! Paths from -L/path
        type(string_t), allocatable :: library_names(:)    ! Names from -lfoo (stored without -l)
    end type link_flags_t

    !> ===== Link Flag Categories =====
    !> Unknown flag type (treated as linker option for safety)
    integer, parameter :: LINK_FLAG_UNKNOWN = 0
    !> Linker option flag (-Wl,*, -pthread, -framework, etc.)
    integer, parameter :: LINK_FLAG_OPTION = 1
    !> Library directory flag (-L/path)
    integer, parameter :: LINK_FLAG_LIBDIR = 2
    !> Library name flag (-lfoo)
    integer, parameter :: LINK_FLAG_LIBNAME = 3

    !> ===== Target Language Codes =====
    !> Fortran language target
    integer, parameter :: TARGET_LANG_FORTRAN = 1
    !> C language target
    integer, parameter :: TARGET_LANG_C = 2
    !> C++ language target
    integer, parameter :: TARGET_LANG_CXX = 3

    !> ===== CMake Visibility Keywords =====
    !> Public visibility - propagates to dependents
    character(len=*), parameter :: CMAKE_VISIBILITY_PUBLIC = 'PUBLIC'
    !> Private visibility - internal to target only
    character(len=*), parameter :: CMAKE_VISIBILITY_PRIVATE = 'PRIVATE'
    !> Interface visibility - only for dependents (header-only libraries)
    character(len=*), parameter :: CMAKE_VISIBILITY_INTERFACE = 'INTERFACE'

    !> ===== Fortran Source Format =====
    !> Free-form Fortran source format
    character(len=*), parameter :: FORTRAN_FORMAT_FREE = 'FREE'
    !> Fixed-form Fortran source format
    character(len=*), parameter :: FORTRAN_FORMAT_FIXED = 'FIXED'

    !> ===== CMake Configuration =====
    !> Minimum CMake version required for generated CMakeLists.txt
    character(len=*), parameter :: CMAKE_MINIMUM_VERSION = '3.12'
    !> Suffix appended to executable names to avoid CMake reserved name conflicts
    character(len=*), parameter :: CMAKE_EXE_SUFFIX = '_exe'

contains

    !> Get the compiler-specific preprocessing flag for Fortran
    pure function get_cpp_flag(compiler_id) result(flag)
        integer(compiler_enum), intent(in) :: compiler_id
        character(:), allocatable :: flag
        select case(compiler_id)
        case default
            flag = ""
        case(id_caf, id_gcc, id_f95, id_nvhpc, id_flang, id_amdflang)
            flag = "-cpp"
        case(id_intel_classic_windows, id_intel_llvm_windows)
            flag = "/fpp"
        case(id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix, id_nag)
            flag = "-fpp"
        case(id_lfortran)
            flag = "--cpp"
        end select
    end function get_cpp_flag

    !> Add preprocessing flags to target (preprocessing flag + expanded macros)
    subroutine append_preprocessing_flags(builder, target_name, compiler_id, &
                                          preprocess, version, visibility)
        type(line_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: target_name
        integer(compiler_enum), intent(in) :: compiler_id
        type(preprocess_config_t), intent(in), optional :: preprocess(:)
        type(version_t), intent(in), optional :: version
        character(len=*), intent(in), optional :: visibility

        character(:), allocatable :: vis
        integer :: i
        logical :: has_macros

        ! Return early if no preprocessing
        if (.not. present(preprocess)) return
        if (size(preprocess) == 0) return

        ! Check if any preprocess config has macros
        has_macros = .false.
        do i = 1, size(preprocess)
            if (allocated(preprocess(i)%macros)) then
                if (size(preprocess(i)%macros) > 0) then
                    has_macros = .true.
                    exit
                end if
            end if
        end do
        if (.not. has_macros) return

        ! Default visibility
        vis = CMAKE_VISIBILITY_PUBLIC
        if (present(visibility)) vis = visibility

        ! Start target_compile_options only if we have macros to add
        call builder%append( 'target_compile_options('//target_name//' '//vis)

        ! Process each preprocess config (typically just one: cpp)
        do i = 1, size(preprocess)
            if (.not. allocated(preprocess(i)%macros)) cycle
            if (size(preprocess(i)%macros) == 0) cycle
            call append_macros_for_config(builder, preprocess(i), compiler_id, version)
        end do

        call builder%append( ')')

    end subroutine append_preprocessing_flags

    !> Add base preprocessing flag (-cpp) to any Fortran target without macros
    subroutine append_base_preprocessing(builder, target_name, compiler_id, visibility)
        type(line_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: target_name
        integer(compiler_enum), intent(in) :: compiler_id
        character(len=*), intent(in), optional :: visibility

        character(:), allocatable :: cpp_flag, vis

        ! Default visibility
        vis = CMAKE_VISIBILITY_PUBLIC
        if (present(visibility)) vis = visibility

        cpp_flag = get_cpp_flag(compiler_id)

        ! Add preprocessing flag if compiler needs one
        if (len(cpp_flag) > 0) then
            call builder%append( 'target_compile_options('//target_name//' '//vis)
            call builder%append( '    $<$<COMPILE_LANGUAGE:Fortran>:'//cpp_flag//'>')
            call builder%append( ')')
        end if

    end subroutine append_base_preprocessing

    !> Resolve include directories from library config
    subroutine resolve_include_dirs(library_config, include_dirs)
        type(library_config_t), intent(in), optional :: library_config
        type(string_t), allocatable, intent(out) :: include_dirs(:)

        integer :: i, count

        ! Count existing include directories
        count = 0
        if (present(library_config)) then
            if (allocated(library_config%include_dir)) then
                do i = 1, size(library_config%include_dir)
                    if (is_dir(trim(library_config%include_dir(i)%s))) then
                        count = count + 1
                    end if
                end do
            else if (has_include_dir()) then
                count = 1
            end if
        else if (has_include_dir()) then
            count = 1
        end if

        ! Allocate and fill include_dirs array
        if (count > 0) then
            allocate(include_dirs(count))
            count = 0
            if (present(library_config)) then
                if (allocated(library_config%include_dir)) then
                    do i = 1, size(library_config%include_dir)
                        if (is_dir(trim(library_config%include_dir(i)%s))) then
                            count = count + 1
                            include_dirs(count)%s = trim(library_config%include_dir(i)%s)
                        end if
                    end do
                else if (has_include_dir()) then
                    include_dirs(1)%s = 'include'
                end if
            else if (has_include_dir()) then
                include_dirs(1)%s = 'include'
            end if
        else
            allocate(include_dirs(0))
        end if

    end subroutine resolve_include_dirs

    !> Resolve include directories for a dependency
    subroutine resolve_dep_include_dirs(dep_path, include_dirs)
        character(len=*), intent(in) :: dep_path
        type(string_t), allocatable, intent(out) :: include_dirs(:)

        logical :: dep_has_include

        dep_has_include = is_dir(trim(dep_path)//'/include')
        if (dep_has_include) then
            allocate(include_dirs(1))
            include_dirs(1)%s = 'include'
        else
            allocate(include_dirs(0))
        end if

    end subroutine resolve_dep_include_dirs

    !> Write a library target (STATIC or INTERFACE) to CMakeLists.txt
    subroutine write_library_target(builder, target_name, sources, &
                                    include_dirs, compiler_id, &
                                    preprocess, version, fortran_config)
        type(line_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: target_name
        type(string_t), intent(in) :: sources(:)
        type(string_t), intent(in) :: include_dirs(:)
        integer(compiler_enum), intent(in) :: compiler_id
        type(preprocess_config_t), intent(in), optional :: preprocess(:)
        type(version_t), intent(in), optional :: version
        type(fortran_config_t), intent(in), optional :: fortran_config

        integer :: i
        character(len=:), allocatable :: visibility
        logical :: is_header_only

        is_header_only = (size(sources) == 0)

        if (is_header_only) then
            ! INTERFACE library for header-only
            visibility = CMAKE_VISIBILITY_INTERFACE
            call builder%append( "# Header-only library")
            call builder%append( 'add_library('//target_name//' INTERFACE)')
            call builder%append( 'target_include_directories('//target_name//' INTERFACE')
        else
            ! STATIC library
            visibility = CMAKE_VISIBILITY_PUBLIC
            call builder%append( "# Library")
            call builder%append( 'add_library('//target_name)
            do i = 1, size(sources)
                call builder%append( '    '//clean_path(sources(i)%s))
            end do
            call builder%append( ')')

            ! Set source format if not default
            if (should_set_fortran_format(fortran_config, preprocess, sources)) then
                call append_fortran_format(builder, sources, &
                                           get_fortran_format_string(fortran_config), preprocess)
            end if

            ! Set module directory properties
            call builder%append( 'set_target_properties('//target_name//' PROPERTIES')
            call builder%append( '    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"')
            call builder%append( '    POSITION_INDEPENDENT_CODE ON')
            call builder%append( ')')
            call builder%append( 'target_include_directories('//target_name//' PUBLIC')
            call builder%append( '    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>')
        end if

        ! Add include directories
        do i = 1, size(include_dirs)
            call builder%append( '    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/'// &
                           trim(unix_path(include_dirs(i)%s))//'>')
        end do
        call builder%append( '    $<INSTALL_INTERFACE:include>')
        call builder%append( ')')

        ! Add base preprocessing flag (always enable -cpp for Fortran)
        call append_base_preprocessing(builder, target_name, compiler_id, visibility)

        ! Add preprocessing flags with macros
        if (present(preprocess) .or. present(version)) then
            call append_preprocessing_flags(builder, target_name, compiler_id, &
                                            preprocess, version, visibility)
        end if

    end subroutine write_library_target

    !> Entry point for the generate subcommand
    subroutine cmd_generate(settings)
        !> Representation of the command line arguments
        type(fpm_generate_settings), intent(inout) :: settings

        type(package_config_t) :: package
        type(fpm_model_t) :: model
        type(error_t), allocatable :: error

        ! Read in manifest
        call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
        if (allocated(error)) then
            call fpm_stop(1, '*cmd_generate* Package error: '//error%message)
        end if

        ! For CMake generation, always include tests to generate a complete file
        settings%build_tests = .true.

        ! Build model to discover sources
        call build_model(model, settings, package, error)
        if (allocated(error)) then
            call fpm_stop(1, '*cmd_generate* Model error: '//error%message)
        end if

        ! Generate CMake if requested
        if (settings%cmake) then
            call generate_cmake(package, model, error)
            if (allocated(error)) then
                call fpm_stop(1, '*cmd_generate* CMake error: '//error%message)
            end if
            write(stdout, '(a)') 'Generated CMakeLists.txt'
        end if

    end subroutine cmd_generate

    !> Generate CMakeLists.txt from package and model
    subroutine generate_cmake(package, model, error)
        type(package_config_t), intent(in) :: package
        type(fpm_model_t), intent(inout) :: model
        type(error_t), allocatable, intent(out) :: error

        type(string_t), allocatable :: lib_sources(:), app_sources(:), test_sources(:)
        type(string_t), allocatable :: executables(:), tests(:)
        type(line_builder_t) :: cmake_builder
        type(dependency_info_t), allocatable :: dependencies(:)
        type(string_t), allocatable :: used_package_names(:)
        type(build_target_ptr), allocatable :: targets(:)
        character(len=:), allocatable :: version_str, manifest_hash
        integer :: i
        logical :: has_library

        ! Collect sources by scope from the root package (index 1)
        call collect_sources(model%packages(1)%sources, lib_sources, app_sources, &
                            test_sources, executables, tests)

        ! Check if package has a library (either compilable sources or header-only)
        ! Header-only libraries have an include/ directory but no compilable sources
        has_library = size(lib_sources) > 0 .or. &
                     (size(lib_sources) == 0 .and. has_include_dir_from_manifest(package%library))

        ! Build version string
        version_str = package%version%s()
        if (version_str == '0') version_str = "0.1.0"

        ! Compute project hash for staleness detection (manifest + all source files)
        manifest_hash = compute_project_hash("fpm.toml", model%packages(1)%sources)

        ! Generate pruned build targets to determine which packages are actually used
        call targets_from_sources(targets, model, prune=.true., error=error)
        if (allocated(error)) return

        ! Extract which packages are actually used from the pruned targets
        call collect_used_packages(targets, used_package_names)

        ! Collect ONLY dependencies that appear in targets
        call collect_dependencies(model, package, dependencies, used_package_names)

        ! Generate CMakeLists.txt for fpm-only dependencies
        do i = 1, size(dependencies)
            if (.not. dependencies(i)%has_cmake) then
                call generate_dependency_cmake(dependencies(i), '.', model%compiler%id, error)
                if (allocated(error)) return
            end if
        end do

        ! Generate CMakeLists.txt content as string_t array
        call write_cmake_content(cmake_builder, package%name, version_str, manifest_hash, &
                                lib_sources, executables, tests, has_library, &
                                model%include_tests, model%packages(1)%sources, &
                                dependencies, model, package%library, package%preprocess, &
                                package%fortran, package%executable, package%test, package%version)

        ! Write to file
        call write_lines_to_file("CMakeLists.txt", cmake_builder%finalize(), error)

    end subroutine generate_cmake

    !> Write string_t array to file
    subroutine write_lines_to_file(filename, lines, error)
        character(len=*), intent(in) :: filename
        type(string_t), intent(in) :: lines(:)
        type(error_t), allocatable, intent(out) :: error

        integer :: lun, i, ios
        character(len=256) :: message

        call fileopen(filename, lun, ier=ios, file_status='replace')
        if (ios /= 0) then
            call fatal_error(error, "Failed to open file for writing: "//filename)
            return
        end if

        do i = 1, size(lines)
            write(lun, '(a)', iostat=ios, iomsg=message) trim(lines(i)%s)
            if (ios /= 0) then
                call fileclose(lun)
                call fatal_error(error, "Failed to write to file "//filename//": "//trim(message))
                return
            end if
        end do

        call fileclose(lun)

    end subroutine write_lines_to_file

    !> Collect sources by scope
    subroutine collect_sources(sources, lib_sources, app_sources, test_sources, &
                              executables, tests)
        type(srcfile_t), intent(in) :: sources(:)
        type(string_t), allocatable, intent(out) :: lib_sources(:)
        type(string_t), allocatable, intent(out) :: app_sources(:)
        type(string_t), allocatable, intent(out) :: test_sources(:)
        type(string_t), allocatable, intent(out) :: executables(:)
        type(string_t), allocatable, intent(out) :: tests(:)

        integer :: i, n_lib, n_exe, n_tests
        type(string_t), allocatable :: temp_lib(:)
        type(string_t), allocatable :: temp_exe(:), temp_tests(:)

        ! First pass: count
        n_lib = 0
        n_exe = 0
        n_tests = 0

        do i = 1, size(sources)
            select case (sources(i)%unit_scope)
            case (FPM_SCOPE_LIB)
                n_lib = n_lib + 1
            case (FPM_SCOPE_APP)
                if (sources(i)%unit_type == FPM_UNIT_PROGRAM) n_exe = n_exe + 1
            case (FPM_SCOPE_TEST)
                if (sources(i)%unit_type == FPM_UNIT_PROGRAM) n_tests = n_tests + 1
            end select
        end do

        ! Allocate arrays
        allocate(temp_lib(n_lib))
        allocate(temp_exe(n_exe), temp_tests(n_tests))

        ! Second pass: populate arrays
        n_lib = 0
        n_exe = 0
        n_tests = 0

        do i = 1, size(sources)
            select case (sources(i)%unit_scope)
            case (FPM_SCOPE_LIB)
                n_lib = n_lib + 1
                temp_lib(n_lib)%s = sources(i)%file_name
            case (FPM_SCOPE_APP)
                if (sources(i)%unit_type == FPM_UNIT_PROGRAM) then
                    n_exe = n_exe + 1
                    if (allocated(sources(i)%exe_name)) then
                        temp_exe(n_exe)%s = sources(i)%exe_name
                    else
                        temp_exe(n_exe)%s = "app"
                    end if
                end if
            case (FPM_SCOPE_TEST)
                if (sources(i)%unit_type == FPM_UNIT_PROGRAM) then
                    n_tests = n_tests + 1
                    if (allocated(sources(i)%exe_name)) then
                        temp_tests(n_tests)%s = sources(i)%exe_name
                    else
                        temp_tests(n_tests)%s = "test"
                    end if
                end if
            end select
        end do

        ! Move to output
        call move_alloc(temp_lib, lib_sources)
        call move_alloc(temp_exe, executables)
        call move_alloc(temp_tests, tests)

        ! For app_sources and test_sources, we need to collect all non-program sources
        ! For simplicity in CMake, each executable will just reference its main file
        ! and link against the library
        call collect_scope_sources(sources, FPM_SCOPE_APP, app_sources)
        call collect_scope_sources(sources, FPM_SCOPE_TEST, test_sources)

    end subroutine collect_sources

    !> Collect all source files for a given scope
    subroutine collect_scope_sources(sources, scope, result_sources)
        type(srcfile_t), intent(in) :: sources(:)
        integer, intent(in) :: scope
        type(string_t), allocatable, intent(out) :: result_sources(:)

        integer :: i, n
        type(string_t), allocatable :: temp(:)

        ! Count
        n = 0
        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope) n = n + 1
        end do

        ! Allocate and populate
        allocate(temp(n))
        n = 0
        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope) then
                n = n + 1
                temp(n)%s = sources(i)%file_name
            end if
        end do

        call move_alloc(temp, result_sources)

    end subroutine collect_scope_sources

    !> Get sources for a specific executable by name and scope
    subroutine get_sources_for_exe(sources, exe_name, scope, result_sources, exclude_sources)
        type(srcfile_t), intent(in) :: sources(:)
        character(len=*), intent(in) :: exe_name
        integer, intent(in) :: scope
        type(string_t), allocatable, intent(out) :: result_sources(:)
        type(string_t), intent(in), optional :: exclude_sources(:)

        integer :: i, n, j
        type(string_t), allocatable :: temp(:)
        character(len=:), allocatable :: exe_dir
        logical :: found_main
        logical :: use_exclude_hash
        type(string_hash_map_t) :: exclude_map

        ! Find the main program file to determine the source directory
        found_main = .false.
        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope .and. &
                allocated(sources(i)%exe_name) .and. &
                trim(sources(i)%exe_name) == trim(exe_name)) then
                ! Extract directory from the main file path
                exe_dir = dirname(sources(i)%file_name)
                found_main = .true.
                exit
            end if
        end do

        if (.not. found_main) then
            ! No main file found, return empty array
            allocate(result_sources(0))
            return
        end if

        ! Build hash table for exclude_sources if present
        use_exclude_hash = .false.
        if (present(exclude_sources)) then
            if (size(exclude_sources) > 0) then
                use_exclude_hash = .true.
                call exclude_map%init(capacity=max(32, size(exclude_sources) * 2))
                do j = 1, size(exclude_sources)
                    call exclude_map%set(exclude_sources(j)%s, 1)
                end do
            end if
        end if

        ! Count all sources from the same directory with the same scope
        ! Exclude program files that don't match this executable
        n = 0
        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope .and. &
                dirname(sources(i)%file_name) == exe_dir) then
                ! Skip sources in the exclusion list
                if (use_exclude_hash) then
                    if (exclude_map%contains(sources(i)%file_name)) cycle
                end if

                ! Include all non-program sources (modules, etc.)
                ! or program sources that match this executable name
                if (sources(i)%unit_type /= FPM_UNIT_PROGRAM) then
                    n = n + 1
                else if (allocated(sources(i)%exe_name) .and. &
                         trim(sources(i)%exe_name) == trim(exe_name)) then
                    n = n + 1
                end if
            end if
        end do

        ! Allocate and populate
        allocate(temp(n))
        n = 0
        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope .and. &
                dirname(sources(i)%file_name) == exe_dir) then
                ! Skip sources in the exclusion list
                if (use_exclude_hash) then
                    if (exclude_map%contains(sources(i)%file_name)) cycle
                end if

                ! Include all non-program sources (modules, etc.)
                ! or program sources that match this executable name
                if (sources(i)%unit_type /= FPM_UNIT_PROGRAM) then
                    n = n + 1
                    temp(n)%s = sources(i)%file_name
                else if (allocated(sources(i)%exe_name) .and. &
                         trim(sources(i)%exe_name) == trim(exe_name)) then
                    n = n + 1
                    temp(n)%s = sources(i)%file_name
                end if
            end if
        end do

        call move_alloc(temp, result_sources)

        ! Cleanup
        if (use_exclude_hash) call exclude_map%destroy()

    end subroutine get_sources_for_exe


    !> Find module sources shared across multiple executables in a directory
    subroutine find_shared_module_sources(sources, scope, shared_sources)
        type(srcfile_t), intent(in) :: sources(:)
        integer, intent(in) :: scope
        type(string_t), allocatable, intent(out) :: shared_sources(:)

        type(string_hash_map_t) :: dir_program_count_map
        integer :: i, n_shared, program_count
        character(len=:), allocatable :: dir_path
        type(string_t), allocatable :: temp_shared(:)

        ! Initialize hash map for directory program counts
        call dir_program_count_map%init(capacity=256)

        ! Count programs per directory using hash map - O(n)
        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope .and. &
                sources(i)%unit_type == FPM_UNIT_PROGRAM) then
                dir_path = dirname(sources(i)%file_name)
                call dir_program_count_map%increment(dir_path)
            end if
        end do

        ! Collect non-program sources from directories with 2+ programs - O(n)
        n_shared = 0
        allocate(temp_shared(size(sources)))  ! Over-allocate

        do i = 1, size(sources)
            if (sources(i)%unit_scope /= scope) cycle
            if (sources(i)%unit_type == FPM_UNIT_PROGRAM) cycle

            dir_path = dirname(sources(i)%file_name)
            if (.not. dir_program_count_map%get(dir_path, program_count)) then
                program_count = 0
            end if

            if (program_count >= 2) then
                n_shared = n_shared + 1
                temp_shared(n_shared)%s = sources(i)%file_name
            end if
        end do

        ! Copy to correctly-sized output array
        if (n_shared > 0) then
            allocate(shared_sources(n_shared))
            shared_sources(1:n_shared) = temp_shared(1:n_shared)
        else
            allocate(shared_sources(0))
        end if

        ! Cleanup hash map
        call dir_program_count_map%destroy()

    end subroutine find_shared_module_sources

    !> Detect primary language of target sources
    function detect_target_language(sources) result(lang)
        type(string_t), intent(in) :: sources(:)
        integer :: lang

        integer :: i, j
        logical :: has_cpp, has_c, has_fortran, matched
        character(len=:), allocatable :: lower_name

        has_fortran = .false.
        has_c = .false.
        has_cpp = .false.

        do i = 1, size(sources)
            lower_name = lower(sources(i)%s)
            matched = .false.

            ! Check for C++ source extensions
            do j = 1, size(cpp_source_suffixes)
                if (str_ends_with(lower_name, trim(cpp_source_suffixes(j)))) then
                    has_cpp = .true.
                    matched = .true.
                    exit
                end if
            end do
            if (matched) cycle

            ! Check for C source extensions
            do j = 1, size(c_source_suffixes)
                if (str_ends_with(lower_name, trim(c_source_suffixes(j)))) then
                    has_c = .true.
                    matched = .true.
                    exit
                end if
            end do
            if (matched) cycle

            ! Check for C/C++ header extensions (don't count as any language)
            do j = 1, size(c_header_suffixes)
                if (str_ends_with(lower_name, trim(c_header_suffixes(j)))) then
                    matched = .true.
                    exit
                end if
            end do
            if (matched) cycle

            ! Assume Fortran for .f90, .f, .F90, .F, etc.
            has_fortran = .true.
        end do

        ! Priority: C++ > C > Fortran
        if (has_cpp) then
            lang = TARGET_LANG_CXX
        else if (has_c) then
            lang = TARGET_LANG_C
        else
            lang = TARGET_LANG_FORTRAN
        end if
    end function detect_target_language

    !> Detect all languages present in source files and return a CMake LANGUAGES string
    function detect_languages_string(sources) result(languages)
        type(string_t), intent(in) :: sources(:)
        character(:), allocatable :: languages

        integer :: i, j
        logical :: has_fortran, has_c, has_cpp, matched
        character(len=:), allocatable :: lower_name

        has_fortran = .false.
        has_c = .false.
        has_cpp = .false.

        do i = 1, size(sources)
            lower_name = lower(sources(i)%s)
            matched = .false.

            do j = 1, size(cpp_source_suffixes)
                if (str_ends_with(lower_name, trim(cpp_source_suffixes(j)))) then
                    has_cpp = .true.
                    matched = .true.
                    exit
                end if
            end do
            if (matched) cycle

            do j = 1, size(c_source_suffixes)
                if (str_ends_with(lower_name, trim(c_source_suffixes(j)))) then
                    has_c = .true.
                    matched = .true.
                    exit
                end if
            end do
            if (matched) cycle

            do j = 1, size(c_header_suffixes)
                if (str_ends_with(lower_name, trim(c_header_suffixes(j)))) then
                    matched = .true.
                    exit
                end if
            end do
            if (matched) cycle

            has_fortran = .true.
        end do

        languages = ''
        if (has_fortran) languages = 'Fortran'
        if (has_c) then
            if (len(languages) > 0) then
                languages = trim(languages)//' C'
            else
                languages = 'C'
            end if
        end if
        if (has_cpp) then
            if (len(languages) > 0) then
                languages = trim(languages)//' CXX'
            else
                languages = 'CXX'
            end if
        end if

        if (len(languages) == 0) languages = 'Fortran'
    end function detect_languages_string

    !> Add metapackage settings (include directories, link options, and libraries) to a target
    subroutine append_metapackage_settings(builder, target_name, model, is_interface, target_sources)
        type(line_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: target_name
        type(fpm_model_t), intent(in) :: model
        logical, intent(in), optional :: is_interface
        type(string_t), intent(in), optional :: target_sources(:)
        integer :: i, target_lang
        type(link_flags_t) :: parsed_flags
        character(len=:), allocatable :: prop_keyword, link_flags_to_use

        ! Determine property keyword based on target type
        prop_keyword = CMAKE_VISIBILITY_PRIVATE  ! Default for regular targets
        if (present(is_interface)) then
            if (is_interface) prop_keyword = CMAKE_VISIBILITY_INTERFACE
        end if

        ! Detect target language and select appropriate link flags
        if (present(target_sources)) then
            if (size(target_sources) > 0) then
                target_lang = detect_target_language(target_sources)
            else
                target_lang = 1  ! Default to Fortran for empty sources
            end if
        else
            target_lang = 1  ! Default to Fortran if sources not provided
        end if

        ! Select appropriate link flags based on language
        select case (target_lang)
            case (TARGET_LANG_FORTRAN)
                if (allocated(model%fortran_link_flags)) then
                    link_flags_to_use = model%fortran_link_flags
                end if
            case (TARGET_LANG_C)
                if (allocated(model%c_link_flags)) then
                    link_flags_to_use = model%c_link_flags
                end if
            case (TARGET_LANG_CXX)
                if (allocated(model%cxx_link_flags)) then
                    link_flags_to_use = model%cxx_link_flags
                end if
        end select

        ! Fallback to merged link_flags if language-specific not available
        if (.not. allocated(link_flags_to_use) .and. allocated(model%link_flags)) then
            link_flags_to_use = model%link_flags
        end if

        ! Add include directories from metapackages (e.g., MPI, HDF5)
        if (allocated(model%include_dirs)) then
            call append_cmake_list_directive(builder, 'target_include_directories', &
                                             target_name, prop_keyword, model%include_dirs)
        end if

        ! Parse and categorize link flags
        if (allocated(link_flags_to_use)) then
            if (len_trim(link_flags_to_use) > 0) then
                call parse_link_flags(link_flags_to_use, parsed_flags)

                ! Add library directories
                if (allocated(parsed_flags%library_dirs)) then
                    call append_cmake_list_directive(builder, 'target_link_directories', &
                                                     target_name, prop_keyword, parsed_flags%library_dirs)
                end if

                ! Add linker options
                if (allocated(parsed_flags%linker_options)) then
                    call append_cmake_list_directive(builder, 'target_link_options', &
                                                     target_name, prop_keyword, parsed_flags%linker_options)
                end if

                ! Add library names
                if (allocated(parsed_flags%library_names)) then
                    call append_cmake_list_directive(builder, 'target_link_libraries', &
                                                     target_name, prop_keyword, parsed_flags%library_names)
                end if
            end if
        end if

        ! Add link libraries from model (e.g., openblas, lapack from non-link_flags sources)
        if (allocated(model%link_libraries)) then
            call append_cmake_list_directive(builder, 'target_link_libraries', &
                                             target_name, prop_keyword, model%link_libraries)
        end if
    end subroutine append_metapackage_settings

    !> Write CMake content to string_t array
    subroutine write_cmake_content(builder, name, version, manifest_hash, lib_sources, &
                                  executables, tests, has_library, include_tests, sources, &
                                  dependencies, model, library_config, preprocess, fortran_config, &
                                  executable_config, test_config, package_version)
        type(line_builder_t), intent(out) :: builder
        character(len=*), intent(in) :: name, version, manifest_hash
        type(string_t), intent(in) :: lib_sources(:)
        type(string_t), intent(in) :: executables(:), tests(:)
        logical, intent(in) :: has_library, include_tests
        type(srcfile_t), intent(in) :: sources(:)
        type(dependency_info_t), intent(in) :: dependencies(:)
        type(fpm_model_t), intent(in) :: model
        type(library_config_t), intent(in), optional :: library_config
        type(preprocess_config_t), intent(in), optional :: preprocess(:)
        type(fortran_config_t), intent(in), optional :: fortran_config
        type(executable_config_t), intent(in), optional :: executable_config(:)
        type(test_config_t), intent(in), optional :: test_config(:)
        type(version_t), intent(in), optional :: package_version

        integer :: i, j, k, ext_i
        type(string_t), allocatable :: exe_sources(:), include_dirs(:)
        character(len=:), allocatable :: lib_name, exe_name_str, original_exe_name, original_test_name, languages, lower_fname
        logical :: has_c, has_cpp, has_fortran, ext_matched
        type(string_t), allocatable :: shared_test_modules(:), shared_app_modules(:)

        ! Initialize empty lines array
        ! Builder will auto-initialize

        ! Library name - if same as an executable, append _lib
        lib_name = trim(name)

        ! Detect languages used
        has_fortran = .false.
        has_c = .false.
        has_cpp = .false.
        do k = 1, size(sources)
            select case (sources(k)%unit_type)
                case (FPM_UNIT_PROGRAM)
                    ! Programs can be either Fortran or C/C++ - check extension
                    lower_fname = lower(sources(k)%file_name)
                    ext_matched = .false.

                    ! Check for C++ extensions
                    do ext_i = 1, size(cpp_source_suffixes)
                        if (str_ends_with(lower_fname, trim(cpp_source_suffixes(ext_i)))) then
                            has_cpp = .true.
                            ext_matched = .true.
                            exit
                        end if
                    end do

                    if (.not. ext_matched) then
                        ! Check for C extensions
                        do ext_i = 1, size(c_source_suffixes)
                            if (str_ends_with(lower_fname, trim(c_source_suffixes(ext_i)))) then
                                has_c = .true.
                                ext_matched = .true.
                                exit
                            end if
                        end do
                    end if

                    if (.not. ext_matched) then
                        has_fortran = .true.
                    end if
                case (FPM_UNIT_MODULE, FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM)
                    has_fortran = .true.
                case (FPM_UNIT_CSOURCE)
                    has_c = .true.
                case (FPM_UNIT_CPPSOURCE)
                    has_cpp = .true.
            end select
        end do

        ! Build languages string based on detected sources
        languages = ''
        if (has_fortran) languages = 'Fortran'
        if (has_c) then
            if (len(languages) > 0) then
                languages = trim(languages)//' C'
            else
                languages = 'C'
            end if
        end if
        if (has_cpp) then
            if (len(languages) > 0) then
                languages = trim(languages)//' CXX'
            else
                languages = 'CXX'
            end if
        end if

        ! If no compilable sources detected (e.g., header-only), default to Fortran
        ! This maintains backward compatibility for edge cases
        if (len(languages) == 0) languages = 'Fortran'

        ! Header
        call builder%append( "# CMakeLists.txt generated by fpm")
        call builder%append( "# Project hash: "//manifest_hash)
        call builder%append( "")
        call builder%append( "cmake_minimum_required(VERSION "//CMAKE_MINIMUM_VERSION//")")
        call builder%append( 'project('//trim(name)//' VERSION '//trim(version)// &
                       ' LANGUAGES '//trim(languages)//')')
        call builder%append( "")

        ! Library target (defined before dependencies so subdirs can reference it)
        if (has_library) then
            ! Check if any executable has the same name as the library
            do i = 1, size(executables)
                if (trim(executables(i)%s) == trim(name)) then
                    lib_name = trim(name)//'_lib'
                    exit
                end if
            end do

            ! Resolve include directories from library config
            call resolve_include_dirs(library_config, include_dirs)

            ! Generate library target (STATIC or INTERFACE)
            call write_library_target(builder, lib_name, lib_sources, &
                                     include_dirs, model%compiler%id, &
                                     preprocess, package_version, fortran_config)

            call builder%append( "")
        end if

        ! Dependencies section (after library so subdirs can reference it)
        call write_cmake_dependencies(builder, dependencies, '.')

        ! Link library to dependencies (after dependencies are added)
        ! Skip dev-dependencies - those are only for tests
        if (has_library .and. size(dependencies) > 0) then
            ! Use INTERFACE for header-only libraries, PUBLIC for regular libraries
            if (size(lib_sources) == 0) then
                call builder%append( 'target_link_libraries('//lib_name//' INTERFACE')
            else
                call builder%append( 'target_link_libraries('//lib_name//' PUBLIC')
            end if
            do i = 1, size(dependencies)
                ! Skip dev-dependencies (only link to regular dependencies)
                if (.not. dependencies(i)%is_dev_dependency) then
                    ! Use the correct target name based on CMake support
                    if (dependencies(i)%has_cmake) then
                        ! For toml-f and jonquil, use the :: interface target
                        call builder%append( '    '//trim(dependencies(i)%name)//'::'//trim(dependencies(i)%name))
                    else
                        ! For fpm-only deps, use the library target directly
                        call builder%append( '    '//trim(dependencies(i)%name))
                    end if
                end if
            end do
            call builder%append( ')')
            call builder%append( "")
        end if

        ! Add metapackage settings (include dirs, link flags, and libraries)
        if (has_library) then
            ! Pass .true. for header-only libraries (INTERFACE), .false. for regular libraries
            call append_metapackage_settings(builder, lib_name, model, size(lib_sources) == 0, lib_sources)
            call builder%append( "")
        end if

        ! Executable targets
        if (size(executables) > 0) then
            ! Find shared app module sources before generating app executables
            call find_shared_module_sources(sources, FPM_SCOPE_APP, shared_app_modules)

            ! Generate object library for shared app modules if any exist
            if (allocated(shared_app_modules)) then
                if (size(shared_app_modules) > 0) then
                    call builder%append( "# Shared app modules")
                    call builder%append( 'add_library(app_modules_obj OBJECT')
                    do j = 1, size(shared_app_modules)
                        call builder%append( '    '//clean_path(shared_app_modules(j)%s))
                    end do
                    call builder%append( ')')
                    call builder%append( 'set_target_properties(app_modules_obj PROPERTIES')
                    call builder%append( '    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"')
                    call builder%append( ')')
                    call builder%append( 'target_include_directories(app_modules_obj PUBLIC')
                    call builder%append( '    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>')
                    call builder%append( ')')
                    if (has_library) then
                        call builder%append( 'target_link_libraries(app_modules_obj PUBLIC '//lib_name//')')
                    end if
                    call builder%append( "")
                end if
            end if

            call builder%append( "# Executables")

            do i = 1, size(executables)
                original_exe_name = trim(executables(i)%s)
                exe_name_str = sanitize_target_name(original_exe_name)

                ! Get sources specific to this executable, excluding shared modules
                if (allocated(shared_app_modules)) then
                    call get_sources_for_exe(sources, original_exe_name, FPM_SCOPE_APP, exe_sources, shared_app_modules)
                else
                    call get_sources_for_exe(sources, original_exe_name, FPM_SCOPE_APP, exe_sources)
                end if

                call builder%append( 'add_executable('//exe_name_str)
                do j = 1, size(exe_sources)
                    call builder%append( '    '//clean_path(exe_sources(j)%s))
                end do
                call builder%append( ')')

                ! Set output binary name to original if target name was sanitized
                if (exe_name_str /= original_exe_name) then
                    call builder%append( 'set_target_properties('//exe_name_str//' PROPERTIES')
                    call builder%append( '    OUTPUT_NAME "'//original_exe_name//'"')
                    call builder%append( ')')
                end if

                ! Set source format if not default
                if (should_set_fortran_format(fortran_config, preprocess, exe_sources)) then
                    call append_fortran_format(builder, exe_sources, &
                                               get_fortran_format_string(fortran_config), preprocess)
                end if

                if (has_library .or. size(dependencies) > 0 .or. &
                    (allocated(shared_app_modules) .and. size(shared_app_modules) > 0)) then
                    call builder%append( 'target_link_libraries('//exe_name_str//' PRIVATE')
                    if (has_library) then
                        call builder%append( '    '//lib_name)
                    end if
                    ! Link dependencies and dev-dependencies to executables
                    if (size(dependencies) > 0) then
                        do k = 1, size(dependencies)
                            ! Link both regular and dev-dependencies for executables
                            if (.not. has_library .or. dependencies(k)%is_dev_dependency) then
                                if (dependencies(k)%has_cmake) then
                                    call builder%append( '    '//trim(dependencies(k)%name)//'::'//trim(dependencies(k)%name))
                                else
                                    call builder%append( '    '//trim(dependencies(k)%name))
                                end if
                            end if
                        end do
                    end if
                    ! Link to shared app modules object library if it exists
                    if (allocated(shared_app_modules)) then
                        if (size(shared_app_modules) > 0) then
                            call builder%append( '    app_modules_obj')
                        end if
                    end if
                    call builder%append( ')')
                end if

                ! Add link libraries from executable manifest
                if (present(executable_config)) then
                    do k = 1, size(executable_config)
                        if (trim(executable_config(k)%name) == original_exe_name) then
                            if (allocated(executable_config(k)%link)) then
                                if (size(executable_config(k)%link) > 0) then
                                    call builder%append( 'target_link_libraries('//exe_name_str//' PRIVATE')
                                    do j = 1, size(executable_config(k)%link)
                                        call builder%append( '    '//trim(executable_config(k)%link(j)%s))
                                    end do
                                    call builder%append( ')')
                                end if
                            end if
                            exit
                        end if
                    end do
                end if

                ! Add base preprocessing flag (always enable -cpp for Fortran)
                call append_base_preprocessing(builder, exe_name_str, model%compiler%id, CMAKE_VISIBILITY_PRIVATE)

                ! Add preprocessing flags with macros from package-level preprocess config
                call append_preprocessing_flags(builder, exe_name_str, model%compiler%id, &
                                                preprocess, package_version, CMAKE_VISIBILITY_PRIVATE)

                ! Add metapackage settings (include dirs, link flags, and libraries)
                ! Executables are always regular targets (not INTERFACE)
                call append_metapackage_settings(builder, exe_name_str, model, .false., exe_sources)
                call builder%append( "")
            end do
        end if

        ! Test targets
        if (include_tests .and. size(tests) > 0) then
            ! Find shared test module sources before generating test executables
            call find_shared_module_sources(sources, FPM_SCOPE_TEST, shared_test_modules)

            ! Generate object library for shared test modules if any exist
            if (allocated(shared_test_modules)) then
                if (size(shared_test_modules) > 0) then
                    call builder%append( "# Shared test modules")
                    call builder%append( 'add_library(test_modules_obj OBJECT')
                    do j = 1, size(shared_test_modules)
                        call builder%append( '    '//clean_path(shared_test_modules(j)%s))
                    end do
                    call builder%append( ')')
                    call builder%append( 'set_target_properties(test_modules_obj PROPERTIES')
                    call builder%append( '    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"')
                    call builder%append( ')')
                    call builder%append( 'target_include_directories(test_modules_obj PUBLIC')
                    call builder%append( '    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>')
                    call builder%append( ')')
                    if (has_library) then
                        call builder%append( 'target_link_libraries(test_modules_obj PUBLIC '//lib_name//')')
                    end if
                    call builder%append( "")
                end if
            end if

            call builder%append( "# Tests")
            call builder%append( "enable_testing()")
            call builder%append( "")

            do i = 1, size(tests)
                original_test_name = trim(tests(i)%s)
                exe_name_str = sanitize_target_name(original_test_name)

                ! Get sources specific to this test, excluding shared modules
                if (allocated(shared_test_modules)) then
                    call get_sources_for_exe(sources, original_test_name, FPM_SCOPE_TEST, exe_sources, shared_test_modules)
                else
                    call get_sources_for_exe(sources, original_test_name, FPM_SCOPE_TEST, exe_sources)
                end if

                call builder%append( 'add_executable('//exe_name_str)
                do j = 1, size(exe_sources)
                    call builder%append( '    '//clean_path(exe_sources(j)%s))
                end do
                call builder%append( ')')

                ! Set output binary name to original if target name was sanitized
                if (exe_name_str /= original_test_name) then
                    call builder%append( 'set_target_properties('//exe_name_str//' PROPERTIES')
                    call builder%append( '    OUTPUT_NAME "'//original_test_name//'"')
                    call builder%append( ')')
                end if

                ! Set source format if not default
                if (should_set_fortran_format(fortran_config, preprocess, exe_sources)) then
                    call append_fortran_format(builder, exe_sources, &
                                               get_fortran_format_string(fortran_config), preprocess)
                end if

                ! Link test to library and dev-dependencies
                call builder%append( 'target_link_libraries('//exe_name_str//' PRIVATE')
                if (has_library) then
                    call builder%append( '    '//lib_name)
                end if
                ! Link all dependencies (both regular and dev-dependencies for tests)
                if (size(dependencies) > 0) then
                    do k = 1, size(dependencies)
                        ! For tests, link both regular and dev-dependencies
                        if (.not. has_library .or. dependencies(k)%is_dev_dependency) then
                            if (dependencies(k)%has_cmake) then
                                call builder%append( '    '//trim(dependencies(k)%name)//'::'//trim(dependencies(k)%name))
                            else
                                call builder%append( '    '//trim(dependencies(k)%name))
                            end if
                        end if
                    end do
                end if
                ! Link to shared test modules object library if it exists
                if (allocated(shared_test_modules)) then
                    if (size(shared_test_modules) > 0) then
                        call builder%append( '    test_modules_obj')
                    end if
                end if
                call builder%append( ')')

                ! Add link libraries from test manifest
                if (present(test_config)) then
                    do k = 1, size(test_config)
                        if (trim(test_config(k)%name) == original_test_name) then
                            if (allocated(test_config(k)%link)) then
                                if (size(test_config(k)%link) > 0) then
                                    call builder%append( 'target_link_libraries('//exe_name_str//' PRIVATE')
                                    do j = 1, size(test_config(k)%link)
                                        call builder%append( '    '//trim(test_config(k)%link(j)%s))
                                    end do
                                    call builder%append( ')')
                                end if
                            end if
                            exit
                        end if
                    end do
                end if

                ! Add base preprocessing flag (always enable -cpp for Fortran)
                call append_base_preprocessing(builder, exe_name_str, model%compiler%id, CMAKE_VISIBILITY_PRIVATE)

                ! Add preprocessing flags with macros from package-level preprocess config
                call append_preprocessing_flags(builder, exe_name_str, model%compiler%id, &
                                                preprocess, package_version, CMAKE_VISIBILITY_PRIVATE)

                ! Add metapackage settings (include dirs, link flags, and libraries)
                ! Tests are always regular targets (not INTERFACE)
                call append_metapackage_settings(builder, exe_name_str, model, .false., exe_sources)
                call builder%append( 'add_test(NAME '//original_test_name// &
                             ' COMMAND '//original_test_name//')')
                call builder%append( "")
            end do
        end if

    end subroutine write_cmake_content

    !> Append a line to the builder
    !> Automatically resizes with 1.5x growth when capacity is reached
    subroutine builder_append_line(self, text)
        class(line_builder_t), intent(inout) :: self
        character(len=*), intent(in) :: text

        ! Initialize on first use
        if (.not. allocated(self%lines)) then
            allocate(self%lines(0))
            self%size = 0
            self%capacity = 0
        end if

        ! Resize if at capacity
        if (self%size >= self%capacity) then
            call resize(self%lines)
            self%capacity = size(self%lines)
        end if

        ! Append to next slot
        self%size = self%size + 1
        self%lines(self%size)%s = text

    end subroutine builder_append_line

    !> Finalize and return the lines array, trimmed to exact size
    !> Resets the builder for potential reuse
    function builder_finalize(self) result(output)
        class(line_builder_t), intent(inout) :: self
        type(string_t), allocatable :: output(:)

        if (.not. allocated(self%lines)) then
            allocate(output(0))
            return
        end if

        ! Trim to exact size
        if (self%size < self%capacity) then
            call resize(self%lines, self%size)
        end if

        ! Move ownership to output
        call move_alloc(self%lines, output)

        ! Reset state
        self%size = 0
        self%capacity = 0

    end function builder_finalize

    !> Clean up path (remove leading ./ or ././)
    function clean_path(path) result(cleaned)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: cleaned

        ! Normalize backslashes to forward slashes (for Windows compatibility)
        cleaned = unix_path(path)

        ! Remove leading ././
        do while (len(cleaned) > 4 .and. cleaned(1:4) == '././')
            cleaned = cleaned(5:)
        end do

        ! Remove leading ./
        do while (len(cleaned) > 2 .and. cleaned(1:2) == './')
            cleaned = cleaned(3:)
        end do

    end function clean_path

    !> Check if a dependency has CMake support
    function has_cmake_support(dep_path) result(has_cmake)
        character(len=*), intent(in) :: dep_path
        logical :: has_cmake

        character(len=:), allocatable :: cmake_file

        cmake_file = trim(dep_path)//'/CMakeLists.txt'
        inquire(file=cmake_file, exist=has_cmake)

    end function has_cmake_support

    !> Check if target name conflicts with CMake reserved names
    !> CMake Policy CMP0037 defines reserved target names
    pure function is_cmake_reserved_name(name) result(is_reserved)
        character(len=*), intent(in) :: name
        logical :: is_reserved

        character(len=:), allocatable :: lower_name

        lower_name = lower(name)

        ! CMake reserved target names (case-insensitive)
        ! - Always reserved: all, clean, help, install
        ! - Reserved when testing enabled: test
        ! - Reserved when packaging enabled: package
        is_reserved = (lower_name == 'all') .or. &
                      (lower_name == 'clean') .or. &
                      (lower_name == 'help') .or. &
                      (lower_name == 'install') .or. &
                      (lower_name == 'test') .or. &
                      (lower_name == 'package')

    end function is_cmake_reserved_name

    !> Generate a non-conflicting CMake target name by appending _exe suffix
    pure function sanitize_target_name(name) result(sanitized)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: sanitized

        ! If no conflict, return original name
        if (.not. is_cmake_reserved_name(name)) then
            sanitized = trim(name)
        else
            ! Append _exe suffix to avoid conflict
            sanitized = trim(name)//CMAKE_EXE_SUFFIX
        end if

    end function sanitize_target_name

    !> Collect dependencies from model
    subroutine collect_dependencies(model, package, deps, used_packages)
        type(fpm_model_t), intent(in) :: model
        type(package_config_t), intent(in) :: package
        type(dependency_info_t), allocatable, intent(out) :: deps(:)
        type(string_t), intent(in), optional :: used_packages(:)

        integer :: i, n_deps, j, k, n_used_deps, n_preprocess
        logical :: found_match
        type(dependency_info_t), allocatable :: temp_deps(:), filtered_deps(:)
        type(preprocess_config_t), allocatable :: temp_preprocess(:)
        logical :: is_dev_dep
        type(package_config_t) :: dep_package
        type(error_t), allocatable :: dep_error
        character(:), allocatable :: dep_manifest_path
        logical :: has_dev_deps
        type(string_hash_map_t) :: dev_dep_map
        type(string_hash_map_t) :: package_map

        ! Count dependencies (packages 2 onwards are dependencies)
        n_deps = size(model%packages) - 1

        if (n_deps > 0) then
            allocate(temp_deps(n_deps))

            ! Build hash table for dev-dependency lookups
            has_dev_deps = .false.
            if (allocated(package%dev_dependency)) then
                if (size(package%dev_dependency) > 0) then
                    has_dev_deps = .true.
                    call dev_dep_map%init(capacity=max(16, size(package%dev_dependency) * 2))
                    do j = 1, size(package%dev_dependency)
                        call dev_dep_map%set(package%dev_dependency(j)%name, 1)
                    end do
                end if
            end if

            ! Collect dependency information
            do i = 1, n_deps
                temp_deps(i)%name = model%packages(i+1)%name

                ! Use actual dependency location from proj_dir (handles path, git, and registry deps)
                temp_deps(i)%path = model%deps%dep(i+1)%proj_dir

                ! Check if it has CMake support
                temp_deps(i)%has_cmake = has_cmake_support(temp_deps(i)%path)

                ! Check if this is a dev-dependency using hash table
                temp_deps(i)%is_dev_dependency = .false.
                if (has_dev_deps) then
                    temp_deps(i)%is_dev_dependency = dev_dep_map%contains(temp_deps(i)%name)
                end if

                ! Read dependency's manifest to get preprocessing config and version
                dep_manifest_path = trim(temp_deps(i)%path)//'/fpm.toml'
                call get_package_data(dep_package, dep_manifest_path, dep_error, apply_defaults=.true.)
                if (.not. allocated(dep_error)) then
                    ! Merge preprocessing configuration from both sources
                    ! This follows the pattern in src/fpm.f90 lines 173-185

                    ! Start with dependency's own manifest macros
                    if (allocated(dep_package%preprocess)) then
                        if (size(dep_package%preprocess) > 0) then
                            allocate(temp_deps(i)%preprocess(size(dep_package%preprocess)))
                            temp_deps(i)%preprocess = dep_package%preprocess
                        end if
                    end if

                    ! Add per-dependency macros from parent package using add_config
                    if (allocated(model%deps%dep(i+1)%preprocess)) then
                        do j = 1, size(model%deps%dep(i+1)%preprocess)
                            if (allocated(temp_deps(i)%preprocess)) then
                                ! Find matching config by name and merge
                                found_match = .false.
                                do k = 1, size(temp_deps(i)%preprocess)
                                    if (temp_deps(i)%preprocess(k)%name == &
                                        model%deps%dep(i+1)%preprocess(j)%name) then
                                        call temp_deps(i)%preprocess(k)%add_config( &
                                            model%deps%dep(i+1)%preprocess(j))
                                        found_match = .true.
                                        exit
                                    end if
                                end do
                                ! If no match found, append new config
                                if (.not. found_match) then
                                    n_preprocess = size(temp_deps(i)%preprocess)
                                    allocate(temp_preprocess(n_preprocess + 1))
                                    temp_preprocess(1:n_preprocess) = temp_deps(i)%preprocess
                                    temp_preprocess(n_preprocess + 1) = &
                                        model%deps%dep(i+1)%preprocess(j)
                                    call move_alloc(temp_preprocess, temp_deps(i)%preprocess)
                                end if
                            else
                                ! No existing configs, just allocate and copy
                                allocate(temp_deps(i)%preprocess(1))
                                temp_deps(i)%preprocess(1) = model%deps%dep(i+1)%preprocess(j)
                            end if
                        end do
                    end if

                    ! Extract version
                    temp_deps(i)%version = dep_package%version
                else
                    ! If reading manifest fails, just skip preprocessing (don't crash)
                    deallocate(dep_error)
                end if

                ! Collect sources if it doesn't have CMake
                if (.not. temp_deps(i)%has_cmake) then
                    allocate(temp_deps(i)%sources(size(model%packages(i+1)%sources)))
                    do j = 1, size(model%packages(i+1)%sources)
                        temp_deps(i)%sources(j)%s = model%packages(i+1)%sources(j)%file_name
                    end do
                end if

                ! Collect this dependency's own dependencies (sub-dependencies)
                if (allocated(model%deps%dep(i+1)%package_dep)) then
                    allocate(temp_deps(i)%depends_on(size(model%deps%dep(i+1)%package_dep)))
                    do j = 1, size(model%deps%dep(i+1)%package_dep)
                        temp_deps(i)%depends_on(j)%s = model%deps%dep(i+1)%package_dep(j)%s
                    end do
                else
                    allocate(temp_deps(i)%depends_on(0))
                end if
            end do

            ! Filter dependencies if used_packages is provided
            if (present(used_packages)) then
                ! Build hash table for O(1) package lookups
                call package_map%init(capacity=max(64, size(used_packages) * 2))
                do j = 1, size(used_packages)
                    call package_map%set(used_packages(j)%s, 1)
                end do

                ! First pass: count used dependencies (include header-only deps regardless)
                n_used_deps = 0
                do i = 1, n_deps
                    if (package_map%contains(temp_deps(i)%name) .or. &
                        is_header_only_dep(temp_deps(i))) then
                        n_used_deps = n_used_deps + 1
                    end if
                end do

                ! Second pass: collect used dependencies
                allocate(filtered_deps(n_used_deps))
                n_used_deps = 0
                do i = 1, n_deps
                    if (package_map%contains(temp_deps(i)%name) .or. &
                        is_header_only_dep(temp_deps(i))) then
                        n_used_deps = n_used_deps + 1
                        filtered_deps(n_used_deps) = temp_deps(i)
                    end if
                end do
                call move_alloc(filtered_deps, deps)
            else
                call move_alloc(temp_deps, deps)
            end if
        else
            allocate(deps(0))
        end if

        ! Cleanup
        if (has_dev_deps) call dev_dep_map%destroy()
        if (present(used_packages)) call package_map%destroy()

    end subroutine collect_dependencies

    !> Extract unique package names from build targets
    subroutine collect_used_packages(targets, used_packages)
        type(build_target_ptr), intent(in) :: targets(:)
        type(string_t), allocatable, intent(out) :: used_packages(:)

        type(string_hash_map_t) :: pkg_dedup_map
        integer :: i, n_unique
        character(len=:), allocatable :: pkg_name
        type(string_t), allocatable :: temp_packages(:)

        ! Initialize hash map for deduplication
        call pkg_dedup_map%init(capacity=64)

        ! Over-allocate output array
        allocate(used_packages(size(targets)))
        n_unique = 0

        ! Single pass: deduplicate using hash map
        do i = 1, size(targets)
            ! Extract and validate package name
            if (.not. allocated(targets(i)%ptr%package_name)) cycle
            pkg_name = targets(i)%ptr%package_name
            if (len_trim(pkg_name) == 0) cycle

            ! Check if package name has been seen before
            if (.not. pkg_dedup_map%contains(pkg_name)) then
                ! First occurrence - add to hash map and output
                call pkg_dedup_map%set(pkg_name, 1)
                n_unique = n_unique + 1
                used_packages(n_unique)%s = pkg_name
            end if
            ! Duplicate - skip
        end do

        ! Resize to actual size (using array slicing and move_alloc)
        if (n_unique > 0) then
            temp_packages = used_packages(1:n_unique)
            call move_alloc(temp_packages, used_packages)
        else
            deallocate(used_packages)
            allocate(used_packages(0))
        end if

        ! Cleanup hash map
        call pkg_dedup_map%destroy()

    end subroutine collect_used_packages


    !> Check if a dependency is header-only (no compilable sources)
    function is_header_only_dep(dep) result(is_header_only)
        type(dependency_info_t), intent(in) :: dep
        logical :: is_header_only

        integer :: i, hdr_i
        character(len=:), allocatable :: file_path
        logical :: has_compilable, is_header

        is_header_only = .false.

        ! If dependency has CMake support, assume it's not header-only for filtering purposes
        ! (CMake-enabled deps manage their own header-only status)
        if (dep%has_cmake) return

        ! Check if dependency has any compilable sources
        if (.not. allocated(dep%sources)) then
            ! No sources allocated means header-only
            is_header_only = .true.
            return
        end if

        if (size(dep%sources) == 0) then
            ! No sources means header-only
            is_header_only = .true.
            return
        end if

        ! Check if all sources are headers
        has_compilable = .false.
        do i = 1, size(dep%sources)
            file_path = lower(trim(dep%sources(i)%s))

            ! Check if the file is a header
            is_header = .false.
            do hdr_i = 1, size(c_header_suffixes)
                if (str_ends_with(file_path, trim(c_header_suffixes(hdr_i)))) then
                    is_header = .true.
                    exit
                end if
            end do

            ! If NOT a header, it's compilable
            if (.not. is_header) then
                has_compilable = .true.
                exit
            end if
        end do

        is_header_only = .not. has_compilable

    end function is_header_only_dep

    !> Append macros from a single preprocess config to builder
    subroutine append_macros_for_config(builder, config, compiler_id, version)
        type(line_builder_t), intent(inout) :: builder
        type(preprocess_config_t), intent(in) :: config
        integer(compiler_enum), intent(in) :: compiler_id
        type(version_t), intent(in), optional :: version

        character(:), allocatable :: macros_str, raw_tokens(:)
        type(string_t), allocatable :: tokens(:)
        integer :: j

        ! Get expanded and prefixed macros from get_macros()
        ! This returns e.g., " -DMACRO1 -DMACRO2=3 -DMACRO3=1"
        if (present(version)) then
            macros_str = get_macros(compiler_id, config%macros, version)
        else
            macros_str = get_macros(compiler_id, config%macros)
        end if

        ! Split space-separated macro string into individual flags
        raw_tokens = shlex_split(macros_str)

        ! Convert to string_t array
        allocate(tokens(size(raw_tokens)))
        do j = 1, size(raw_tokens)
            tokens(j)%s = raw_tokens(j)
        end do

        ! Add each macro flag (applies to all languages)
        do j = 1, size(tokens)
            if (len_trim(tokens(j)%s) > 0) then
                call builder%append('    '//trim(tokens(j)%s))
            end if
        end do

    end subroutine append_macros_for_config

    !> Generic helper to append CMake list directive (include_directories, link_directories, link_libraries)
    subroutine append_cmake_list_directive(builder, directive, target_name, visibility, items)
        type(line_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: directive
        character(len=*), intent(in) :: target_name
        character(len=*), intent(in) :: visibility
        type(string_t), intent(in) :: items(:)

        integer :: i

        if (size(items) == 0) return

        call builder%append(trim(directive)//'('//trim(target_name)//' '//trim(visibility))
        do i = 1, size(items)
            call builder%append('    '//clean_path(items(i)%s))
        end do
        call builder%append(')')

    end subroutine append_cmake_list_directive

    !> Append dependency links with visibility (PUBLIC or INTERFACE)
    subroutine append_dependency_links(builder, target_name, depends_on, visibility)
        type(line_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: target_name
        type(string_t), intent(in) :: depends_on(:)
        character(len=*), intent(in) :: visibility

        integer :: i

        if (size(depends_on) == 0) return

        call builder%append("")
        call builder%append('target_link_libraries('//trim(target_name)//' '//trim(visibility))
        do i = 1, size(depends_on)
            ! Skip self-references
            if (trim(depends_on(i)%s) /= trim(target_name)) then
                call builder%append('    '//trim(depends_on(i)%s))
            end if
        end do
        call builder%append(')')

    end subroutine append_dependency_links

    !> Check if packages(index) exists in packages(1:index-1)

    !> Categorize a link flag token
    !> Returns: 1=option, 2=libdir, 3=libname, 0=unknown (treated as option)
    function categorize_link_flag(token, is_framework_arg) result(category)
        character(len=*), intent(in) :: token
        logical, intent(in) :: is_framework_arg
        integer :: category

        if (is_framework_arg) then
            ! Token after -framework is an option argument
            category = LINK_FLAG_OPTION
        else if (trim(token) == '-framework') then
            ! -framework itself is an option
            category = LINK_FLAG_OPTION
        else if (is_library_name_flag(trim(token))) then
            category = LINK_FLAG_LIBNAME
        else if (is_library_dir_flag(trim(token))) then
            category = LINK_FLAG_LIBDIR
        else if (is_linker_option_flag(trim(token))) then
            category = LINK_FLAG_OPTION
        else
            ! Unknown flags default to linker options (safer)
            category = LINK_FLAG_UNKNOWN
        end if

    end function categorize_link_flag

    !> Generate CMakeLists.txt for an fpm-only dependency
    subroutine generate_dependency_cmake(dep, base_dir, compiler_id, error)
        type(dependency_info_t), intent(in) :: dep
        character(len=*), intent(in) :: base_dir
        integer(compiler_enum), intent(in) :: compiler_id
        type(error_t), allocatable, intent(out) :: error

        type(line_builder_t) :: builder
        character(len=:), allocatable :: cmake_file, rel_path, dep_manifest, manifest_hash
        character(len=:), allocatable :: languages
        type(string_t), allocatable :: rel_sources(:), include_dirs(:)
        integer :: i, path_len, src_count
        logical :: header_only
        character(len=:), allocatable :: visibility

        ! Builder will auto-initialize

        ! Compute manifest hash for dependency
        dep_manifest = trim(base_dir)//'/'//trim(dep%path)//'/fpm.toml'
        manifest_hash = compute_manifest_hash(dep_manifest)

        ! Header
        call builder%append( "# CMakeLists.txt generated by fpm for "//trim(dep%name))
        call builder%append( "# Manifest hash: "//manifest_hash)
        call builder%append( "cmake_minimum_required(VERSION "//CMAKE_MINIMUM_VERSION//")")
        languages = detect_languages_string(dep%sources)
        call builder%append( 'project('//trim(dep%name)//' LANGUAGES '//trim(languages)//')')
        call builder%append( "")

        ! Check if dependency is header-only using existing helper
        header_only = is_header_only_dep(dep)

        ! Prepare relative sources (strip dependency path prefix)
        if (header_only) then
            allocate(rel_sources(0))
        else
            allocate(rel_sources(size(dep%sources)))
            src_count = 0
            do i = 1, size(dep%sources)
                ! Strip the dependency path prefix to make paths relative
                ! dep%path is "build/dependencies/NAME" and we need to remove "build/dependencies/NAME/"
                path_len = len_trim(dep%path) + 1  ! +1 for the slash after the path
                if (len_trim(dep%sources(i)%s) > path_len) then
                    rel_path = trim(dep%sources(i)%s(path_len+1:))
                else
                    rel_path = trim(dep%sources(i)%s)
                end if
                src_count = src_count + 1
                rel_sources(src_count)%s = rel_path
            end do
        end if

        ! Resolve include directories for dependency
        call resolve_dep_include_dirs(dep%path, include_dirs)

        ! Generate library target (STATIC or INTERFACE)
        call write_library_target(builder, dep%name, rel_sources, &
                                 include_dirs, compiler_id, &
                                 dep%preprocess, dep%version)

        ! Determine visibility for dependency links
        if (header_only) then
            visibility = CMAKE_VISIBILITY_INTERFACE
        else
            visibility = CMAKE_VISIBILITY_PUBLIC
        end if

        ! Link to this dependency's own dependencies (sub-dependencies)
        if (allocated(dep%depends_on)) then
            call append_dependency_links(builder, dep%name, dep%depends_on, visibility)
        end if

        ! Write to file
        cmake_file = trim(base_dir)//'/'//trim(dep%path)//'/CMakeLists.txt'
        call write_lines_to_file(cmake_file, builder%finalize(), error)

    end subroutine generate_dependency_cmake

    !> Write dependency section to CMakeLists.txt
    subroutine write_cmake_dependencies(builder, deps, base_dir)
        type(line_builder_t), intent(inout) :: builder
        type(dependency_info_t), intent(in) :: deps(:)
        character(len=*), intent(in) :: base_dir

        integer :: i

        if (size(deps) == 0) return

        call builder%append( "# Dependencies")
        call builder%append( 'set(FPM_DEPENDENCIES_DIR "${CMAKE_SOURCE_DIR}/build/dependencies" '// &
                       'CACHE PATH "FPM dependencies directory")')
        call builder%append( "")
        call builder%append( "# Disable testing for all dependencies")
        call builder%append( 'set(BUILD_TESTING OFF CACHE BOOL "" FORCE)')
        call builder%append( "")

        ! Add all dependencies via add_subdirectory
        call builder%append( "# Add all dependencies")
        do i = 1, size(deps)
            call builder%append( 'if(NOT TARGET '//trim(deps(i)%name)//')')
            call builder%append( '    add_subdirectory("${CMAKE_SOURCE_DIR}/'// &
                           clean_path(deps(i)%path)//'" '//trim(deps(i)%name)//' EXCLUDE_FROM_ALL)')
            call builder%append( 'endif()')
            ! Create namespace alias for CMake-enabled dependencies
            if (deps(i)%has_cmake) then
                call builder%append( 'if(NOT TARGET '//trim(deps(i)%name)//'::'// &
                               trim(deps(i)%name)//')')
                call builder%append( '    add_library('//trim(deps(i)%name)//'::'// &
                               trim(deps(i)%name)//' ALIAS '//trim(deps(i)%name)//')')
                call builder%append( 'endif()')
            end if
        end do
        call builder%append( "")

    end subroutine write_cmake_dependencies

    !> Check if include directory exists
    function has_include_dir() result(exists)
        logical :: exists
        exists = is_dir('include')
    end function has_include_dir

    !> Check if library has include directories from manifest
    function has_include_dir_from_manifest(library_config) result(has_dirs)
        type(library_config_t), intent(in), optional :: library_config
        logical :: has_dirs

        has_dirs = .false.
        if (present(library_config)) then
            if (allocated(library_config%include_dir)) then
                has_dirs = size(library_config%include_dir) > 0
            end if
        end if
        ! Also check physical include/ directory
        if (.not. has_dirs) has_dirs = has_include_dir()
    end function has_include_dir_from_manifest

    !> Check if we should set the Fortran_FORMAT property
    function should_set_fortran_format(fortran_config, preprocess, sources) result(should_set)
        type(fortran_config_t), intent(in), optional :: fortran_config
        type(preprocess_config_t), intent(in), optional :: preprocess(:)
        type(string_t), intent(in), optional :: sources(:)
        logical :: should_set

        integer :: i

        should_set = .false.

        ! Check if there's an explicit fortran config
        if (present(fortran_config)) then
            if (allocated(fortran_config%source_form)) then
                ! Set format for both "free" and "fixed" source forms
                ! For "default", let CMake use extension-based detection
                should_set = (fortran_config%source_form == "free" .or. &
                             fortran_config%source_form == "fixed")
            end if
        end if

        ! If no explicit fortran config, check if we have custom preprocessor suffixes
        ! that need explicit FREE format declaration for CMake
        if (.not. should_set .and. present(preprocess) .and. present(sources)) then
            if (has_custom_suffix_sources(sources, preprocess)) then
                should_set = .true.
            end if
        end if
    end function should_set_fortran_format

    !> Check if sources contain files with custom preprocessor suffixes
    function has_custom_suffix_sources(sources, preprocess) result(has_custom)
        type(string_t), intent(in) :: sources(:)
        type(preprocess_config_t), intent(in) :: preprocess(:)
        logical :: has_custom

        integer :: i, j, k
        character(len=:), allocatable :: file_ext

        has_custom = .false.

        do i = 1, size(sources)
            file_ext = lower(sources(i)%s)
            ! Check if this file matches a custom preprocessor suffix
            do j = 1, size(preprocess)
                if (allocated(preprocess(j)%suffixes)) then
                    do k = 1, size(preprocess(j)%suffixes)
                        if (str_ends_with(file_ext, "."//trim(preprocess(j)%suffixes(k)%s))) then
                            has_custom = .true.
                            return
                        end if
                    end do
                end if
            end do
        end do
    end function has_custom_suffix_sources

    !> Get the Fortran format string for CMake based on source form
    function get_fortran_format_string(fortran_config) result(format_str)
        type(fortran_config_t), intent(in), optional :: fortran_config
        character(len=5) :: format_str

        format_str = FORTRAN_FORMAT_FREE  ! Default to FREE (matches fpm default)
        if (present(fortran_config)) then
            if (allocated(fortran_config%source_form)) then
                if (fortran_config%source_form == "free") then
                    format_str = FORTRAN_FORMAT_FREE
                else if (fortran_config%source_form == "fixed") then
                    format_str = FORTRAN_FORMAT_FIXED
                end if
            end if
        end if
    end function get_fortran_format_string

    !> Append set_source_files_properties command for Fortran format
    subroutine append_fortran_format(builder, sources, format, preprocess)
        type(line_builder_t), intent(inout) :: builder
        type(string_t), intent(in) :: sources(:)
        character(len=*), intent(in) :: format
        type(preprocess_config_t), intent(in), optional :: preprocess(:)

        integer :: i
        logical :: has_fortran_sources

        ! Check if there are any Fortran sources (skip C/C++ files)
        has_fortran_sources = .false.
        do i = 1, size(sources)
            if (is_fortran_source(sources(i)%s, preprocess)) then
                has_fortran_sources = .true.
                exit
            end if
        end do

        if (.not. has_fortran_sources) return

        ! Emit set_source_files_properties command
        call builder%append( 'set_source_files_properties(')
        do i = 1, size(sources)
            ! Only include Fortran sources
            if (is_fortran_source(sources(i)%s, preprocess)) then
                call builder%append( '    '//clean_path(sources(i)%s))
            end if
        end do
        call builder%append( '    PROPERTIES Fortran_FORMAT '//trim(format))
        call builder%append( ')')
    end subroutine append_fortran_format

    !> Check if a file has a Fortran extension (standard or custom preprocessor suffix)
    function is_fortran_source(filename, preprocess) result(is_fortran)
        character(len=*), intent(in) :: filename
        type(preprocess_config_t), intent(in), optional :: preprocess(:)
        logical :: is_fortran

        character(len=:), allocatable :: file_ext
        integer :: i, j

        file_ext = lower(filename)

        ! Check standard Fortran extensions first (most common case)
        do i = 1, size(fortran_suffixes)
            if (str_ends_with(file_ext, trim(fortran_suffixes(i)))) then
                is_fortran = .true.
                return
            end if
        end do

        ! Early exit if no custom preprocessor suffixes
        if (.not. present(preprocess)) then
            is_fortran = .false.
            return
        end if

        if (size(preprocess) == 0) then
            is_fortran = .false.
            return
        end if

        ! Check custom preprocessor suffixes
        do i = 1, size(preprocess)
            if (.not. allocated(preprocess(i)%suffixes)) cycle
            if (size(preprocess(i)%suffixes) == 0) cycle

            do j = 1, size(preprocess(i)%suffixes)
                if (str_ends_with(file_ext, "."//trim(preprocess(i)%suffixes(j)%s))) then
                    is_fortran = .true.
                    return
                end if
            end do
        end do

        is_fortran = .false.
    end function is_fortran_source

    !> Check if a token is a library name flag (-l*)
    function is_library_name_flag(token) result(is_lib)
        character(len=*), intent(in) :: token
        logical :: is_lib

        is_lib = str_begins_with_str(token, '-l', case_sensitive=.true.)
    end function is_library_name_flag

    !> Check if a token is a library directory flag (-L*)
    function is_library_dir_flag(token) result(is_dir)
        character(len=*), intent(in) :: token
        logical :: is_dir

        is_dir = str_begins_with_str(token, '-L', case_sensitive=.true.)
    end function is_library_dir_flag

    !> Check if a token is a linker option flag
    function is_linker_option_flag(token) result(is_option)
        character(len=*), intent(in) :: token
        logical :: is_option

        ! Linker-specific flags that should go to target_link_options()
        is_option = str_begins_with_str(token, '-Wl,', case_sensitive=.true.) .or. &
                    str_begins_with_str(token, '-Xlinker', case_sensitive=.true.) .or. &
                    str_begins_with_str(token, '-pthread', case_sensitive=.true.) .or. &
                    str_begins_with_str(token, '-framework', case_sensitive=.true.) .or. &
                    str_begins_with_str(token, '-rdynamic', case_sensitive=.true.)
    end function is_linker_option_flag

    !> Extract path from -L flag
    function extract_path(token) result(path)
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: path

        ! Remove the '-L' prefix
        if (len(token) > 2) then
            path = token(3:)
        else
            path = ""
        end if
    end function extract_path

    !> Extract library name from -l flag
    function extract_libname(token) result(libname)
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: libname

        ! Remove the '-l' prefix
        if (len(token) > 2) then
            libname = token(3:)
        else
            libname = ""
        end if
    end function extract_libname

    !> Parse link flags string into categorized components
    subroutine parse_link_flags(flags_str, parsed_flags)
        character(len=*), intent(in) :: flags_str
        type(link_flags_t), intent(out) :: parsed_flags

        character(len=:), allocatable :: tokens(:)
        type(string_t), allocatable :: temp_options(:), temp_dirs(:), temp_libs(:)
        integer :: i, n_options, n_dirs, n_libs, idx
        logical :: next_is_framework

        ! Tokenize using shlex_split
        tokens = shlex_split(flags_str)

        if (size(tokens) == 0) then
            allocate(parsed_flags%linker_options(0))
            allocate(parsed_flags%library_dirs(0))
            allocate(parsed_flags%library_names(0))
            return
        end if

        ! First pass: count each category
        n_options = 0
        n_dirs = 0
        n_libs = 0
        next_is_framework = .false.

        do i = 1, size(tokens)
            idx = categorize_link_flag(tokens(i), next_is_framework)

            select case (idx)
            case (LINK_FLAG_OPTION)
                n_options = n_options + 1
                next_is_framework = (trim(tokens(i)) == '-framework')
            case (LINK_FLAG_LIBDIR)
                n_dirs = n_dirs + 1
            case (LINK_FLAG_LIBNAME)
                n_libs = n_libs + 1
            case default  ! Unknown (treated as option)
                n_options = n_options + 1
            end select
        end do

        ! Allocate arrays
        allocate(temp_options(n_options))
        allocate(temp_dirs(n_dirs))
        allocate(temp_libs(n_libs))

        ! Second pass: populate arrays
        n_options = 0
        n_dirs = 0
        n_libs = 0
        next_is_framework = .false.

        do i = 1, size(tokens)
            idx = categorize_link_flag(tokens(i), next_is_framework)

            select case (idx)
            case (LINK_FLAG_OPTION)
                n_options = n_options + 1
                temp_options(n_options)%s = trim(tokens(i))
                next_is_framework = (trim(tokens(i)) == '-framework')
            case (LINK_FLAG_LIBDIR)
                n_dirs = n_dirs + 1
                temp_dirs(n_dirs)%s = extract_path(trim(tokens(i)))
            case (LINK_FLAG_LIBNAME)
                n_libs = n_libs + 1
                temp_libs(n_libs)%s = extract_libname(trim(tokens(i)))
            case default  ! Unknown (treated as option)
                n_options = n_options + 1
                temp_options(n_options)%s = trim(tokens(i))
            end select
        end do

        ! Move to output structure
        call move_alloc(temp_options, parsed_flags%linker_options)
        call move_alloc(temp_dirs, parsed_flags%library_dirs)
        call move_alloc(temp_libs, parsed_flags%library_names)

    end subroutine parse_link_flags

end module fpm_cmd_cmake
