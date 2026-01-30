!> Implementation of the `fpm generate --cmake` command
module fpm_cmd_cmake
    use fpm_command_line, only: fpm_generate_settings
    use fpm_error, only: error_t, fpm_stop
    use fpm_filesystem, only: dirname
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_manifest_library, only: library_config_t
    use fpm_manifest_preprocess, only: preprocess_config_t
    use fpm_manifest_fortran, only: fortran_config_t
    use fpm_manifest_executable, only: executable_config_t
    use fpm_manifest_test, only: test_config_t
    use fpm_model, only: fpm_model_t, srcfile_t, FPM_SCOPE_LIB, FPM_SCOPE_APP, &
                         FPM_SCOPE_TEST, FPM_SCOPE_EXAMPLE, FPM_UNIT_PROGRAM, &
                         FPM_UNIT_MODULE, FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                         FPM_UNIT_CSOURCE, FPM_UNIT_CPPSOURCE, FPM_UNIT_CHEADER
    use fpm, only: build_model
    use fpm_compiler, only: get_macros, compiler_enum, &
                        id_gcc, id_f95, id_caf, id_nvhpc, id_flang, id_amdflang, &
                        id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows, &
                        id_intel_llvm_nix, id_intel_llvm_windows, &
                        id_nag, id_lfortran
    use fpm_strings, only: string_t, lower, str_ends_with, str_begins_with_str, fnv_1a
    use fpm_targets, only: targets_from_sources, build_target_ptr
    use fpm_versioning, only: version_t
    use shlex_module, only: shlex_split => split
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, int64
    implicit none
    private
    public :: cmd_generate

    !> Type to hold dependency information
    type :: dependency_info_t
        character(:), allocatable :: name
        character(:), allocatable :: path
        logical :: has_cmake
        logical :: is_dev_dependency  ! True if this is a dev-dependency
        type(string_t), allocatable :: sources(:)
        type(string_t), allocatable :: depends_on(:)  ! Names of this dependency's own dependencies
    end type dependency_info_t

    !> Type to hold categorized link flags
    type :: link_flags_t
        type(string_t), allocatable :: linker_options(:)   ! -Wl,*, -pthread, -framework, etc.
        type(string_t), allocatable :: library_dirs(:)     ! Paths from -L/path
        type(string_t), allocatable :: library_names(:)    ! Names from -lfoo (stored without -l)
    end type link_flags_t

contains

    !> Add preprocessing flags to target (preprocessing flag + expanded macros)
    subroutine append_preprocessing_flags(lines, target_name, compiler_id, &
                                          preprocess, version, visibility)
        type(string_t), allocatable, intent(inout) :: lines(:)
        character(len=*), intent(in) :: target_name
        integer(compiler_enum), intent(in) :: compiler_id
        type(preprocess_config_t), intent(in), optional :: preprocess(:)
        type(version_t), intent(in), optional :: version
        character(len=*), intent(in), optional :: visibility

        character(:), allocatable :: cpp_flag, macros_str, vis
        character(:), allocatable :: raw_tokens(:)
        type(string_t), allocatable :: tokens(:)
        integer :: i, j
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
        vis = 'PUBLIC'
        if (present(visibility)) vis = visibility

        ! Determine preprocessing flag based on compiler ID
        select case(compiler_id)
        case default
            cpp_flag = ""
        case(id_caf, id_gcc, id_f95, id_nvhpc, id_flang, id_amdflang)
            cpp_flag = "-cpp"
        case(id_intel_classic_windows, id_intel_llvm_windows)
            cpp_flag = "/fpp"
        case(id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix, id_nag)
            cpp_flag = "-fpp"
        case(id_lfortran)
            cpp_flag = "--cpp"
        end select

        ! Start target_compile_options
        call append_line(lines, 'target_compile_options('//target_name//' '//vis)

        ! Add preprocessing flag first (if compiler needs one)
        if (len(cpp_flag) > 0) then
            call append_line(lines, '    $<$<COMPILE_LANGUAGE:Fortran>:'//cpp_flag//'>')
        end if

        ! Process each preprocess config (typically just one: cpp)
        do i = 1, size(preprocess)
            if (allocated(preprocess(i)%macros)) then
                if (size(preprocess(i)%macros) > 0) then
                    ! Get expanded and prefixed macros from get_macros()
                    ! This returns e.g., " -DMACRO1 -DMACRO2=3 -DMACRO3=1"
                    if (present(version)) then
                        macros_str = get_macros(compiler_id, preprocess(i)%macros, version)
                    else
                        macros_str = get_macros(compiler_id, preprocess(i)%macros)
                    end if

                    ! Split space-separated macro string into individual flags
                    raw_tokens = shlex_split(macros_str)

                    ! Convert to string_t array
                    allocate(tokens(size(raw_tokens)))
                    do j = 1, size(raw_tokens)
                        tokens(j)%s = raw_tokens(j)
                    end do

                    ! Add each macro flag with generator expression
                    do j = 1, size(tokens)
                        if (len_trim(tokens(j)%s) > 0) then
                            call append_line(lines, '    $<$<COMPILE_LANGUAGE:Fortran>:'// &
                                           trim(tokens(j)%s)//'>')
                        end if
                    end do
                end if
            end if
        end do

        call append_line(lines, ')')

    end subroutine append_preprocessing_flags

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
        type(string_t), allocatable :: cmake_lines(:)
        type(dependency_info_t), allocatable :: dependencies(:)
        type(string_t), allocatable :: used_package_names(:)
        type(build_target_ptr), allocatable :: targets(:)
        character(len=:), allocatable :: version_str
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
                call generate_dependency_cmake(dependencies(i), '.')
            end if
        end do

        ! Generate CMakeLists.txt content as string_t array
        call write_cmake_content(cmake_lines, package%name, version_str, &
                                lib_sources, executables, tests, has_library, &
                                model%include_tests, model%packages(1)%sources, &
                                dependencies, model, package%library, package%preprocess, &
                                package%fortran, package%executable, package%test, package%version)

        ! Write to file
        call write_lines_to_file("CMakeLists.txt", cmake_lines)

    end subroutine generate_cmake

    !> Write string_t array to file
    subroutine write_lines_to_file(filename, lines)
        character(len=*), intent(in) :: filename
        type(string_t), intent(in) :: lines(:)

        integer :: lun, i, ios
        character(len=256) :: message

        open(newunit=lun, file=filename, status='replace', action='write', &
             iostat=ios, iomsg=message)
        if (ios /= 0) return

        do i = 1, size(lines)
            write(lun, '(a)') trim(lines(i)%s)
        end do

        close(lun)

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

        integer :: i, n
        type(string_t), allocatable :: temp(:)
        character(len=:), allocatable :: exe_dir
        logical :: found_main

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

        ! Count all sources from the same directory with the same scope
        ! Exclude program files that don't match this executable
        n = 0
        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope .and. &
                dirname(sources(i)%file_name) == exe_dir) then
                ! Skip sources in the exclusion list
                if (present(exclude_sources)) then
                    if (is_in_list(sources(i)%file_name, exclude_sources)) cycle
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
                if (present(exclude_sources)) then
                    if (is_in_list(sources(i)%file_name, exclude_sources)) cycle
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

    end subroutine get_sources_for_exe

    !> Check if a file path is in a list of string_t
    function is_in_list(file_path, list) result(found)
        character(len=*), intent(in) :: file_path
        type(string_t), intent(in) :: list(:)
        logical :: found
        integer :: i

        found = .false.
        do i = 1, size(list)
            if (trim(list(i)%s) == trim(file_path)) then
                found = .true.
                exit
            end if
        end do

    end function is_in_list

    !> Find module sources shared across multiple executables in a directory
    subroutine find_shared_module_sources(sources, scope, shared_sources)
        type(srcfile_t), intent(in) :: sources(:)
        integer, intent(in) :: scope
        type(string_t), allocatable, intent(out) :: shared_sources(:)

        ! Hash table for O(n) directory lookups with dynamic resizing
        integer, parameter :: INITIAL_HASH_SIZE = 256  ! Starting size (power of 2)
        real, parameter :: LOAD_FACTOR_THRESHOLD = 0.75
        type :: directory_entry
            character(:), allocatable :: path
            integer :: program_count
        end type
        type(directory_entry), allocatable :: dir_hash_table(:)
        integer :: hash_table_size      ! Current capacity
        integer :: hash_table_count     ! Number of occupied slots

        integer :: i, j, n_shared
        character(len=:), allocatable :: dir_path
        type(string_t), allocatable :: temp_shared(:)
        integer(int64) :: hash_value
        integer :: hash_idx, probe_idx
        logical :: found_dir

        ! Initialize hash table
        hash_table_size = INITIAL_HASH_SIZE
        hash_table_count = 0
        allocate(dir_hash_table(hash_table_size))

        ! Count programs per directory using hash table - O(n)
        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope .and. &
                sources(i)%unit_type == FPM_UNIT_PROGRAM) then
                dir_path = dirname(sources(i)%file_name)

                ! Check if resize needed before insertion
                if (real(hash_table_count) / real(hash_table_size) >= LOAD_FACTOR_THRESHOLD) then
                    call resize_hash_table()
                end if

                ! Hash directory path to bucket index
                hash_value = fnv_1a(dir_path)
                hash_idx = modulo(abs(hash_value), hash_table_size) + 1

                ! Linear probe to find matching directory or empty slot
                found_dir = .false.
                do probe_idx = 0, hash_table_size - 1
                    j = modulo(hash_idx + probe_idx - 1, hash_table_size) + 1

                    if (.not. allocated(dir_hash_table(j)%path)) then
                        ! Empty slot - new directory
                        dir_hash_table(j)%path = dir_path
                        dir_hash_table(j)%program_count = 1
                        hash_table_count = hash_table_count + 1
                        exit
                    else if (trim(dir_hash_table(j)%path) == trim(dir_path)) then
                        ! Found existing directory - increment count
                        dir_hash_table(j)%program_count = dir_hash_table(j)%program_count + 1
                        found_dir = .true.
                        exit
                    end if
                    ! else: collision, try next slot
                end do
            end if
        end do

        ! Collect non-program sources from directories with 2+ programs - O(n)
        n_shared = 0
        allocate(temp_shared(size(sources)))  ! Over-allocate

        do i = 1, size(sources)
            if (sources(i)%unit_scope == scope .and. &
                sources(i)%unit_type /= FPM_UNIT_PROGRAM) then
                dir_path = dirname(sources(i)%file_name)

                ! Hash lookup for directory
                hash_value = fnv_1a(dir_path)
                hash_idx = modulo(abs(hash_value), hash_table_size) + 1

                ! Linear probe to find matching directory
                do probe_idx = 0, hash_table_size - 1
                    j = modulo(hash_idx + probe_idx - 1, hash_table_size) + 1

                    if (allocated(dir_hash_table(j)%path)) then
                        if (trim(dir_hash_table(j)%path) == trim(dir_path)) then
                            ! Found directory - check if it has 2+ programs
                            if (dir_hash_table(j)%program_count >= 2) then
                                n_shared = n_shared + 1
                                temp_shared(n_shared)%s = sources(i)%file_name
                            end if
                            exit
                        end if
                    else
                        ! Empty slot means directory not found (shouldn't happen for non-program sources)
                        exit
                    end if
                end do
            end if
        end do

        ! Copy to correctly-sized output array
        if (n_shared > 0) then
            allocate(shared_sources(n_shared))
            shared_sources(1:n_shared) = temp_shared(1:n_shared)
        else
            allocate(shared_sources(0))
        end if

    contains

        !> Resize and rehash the directory hash table (doubles size)
        subroutine resize_hash_table()
            type(directory_entry), allocatable :: old_table(:)
            integer :: old_size, new_size, i_old, j_new
            integer(int64) :: hash_value
            integer :: hash_idx, probe_idx

            ! Save old table and double size
            old_size = hash_table_size
            call move_alloc(dir_hash_table, old_table)
            new_size = old_size * 2
            hash_table_size = new_size
            allocate(dir_hash_table(new_size))

            ! Rehash all entries from old table into new table
            do i_old = 1, old_size
                if (allocated(old_table(i_old)%path)) then
                    ! Recompute hash index for new table size
                    hash_value = fnv_1a(old_table(i_old)%path)
                    hash_idx = modulo(abs(hash_value), new_size) + 1

                    ! Linear probe to find empty slot in new table
                    do probe_idx = 0, new_size - 1
                        j_new = modulo(hash_idx + probe_idx - 1, new_size) + 1

                        if (.not. allocated(dir_hash_table(j_new)%path)) then
                            ! Move entry to new location
                            call move_alloc(old_table(i_old)%path, dir_hash_table(j_new)%path)
                            dir_hash_table(j_new)%program_count = old_table(i_old)%program_count
                            exit
                        end if
                    end do
                end if
            end do

            deallocate(old_table)

        end subroutine resize_hash_table

    end subroutine find_shared_module_sources

    !> Detect primary language of target sources
    function detect_target_language(sources) result(lang)
        type(string_t), intent(in) :: sources(:)
        integer :: lang  ! 1=Fortran, 2=C, 3=C++

        integer :: i
        logical :: has_cpp, has_c, has_fortran
        character(len=:), allocatable :: lower_name

        has_fortran = .false.
        has_c = .false.
        has_cpp = .false.

        do i = 1, size(sources)
            lower_name = lower(sources(i)%s)

            ! Check file extension
            if (str_ends_with(lower_name, ".cpp") .or. &
                str_ends_with(lower_name, ".cxx") .or. &
                str_ends_with(lower_name, ".cc") .or. &
                str_ends_with(lower_name, ".c++")) then
                has_cpp = .true.
            else if (str_ends_with(lower_name, ".c")) then
                has_c = .true.
            else
                ! Assume Fortran for .f90, .f, .F90, .F, etc.
                has_fortran = .true.
            end if
        end do

        ! Priority: C++ > C > Fortran
        if (has_cpp) then
            lang = 3
        else if (has_c) then
            lang = 2
        else
            lang = 1
        end if
    end function detect_target_language

    !> Add metapackage settings (include directories, link options, and libraries) to a target
    subroutine append_metapackage_settings(lines, target_name, model, is_interface, target_sources)
        type(string_t), allocatable, intent(inout) :: lines(:)
        character(len=*), intent(in) :: target_name
        type(fpm_model_t), intent(in) :: model
        logical, intent(in), optional :: is_interface
        type(string_t), intent(in), optional :: target_sources(:)
        integer :: i, target_lang
        type(link_flags_t) :: parsed_flags
        character(len=:), allocatable :: prop_keyword, link_flags_to_use

        ! Determine property keyword based on target type
        prop_keyword = 'PRIVATE'  ! Default for regular targets
        if (present(is_interface)) then
            if (is_interface) prop_keyword = 'INTERFACE'
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
            case (1)  ! Fortran
                if (allocated(model%fortran_link_flags)) then
                    link_flags_to_use = model%fortran_link_flags
                end if
            case (2)  ! C
                if (allocated(model%c_link_flags)) then
                    link_flags_to_use = model%c_link_flags
                end if
            case (3)  ! C++
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
            if (size(model%include_dirs) > 0) then
                call append_line(lines, 'target_include_directories('//target_name//' '//prop_keyword)
                do i = 1, size(model%include_dirs)
                    call append_line(lines, '    '//trim(model%include_dirs(i)%s))
                end do
                call append_line(lines, ')')
            end if
        end if

        ! Parse and categorize link flags
        if (allocated(link_flags_to_use)) then
            if (len_trim(link_flags_to_use) > 0) then
                call parse_link_flags(link_flags_to_use, parsed_flags)

                ! Add library directories
                if (allocated(parsed_flags%library_dirs)) then
                    if (size(parsed_flags%library_dirs) > 0) then
                        call append_line(lines, 'target_link_directories('//target_name//' '//prop_keyword)
                        do i = 1, size(parsed_flags%library_dirs)
                            call append_line(lines, '    '//trim(parsed_flags%library_dirs(i)%s))
                        end do
                        call append_line(lines, ')')
                    end if
                end if

                ! Add linker options
                if (allocated(parsed_flags%linker_options)) then
                    if (size(parsed_flags%linker_options) > 0) then
                        call append_line(lines, 'target_link_options('//target_name//' '//prop_keyword)
                        do i = 1, size(parsed_flags%linker_options)
                            call append_line(lines, '    '//trim(parsed_flags%linker_options(i)%s))
                        end do
                        call append_line(lines, ')')
                    end if
                end if

                ! Add library names
                if (allocated(parsed_flags%library_names)) then
                    if (size(parsed_flags%library_names) > 0) then
                        call append_line(lines, 'target_link_libraries('//target_name//' '//prop_keyword)
                        do i = 1, size(parsed_flags%library_names)
                            call append_line(lines, '    '//trim(parsed_flags%library_names(i)%s))
                        end do
                        call append_line(lines, ')')
                    end if
                end if
            end if
        end if

        ! Add link libraries from model (e.g., openblas, lapack from non-link_flags sources)
        if (allocated(model%link_libraries)) then
            if (size(model%link_libraries) > 0) then
                call append_line(lines, 'target_link_libraries('//target_name//' '//prop_keyword)
                do i = 1, size(model%link_libraries)
                    call append_line(lines, '    '//trim(model%link_libraries(i)%s))
                end do
                call append_line(lines, ')')
            end if
        end if
    end subroutine append_metapackage_settings

    !> Write CMake content to string_t array
    subroutine write_cmake_content(lines, name, version, lib_sources, &
                                  executables, tests, has_library, include_tests, sources, &
                                  dependencies, model, library_config, preprocess, fortran_config, &
                                  executable_config, test_config, package_version)
        type(string_t), allocatable, intent(out) :: lines(:)
        character(len=*), intent(in) :: name, version
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

        integer :: i, j, k
        type(string_t), allocatable :: exe_sources(:)
        character(len=:), allocatable :: lib_name, exe_name_str, languages
        logical :: has_c, has_cpp, has_fortran
        type(string_t), allocatable :: shared_test_modules(:), shared_app_modules(:)

        ! Initialize empty lines array
        allocate(lines(0))

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
                    if (str_ends_with(lower(sources(k)%file_name), ".c")) then
                        has_c = .true.
                    else if (str_ends_with(lower(sources(k)%file_name), ".cpp")) then
                        has_cpp = .true.
                    else
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
        call append_line(lines, "# CMakeLists.txt generated by fpm")
        call append_line(lines, "")
        call append_line(lines, "cmake_minimum_required(VERSION 3.12)")
        call append_line(lines, 'project('//trim(name)//' VERSION '//trim(version)// &
                       ' LANGUAGES '//trim(languages)//')')
        call append_line(lines, "")

        ! Library target (defined before dependencies so subdirs can reference it)
        if (has_library) then
            ! Check if any executable has the same name as the library
            do i = 1, size(executables)
                if (trim(executables(i)%s) == trim(name)) then
                    lib_name = trim(name)//'_lib'
                    exit
                end if
            end do

            ! Check if this is a header-only library (has_library but no compilable sources)
            if (size(lib_sources) == 0) then
                ! Generate INTERFACE library for header-only
                call append_line(lines, "# Header-only library")
                call append_line(lines, 'add_library('//lib_name//' INTERFACE)')
                call append_line(lines, 'target_include_directories('//lib_name//' INTERFACE')
                ! Add include directories from manifest
                if (present(library_config)) then
                    if (allocated(library_config%include_dir)) then
                        do i = 1, size(library_config%include_dir)
                            call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/'// &
                                           trim(library_config%include_dir(i)%s)//'>')
                        end do
                    end if
                else
                    ! Fallback: if no library config passed, check for physical include/ directory
                    call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>')
                end if
                call append_line(lines, '    $<INSTALL_INTERFACE:include>')
                call append_line(lines, ')')

                ! Add preprocessing flags for INTERFACE library
                call append_preprocessing_flags(lines, lib_name, model%compiler%id, &
                                                preprocess, package_version, 'INTERFACE')
            else
                ! Generate STATIC library for normal libraries
                call append_line(lines, "# Library")
                call append_line(lines, 'add_library('//lib_name)
                do i = 1, size(lib_sources)
                    call append_line(lines, '    '//clean_path(lib_sources(i)%s))
                end do
                call append_line(lines, ')')

                ! Set source format if not default
                if (should_set_fortran_format(fortran_config)) then
                    call append_fortran_format(lines, lib_sources, &
                                               get_fortran_format_string(fortran_config))
                end if

                ! Set module directory properties
                call append_line(lines, 'set_target_properties('//lib_name//' PROPERTIES')
                call append_line(lines, '    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"')
                call append_line(lines, '    POSITION_INDEPENDENT_CODE ON')
                call append_line(lines, ')')
                call append_line(lines, 'target_include_directories('//lib_name//' PUBLIC')
                call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>')
                ! Add include directories from manifest
                if (present(library_config)) then
                    if (allocated(library_config%include_dir)) then
                        do i = 1, size(library_config%include_dir)
                            call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/'// &
                                           trim(library_config%include_dir(i)%s)//'>')
                        end do
                    end if
                else
                    ! Fallback: if no library config passed, check for physical include/ directory
                    if (has_include_dir()) then
                        call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>')
                    end if
                end if
                call append_line(lines, '    $<INSTALL_INTERFACE:include>')
                call append_line(lines, ')')

                ! Add preprocessing flags (preprocessing flag + expanded macros)
                call append_preprocessing_flags(lines, lib_name, model%compiler%id, &
                                                preprocess, package_version, 'PUBLIC')
            end if

            call append_line(lines, "")
        end if

        ! Dependencies section (after library so subdirs can reference it)
        call write_cmake_dependencies(lines, dependencies, '.')

        ! Link library to dependencies (after dependencies are added)
        ! Skip dev-dependencies - those are only for tests
        if (has_library .and. size(dependencies) > 0) then
            ! Use INTERFACE for header-only libraries, PUBLIC for regular libraries
            if (size(lib_sources) == 0) then
                call append_line(lines, 'target_link_libraries('//lib_name//' INTERFACE')
            else
                call append_line(lines, 'target_link_libraries('//lib_name//' PUBLIC')
            end if
            do i = 1, size(dependencies)
                ! Skip dev-dependencies (only link to regular dependencies)
                if (.not. dependencies(i)%is_dev_dependency) then
                    ! Use the correct target name based on CMake support
                    if (dependencies(i)%has_cmake) then
                        ! For toml-f and jonquil, use the :: interface target
                        call append_line(lines, '    '//trim(dependencies(i)%name)//'::'//trim(dependencies(i)%name))
                    else
                        ! For fpm-only deps, use the library target directly
                        call append_line(lines, '    '//trim(dependencies(i)%name))
                    end if
                end if
            end do
            call append_line(lines, ')')
            call append_line(lines, "")
        end if

        ! Add metapackage settings (include dirs, link flags, and libraries)
        if (has_library) then
            ! Pass .true. for header-only libraries (INTERFACE), .false. for regular libraries
            call append_metapackage_settings(lines, lib_name, model, size(lib_sources) == 0, lib_sources)
            call append_line(lines, "")
        end if

        ! Executable targets
        if (size(executables) > 0) then
            ! Find shared app module sources before generating app executables
            call find_shared_module_sources(sources, FPM_SCOPE_APP, shared_app_modules)

            ! Generate object library for shared app modules if any exist
            if (allocated(shared_app_modules)) then
                if (size(shared_app_modules) > 0) then
                    call append_line(lines, "# Shared app modules")
                    call append_line(lines, 'add_library(app_modules_obj OBJECT')
                    do j = 1, size(shared_app_modules)
                        call append_line(lines, '    '//clean_path(shared_app_modules(j)%s))
                    end do
                    call append_line(lines, ')')
                    call append_line(lines, 'set_target_properties(app_modules_obj PROPERTIES')
                    call append_line(lines, '    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"')
                    call append_line(lines, ')')
                    call append_line(lines, 'target_include_directories(app_modules_obj PUBLIC')
                    call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>')
                    call append_line(lines, ')')
                    if (has_library) then
                        call append_line(lines, 'target_link_libraries(app_modules_obj PUBLIC '//lib_name//')')
                    end if
                    call append_line(lines, "")
                end if
            end if

            call append_line(lines, "# Executables")

            do i = 1, size(executables)
                exe_name_str = trim(executables(i)%s)

                ! Get sources specific to this executable, excluding shared modules
                if (allocated(shared_app_modules)) then
                    call get_sources_for_exe(sources, exe_name_str, FPM_SCOPE_APP, exe_sources, shared_app_modules)
                else
                    call get_sources_for_exe(sources, exe_name_str, FPM_SCOPE_APP, exe_sources)
                end if

                call append_line(lines, 'add_executable('//exe_name_str)
                do j = 1, size(exe_sources)
                    call append_line(lines, '    '//clean_path(exe_sources(j)%s))
                end do
                call append_line(lines, ')')

                ! Set source format if not default
                if (should_set_fortran_format(fortran_config)) then
                    call append_fortran_format(lines, exe_sources, &
                                               get_fortran_format_string(fortran_config))
                end if

                if (has_library .or. size(dependencies) > 0 .or. &
                    (allocated(shared_app_modules) .and. size(shared_app_modules) > 0)) then
                    call append_line(lines, 'target_link_libraries('//exe_name_str//' PRIVATE')
                    if (has_library) then
                        call append_line(lines, '    '//lib_name)
                    end if
                    if (.not. has_library .and. size(dependencies) > 0) then
                        ! If no library, link directly to dependencies
                        do k = 1, size(dependencies)
                            if (dependencies(k)%has_cmake) then
                                call append_line(lines, '    '//trim(dependencies(k)%name)//'::'//trim(dependencies(k)%name))
                            else
                                call append_line(lines, '    '//trim(dependencies(k)%name))
                            end if
                        end do
                    end if
                    ! Link to shared app modules object library if it exists
                    if (allocated(shared_app_modules)) then
                        if (size(shared_app_modules) > 0) then
                            call append_line(lines, '    app_modules_obj')
                        end if
                    end if
                    call append_line(lines, ')')
                end if

                ! Add link libraries from executable manifest
                if (present(executable_config)) then
                    do k = 1, size(executable_config)
                        if (trim(executable_config(k)%name) == exe_name_str) then
                            if (allocated(executable_config(k)%link)) then
                                if (size(executable_config(k)%link) > 0) then
                                    call append_line(lines, 'target_link_libraries('//exe_name_str//' PRIVATE')
                                    do j = 1, size(executable_config(k)%link)
                                        call append_line(lines, '    '//trim(executable_config(k)%link(j)%s))
                                    end do
                                    call append_line(lines, ')')
                                end if
                            end if
                            exit
                        end if
                    end do
                end if

                ! Add metapackage settings (include dirs, link flags, and libraries)
                ! Executables are always regular targets (not INTERFACE)
                call append_metapackage_settings(lines, exe_name_str, model, .false., exe_sources)
                call append_line(lines, "")
            end do
        end if

        ! Test targets
        if (include_tests .and. size(tests) > 0) then
            ! Find shared test module sources before generating test executables
            call find_shared_module_sources(sources, FPM_SCOPE_TEST, shared_test_modules)

            ! Generate object library for shared test modules if any exist
            if (allocated(shared_test_modules)) then
                if (size(shared_test_modules) > 0) then
                    call append_line(lines, "# Shared test modules")
                    call append_line(lines, 'add_library(test_modules_obj OBJECT')
                    do j = 1, size(shared_test_modules)
                        call append_line(lines, '    '//clean_path(shared_test_modules(j)%s))
                    end do
                    call append_line(lines, ')')
                    call append_line(lines, 'set_target_properties(test_modules_obj PROPERTIES')
                    call append_line(lines, '    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"')
                    call append_line(lines, ')')
                    call append_line(lines, 'target_include_directories(test_modules_obj PUBLIC')
                    call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>')
                    call append_line(lines, ')')
                    if (has_library) then
                        call append_line(lines, 'target_link_libraries(test_modules_obj PUBLIC '//lib_name//')')
                    end if
                    call append_line(lines, "")
                end if
            end if

            call append_line(lines, "# Tests")
            call append_line(lines, "enable_testing()")
            call append_line(lines, "")

            do i = 1, size(tests)
                exe_name_str = trim(tests(i)%s)

                ! Get sources specific to this test, excluding shared modules
                if (allocated(shared_test_modules)) then
                    call get_sources_for_exe(sources, exe_name_str, FPM_SCOPE_TEST, exe_sources, shared_test_modules)
                else
                    call get_sources_for_exe(sources, exe_name_str, FPM_SCOPE_TEST, exe_sources)
                end if

                call append_line(lines, 'add_executable('//exe_name_str)
                do j = 1, size(exe_sources)
                    call append_line(lines, '    '//clean_path(exe_sources(j)%s))
                end do
                call append_line(lines, ')')

                ! Set source format if not default
                if (should_set_fortran_format(fortran_config)) then
                    call append_fortran_format(lines, exe_sources, &
                                               get_fortran_format_string(fortran_config))
                end if

                ! Link test to library and dev-dependencies
                call append_line(lines, 'target_link_libraries('//exe_name_str//' PRIVATE')
                if (has_library) then
                    call append_line(lines, '    '//lib_name)
                end if
                ! Link all dependencies (both regular and dev-dependencies for tests)
                if (size(dependencies) > 0) then
                    do k = 1, size(dependencies)
                        ! For tests, link both regular and dev-dependencies
                        if (.not. has_library .or. dependencies(k)%is_dev_dependency) then
                            if (dependencies(k)%has_cmake) then
                                call append_line(lines, '    '//trim(dependencies(k)%name)//'::'//trim(dependencies(k)%name))
                            else
                                call append_line(lines, '    '//trim(dependencies(k)%name))
                            end if
                        end if
                    end do
                end if
                ! Link to shared test modules object library if it exists
                if (allocated(shared_test_modules)) then
                    if (size(shared_test_modules) > 0) then
                        call append_line(lines, '    test_modules_obj')
                    end if
                end if
                call append_line(lines, ')')

                ! Add link libraries from test manifest
                if (present(test_config)) then
                    do k = 1, size(test_config)
                        if (trim(test_config(k)%name) == exe_name_str) then
                            if (allocated(test_config(k)%link)) then
                                if (size(test_config(k)%link) > 0) then
                                    call append_line(lines, 'target_link_libraries('//exe_name_str//' PRIVATE')
                                    do j = 1, size(test_config(k)%link)
                                        call append_line(lines, '    '//trim(test_config(k)%link(j)%s))
                                    end do
                                    call append_line(lines, ')')
                                end if
                            end if
                            exit
                        end if
                    end do
                end if

                ! Add metapackage settings (include dirs, link flags, and libraries)
                ! Tests are always regular targets (not INTERFACE)
                call append_metapackage_settings(lines, exe_name_str, model, .false., exe_sources)
                call append_line(lines, 'add_test(NAME '//exe_name_str// &
                             ' COMMAND '//exe_name_str//')')
                call append_line(lines, "")
            end do
        end if

    end subroutine write_cmake_content

    !> Append a line to the string_t array
    subroutine append_line(lines, text)
        type(string_t), allocatable, intent(inout) :: lines(:)
        character(len=*), intent(in) :: text

        type(string_t), allocatable :: temp(:)
        integer :: n

        n = size(lines)
        allocate(temp(n + 1))
        if (n > 0) temp(1:n) = lines
        temp(n + 1)%s = text
        call move_alloc(temp, lines)

    end subroutine append_line

    !> Clean up path (remove leading ./ or ././)
    function clean_path(path) result(cleaned)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: cleaned

        cleaned = path

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

    !> Collect dependencies from model
    subroutine collect_dependencies(model, package, deps, used_packages)
        type(fpm_model_t), intent(in) :: model
        type(package_config_t), intent(in) :: package
        type(dependency_info_t), allocatable, intent(out) :: deps(:)
        type(string_t), intent(in), optional :: used_packages(:)

        integer :: i, n_deps, j, k, n_used_deps
        type(dependency_info_t), allocatable :: temp_deps(:), filtered_deps(:)
        logical :: is_dev_dep

        ! Count dependencies (packages 2 onwards are dependencies)
        n_deps = size(model%packages) - 1

        if (n_deps > 0) then
            allocate(temp_deps(n_deps))

            ! Collect dependency information
            do i = 1, n_deps
                temp_deps(i)%name = model%packages(i+1)%name

                ! Use actual dependency location from proj_dir (handles path, git, and registry deps)
                temp_deps(i)%path = model%deps%dep(i+1)%proj_dir

                ! Check if it has CMake support
                temp_deps(i)%has_cmake = has_cmake_support(temp_deps(i)%path)

                ! Check if this is a dev-dependency
                temp_deps(i)%is_dev_dependency = .false.
                if (allocated(package%dev_dependency)) then
                    do k = 1, size(package%dev_dependency)
                        if (trim(package%dev_dependency(k)%name) == trim(temp_deps(i)%name)) then
                            temp_deps(i)%is_dev_dependency = .true.
                            exit
                        end if
                    end do
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
                ! First pass: count used dependencies (include header-only deps regardless)
                n_used_deps = 0
                do i = 1, n_deps
                    if (is_package_used(temp_deps(i)%name, used_packages) .or. &
                        is_header_only_dep(temp_deps(i))) then
                        n_used_deps = n_used_deps + 1
                    end if
                end do

                ! Second pass: collect used dependencies
                allocate(filtered_deps(n_used_deps))
                n_used_deps = 0
                do i = 1, n_deps
                    if (is_package_used(temp_deps(i)%name, used_packages) .or. &
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

    end subroutine collect_dependencies

    !> Extract unique package names from build targets
    subroutine collect_used_packages(targets, used_packages)
        type(build_target_ptr), intent(in) :: targets(:)
        type(string_t), allocatable, intent(out) :: used_packages(:)

        integer :: i, j, n_unique
        type(string_t), allocatable :: temp_packages(:)
        logical :: is_duplicate

        ! First pass: collect all package names (with duplicates)
        allocate(temp_packages(size(targets)))
        do i = 1, size(targets)
            if (allocated(targets(i)%ptr%package_name)) then
                temp_packages(i)%s = targets(i)%ptr%package_name
            else
                temp_packages(i)%s = ''
            end if
        end do

        ! Second pass: count unique non-empty package names
        n_unique = 0
        do i = 1, size(temp_packages)
            if (len_trim(temp_packages(i)%s) == 0) cycle
            is_duplicate = .false.
            do j = 1, i - 1
                if (trim(temp_packages(j)%s) == trim(temp_packages(i)%s)) then
                    is_duplicate = .true.
                    exit
                end if
            end do
            if (.not. is_duplicate) n_unique = n_unique + 1
        end do

        ! Third pass: collect unique package names
        allocate(used_packages(n_unique))
        n_unique = 0
        do i = 1, size(temp_packages)
            if (len_trim(temp_packages(i)%s) == 0) cycle
            is_duplicate = .false.
            do j = 1, i - 1
                if (trim(temp_packages(j)%s) == trim(temp_packages(i)%s)) then
                    is_duplicate = .true.
                    exit
                end if
            end do
            if (.not. is_duplicate) then
                n_unique = n_unique + 1
                used_packages(n_unique)%s = temp_packages(i)%s
            end if
        end do

    end subroutine collect_used_packages

    !> Check if a package name is in the used list
    function is_package_used(package_name, used_packages) result(is_used)
        character(len=*), intent(in) :: package_name
        type(string_t), intent(in) :: used_packages(:)
        logical :: is_used

        integer :: i

        is_used = .false.
        do i = 1, size(used_packages)
            if (trim(used_packages(i)%s) == trim(package_name)) then
                is_used = .true.
                exit
            end if
        end do

    end function is_package_used

    !> Check if a dependency is header-only (no compilable sources)
    function is_header_only_dep(dep) result(is_header_only)
        type(dependency_info_t), intent(in) :: dep
        logical :: is_header_only

        integer :: i
        character(len=:), allocatable :: file_path
        logical :: has_compilable

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
            file_path = trim(dep%sources(i)%s)
            ! Check if the file does NOT end with .h or .hpp (i.e., it's compilable)
            if (.not. (len(file_path) >= 2 .and. file_path(len(file_path)-1:len(file_path)) == '.h') .and. &
                .not. (len(file_path) >= 4 .and. file_path(len(file_path)-3:len(file_path)) == '.hpp')) then
                has_compilable = .true.
                exit
            end if
        end do

        is_header_only = .not. has_compilable

    end function is_header_only_dep

    !> Generate CMakeLists.txt for an fpm-only dependency
    subroutine generate_dependency_cmake(dep, base_dir)
        type(dependency_info_t), intent(in) :: dep
        character(len=*), intent(in) :: base_dir

        type(string_t), allocatable :: lines(:)
        character(len=:), allocatable :: cmake_file, rel_path
        integer :: i, path_len
        logical :: has_compilable, dep_has_include

        allocate(lines(0))

        ! Header
        call append_line(lines, "# CMakeLists.txt generated by fpm for "//trim(dep%name))
        call append_line(lines, "cmake_minimum_required(VERSION 3.12)")
        call append_line(lines, 'project('//trim(dep%name)//' LANGUAGES Fortran)')
        call append_line(lines, "")

        ! Check if dependency has any compilable sources (not just headers)
        has_compilable = .false.
        do i = 1, size(dep%sources)
            rel_path = trim(dep%sources(i)%s)
            ! Check if the file does NOT end with .h or .hpp (i.e., it's compilable)
            if (.not. (len(rel_path) >= 2 .and. rel_path(len(rel_path)-1:len(rel_path)) == '.h') .and. &
                .not. (len(rel_path) >= 4 .and. rel_path(len(rel_path)-3:len(rel_path)) == '.hpp')) then
                has_compilable = .true.
                exit
            end if
        end do

        if (has_compilable) then
            ! Library target with compilable sources
            call append_line(lines, 'add_library('//trim(dep%name)//' STATIC')
            do i = 1, size(dep%sources)
                ! Strip the dependency path prefix to make paths relative
                ! dep%path is "build/dependencies/NAME" and we need to remove "build/dependencies/NAME/"
                path_len = len_trim(dep%path) + 1  ! +1 for the slash after the path
                if (len_trim(dep%sources(i)%s) > path_len) then
                    rel_path = trim(dep%sources(i)%s(path_len+1:))
                else
                    rel_path = trim(dep%sources(i)%s)
                end if
                call append_line(lines, '    '//clean_path(rel_path))
            end do
            call append_line(lines, ')')
            call append_line(lines, "")

            ! Set properties
            call append_line(lines, 'set_target_properties('//trim(dep%name)//' PROPERTIES')
            call append_line(lines, '    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"')
            call append_line(lines, '    POSITION_INDEPENDENT_CODE ON')
            call append_line(lines, ')')
            call append_line(lines, "")

            ! Include directories
            call append_line(lines, 'target_include_directories('//trim(dep%name)//' PUBLIC')
            call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>')
            ! Check if dependency has include directory
            inquire(file=trim(dep%path)//'/include', exist=dep_has_include)
            if (dep_has_include) then
                call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>')
            end if
            call append_line(lines, '    $<INSTALL_INTERFACE:include>')
            call append_line(lines, ')')

            ! Link to this dependency's own dependencies (sub-dependencies)
            if (allocated(dep%depends_on)) then
                if (size(dep%depends_on) > 0) then
                    call append_line(lines, "")
                    call append_line(lines, 'target_link_libraries('//trim(dep%name)//' PUBLIC')
                    do i = 1, size(dep%depends_on)
                        ! Skip self-references
                        if (trim(dep%depends_on(i)%s) /= trim(dep%name)) then
                            call append_line(lines, '    '//trim(dep%depends_on(i)%s))
                        end if
                    end do
                    call append_line(lines, ')')
                end if
            end if
        else
            ! INTERFACE library for header-only dependency
            call append_line(lines, 'add_library('//trim(dep%name)//' INTERFACE)')
            call append_line(lines, 'target_include_directories('//trim(dep%name)//' INTERFACE')
            call append_line(lines, '    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>')
            call append_line(lines, '    $<INSTALL_INTERFACE:include>')
            call append_line(lines, ')')

            ! Link to this dependency's own dependencies (sub-dependencies) for INTERFACE library
            if (allocated(dep%depends_on)) then
                if (size(dep%depends_on) > 0) then
                    call append_line(lines, 'target_link_libraries('//trim(dep%name)//' INTERFACE')
                    do i = 1, size(dep%depends_on)
                        ! Skip self-references
                        if (trim(dep%depends_on(i)%s) /= trim(dep%name)) then
                            call append_line(lines, '    '//trim(dep%depends_on(i)%s))
                        end if
                    end do
                    call append_line(lines, ')')
                end if
            end if
        end if

        ! Write to file
        cmake_file = trim(base_dir)//'/'//trim(dep%path)//'/CMakeLists.txt'
        call write_lines_to_file(cmake_file, lines)

    end subroutine generate_dependency_cmake

    !> Write dependency section to CMakeLists.txt
    subroutine write_cmake_dependencies(lines, deps, base_dir)
        type(string_t), allocatable, intent(inout) :: lines(:)
        type(dependency_info_t), intent(in) :: deps(:)
        character(len=*), intent(in) :: base_dir

        integer :: i

        if (size(deps) == 0) return

        call append_line(lines, "# Dependencies")
        call append_line(lines, 'set(FPM_DEPENDENCIES_DIR "${CMAKE_SOURCE_DIR}/build/dependencies" '// &
                       'CACHE PATH "FPM dependencies directory")')
        call append_line(lines, "")
        call append_line(lines, "# Disable testing for all dependencies")
        call append_line(lines, 'set(BUILD_TESTING OFF CACHE BOOL "" FORCE)')
        call append_line(lines, "")

        ! Add all dependencies via add_subdirectory
        call append_line(lines, "# Add all dependencies")
        do i = 1, size(deps)
            call append_line(lines, 'add_subdirectory("${CMAKE_SOURCE_DIR}/'// &
                           trim(deps(i)%path)//'" '//trim(deps(i)%name)//' EXCLUDE_FROM_ALL)')
            ! Create namespace alias for CMake-enabled dependencies
            if (deps(i)%has_cmake) then
                call append_line(lines, 'if(NOT TARGET '//trim(deps(i)%name)//'::'// &
                               trim(deps(i)%name)//')')
                call append_line(lines, '    add_library('//trim(deps(i)%name)//'::'// &
                               trim(deps(i)%name)//' ALIAS '//trim(deps(i)%name)//')')
                call append_line(lines, 'endif()')
            end if
        end do
        call append_line(lines, "")

    end subroutine write_cmake_dependencies

    !> Check if include directory exists
    function has_include_dir() result(exists)
        logical :: exists
        inquire(file='include', exist=exists)
    end function has_include_dir

    !> Check if sources contain only headers (no compilable library sources)
    function is_header_only(sources) result(header_only)
        type(srcfile_t), intent(in) :: sources(:)
        logical :: header_only
        integer :: i
        logical :: has_headers, has_compilable_lib

        has_headers = .false.
        has_compilable_lib = .false.

        ! Check for headers and compilable library sources
        do i = 1, size(sources)
            ! Check if we have any C header files
            if (sources(i)%unit_type == FPM_UNIT_CHEADER) then
                has_headers = .true.
            end if
            ! Check if we have any compilable library sources (not headers)
            if (sources(i)%unit_scope == FPM_SCOPE_LIB .and. &
                sources(i)%unit_type /= FPM_UNIT_CHEADER) then
                has_compilable_lib = .true.
            end if
        end do

        ! It's header-only if we have headers but no compilable library sources
        header_only = has_headers .and. .not. has_compilable_lib
    end function is_header_only

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
    function should_set_fortran_format(fortran_config) result(should_set)
        type(fortran_config_t), intent(in), optional :: fortran_config
        logical :: should_set

        should_set = .false.
        if (present(fortran_config)) then
            if (allocated(fortran_config%source_form)) then
                ! Set format for both "free" and "fixed" source forms
                ! For "default", let CMake use extension-based detection
                should_set = (fortran_config%source_form == "free" .or. &
                             fortran_config%source_form == "fixed")
            end if
        end if
    end function should_set_fortran_format

    !> Get the Fortran format string for CMake based on source form
    function get_fortran_format_string(fortran_config) result(format_str)
        type(fortran_config_t), intent(in), optional :: fortran_config
        character(len=5) :: format_str

        format_str = 'FIXED'  ! Default fallback
        if (present(fortran_config)) then
            if (allocated(fortran_config%source_form)) then
                if (fortran_config%source_form == "free") then
                    format_str = 'FREE'
                else if (fortran_config%source_form == "fixed") then
                    format_str = 'FIXED'
                end if
            end if
        end if
    end function get_fortran_format_string

    !> Append set_source_files_properties command for Fortran format
    subroutine append_fortran_format(lines, sources, format)
        type(string_t), allocatable, intent(inout) :: lines(:)
        type(string_t), intent(in) :: sources(:)
        character(len=*), intent(in) :: format

        integer :: i
        logical :: has_fortran_sources
        character(len=:), allocatable :: file_ext

        ! Check if there are any Fortran sources (skip C/C++ files)
        has_fortran_sources = .false.
        do i = 1, size(sources)
            file_ext = lower(sources(i)%s)
            ! Check for Fortran extensions
            if (str_ends_with(file_ext, ".f90") .or. str_ends_with(file_ext, ".f") .or. &
                str_ends_with(file_ext, ".f03") .or. str_ends_with(file_ext, ".f08") .or. &
                str_ends_with(file_ext, ".f18") .or. str_ends_with(file_ext, ".for")) then
                has_fortran_sources = .true.
                exit
            end if
        end do

        if (.not. has_fortran_sources) return

        ! Emit set_source_files_properties command
        call append_line(lines, 'set_source_files_properties(')
        do i = 1, size(sources)
            file_ext = lower(sources(i)%s)
            ! Only include Fortran sources
            if (str_ends_with(file_ext, ".f90") .or. str_ends_with(file_ext, ".f") .or. &
                str_ends_with(file_ext, ".f03") .or. str_ends_with(file_ext, ".f08") .or. &
                str_ends_with(file_ext, ".f18") .or. str_ends_with(file_ext, ".for")) then
                call append_line(lines, '    '//clean_path(sources(i)%s))
            end if
        end do
        call append_line(lines, '    PROPERTIES Fortran_FORMAT '//trim(format))
        call append_line(lines, ')')
    end subroutine append_fortran_format

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
            if (next_is_framework) then
                ! The token after -framework is part of the linker option
                n_options = n_options + 1
                next_is_framework = .false.
            else if (trim(tokens(i)) == '-framework') then
                n_options = n_options + 1
                next_is_framework = .true.
            else if (is_library_name_flag(trim(tokens(i)))) then
                n_libs = n_libs + 1
            else if (is_library_dir_flag(trim(tokens(i)))) then
                n_dirs = n_dirs + 1
            else if (is_linker_option_flag(trim(tokens(i)))) then
                n_options = n_options + 1
            else
                ! Unknown flags default to linker options (safer)
                n_options = n_options + 1
            end if
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
            if (next_is_framework) then
                ! Append framework name to previous -framework token
                n_options = n_options + 1
                temp_options(n_options)%s = trim(tokens(i))
                next_is_framework = .false.
            else if (trim(tokens(i)) == '-framework') then
                n_options = n_options + 1
                temp_options(n_options)%s = '-framework'
                next_is_framework = .true.
            else if (is_library_name_flag(trim(tokens(i)))) then
                n_libs = n_libs + 1
                temp_libs(n_libs)%s = extract_libname(trim(tokens(i)))
            else if (is_library_dir_flag(trim(tokens(i)))) then
                n_dirs = n_dirs + 1
                temp_dirs(n_dirs)%s = extract_path(trim(tokens(i)))
            else if (is_linker_option_flag(trim(tokens(i)))) then
                n_options = n_options + 1
                temp_options(n_options)%s = trim(tokens(i))
            else
                ! Unknown flags go to linker options
                n_options = n_options + 1
                temp_options(n_options)%s = trim(tokens(i))
            end if
        end do

        ! Move to output structure
        call move_alloc(temp_options, parsed_flags%linker_options)
        call move_alloc(temp_dirs, parsed_flags%library_dirs)
        call move_alloc(temp_libs, parsed_flags%library_names)

    end subroutine parse_link_flags

end module fpm_cmd_cmake
