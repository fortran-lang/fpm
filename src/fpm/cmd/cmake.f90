!> Implementation of the `fpm generate --cmake` command
module fpm_cmd_cmake
    use fpm_command_line, only: fpm_generate_settings
    use fpm_error, only: error_t, fpm_stop
    use fpm_filesystem, only: dirname
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_manifest_library, only: library_config_t
    use fpm_manifest_preprocess, only: preprocess_config_t
    use fpm_model, only: fpm_model_t, srcfile_t, FPM_SCOPE_LIB, FPM_SCOPE_APP, &
                         FPM_SCOPE_TEST, FPM_SCOPE_EXAMPLE, FPM_UNIT_PROGRAM, &
                         FPM_UNIT_MODULE, FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                         FPM_UNIT_CSOURCE, FPM_UNIT_CPPSOURCE, FPM_UNIT_CHEADER
    use fpm, only: build_model
    use fpm_strings, only: string_t, lower, str_ends_with
    use fpm_targets, only: targets_from_sources, build_target_ptr
    use fpm_filesystem, only: read_lines
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
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

contains

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
                                dependencies, package%library, package%preprocess)

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
    subroutine get_sources_for_exe(sources, exe_name, scope, result_sources)
        type(srcfile_t), intent(in) :: sources(:)
        character(len=*), intent(in) :: exe_name
        integer, intent(in) :: scope
        type(string_t), allocatable, intent(out) :: result_sources(:)

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

    !> Write CMake content to string_t array
    subroutine write_cmake_content(lines, name, version, lib_sources, &
                                  executables, tests, has_library, include_tests, sources, &
                                  dependencies, library_config, preprocess)
        type(string_t), allocatable, intent(out) :: lines(:)
        character(len=*), intent(in) :: name, version
        type(string_t), intent(in) :: lib_sources(:)
        type(string_t), intent(in) :: executables(:), tests(:)
        logical, intent(in) :: has_library, include_tests
        type(srcfile_t), intent(in) :: sources(:)
        type(dependency_info_t), intent(in) :: dependencies(:)
        type(library_config_t), intent(in), optional :: library_config
        type(preprocess_config_t), intent(in), optional :: preprocess(:)

        integer :: i, j, k
        type(string_t), allocatable :: exe_sources(:)
        character(len=:), allocatable :: lib_name, exe_name_str, languages
        logical :: has_c, has_cpp, has_fortran

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

                ! Add preprocessor definitions for INTERFACE library
                if (present(preprocess)) then
                    do i = 1, size(preprocess)
                        if (allocated(preprocess(i)%macros)) then
                            if (size(preprocess(i)%macros) > 0) then
                                call append_line(lines, 'target_compile_definitions('//lib_name//' INTERFACE')
                                do j = 1, size(preprocess(i)%macros)
                                    call append_line(lines, '    '//trim(preprocess(i)%macros(j)%s))
                                end do
                                call append_line(lines, ')')
                            end if
                        end if
                    end do
                end if
            else
                ! Generate STATIC library for normal libraries
                call append_line(lines, "# Library")
                call append_line(lines, 'add_library('//lib_name)
                do i = 1, size(lib_sources)
                    call append_line(lines, '    '//clean_path(lib_sources(i)%s))
                end do
                call append_line(lines, ')')

                ! Set module directory properties
                call append_line(lines, 'set_target_properties('//lib_name//' PROPERTIES')
                call append_line(lines, '    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"')
                call append_line(lines, '    POSITION_INDEPENDENT_CODE ON')
                call append_line(lines, ')')

                ! Enable preprocessing for files with CPP directives
                do i = 1, size(lib_sources)
                    if (file_has_cpp_directives(lib_sources(i)%s)) then
                        call append_line(lines, 'set_source_files_properties('//clean_path(lib_sources(i)%s))
                        call append_line(lines, '    PROPERTIES COMPILE_FLAGS "-cpp"')
                        call append_line(lines, ')')
                    end if
                end do
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

                ! Add preprocessor definitions
                if (present(preprocess)) then
                    do i = 1, size(preprocess)
                        if (allocated(preprocess(i)%macros)) then
                            if (size(preprocess(i)%macros) > 0) then
                                call append_line(lines, 'target_compile_definitions('//lib_name//' PUBLIC')
                                do j = 1, size(preprocess(i)%macros)
                                    call append_line(lines, '    '//trim(preprocess(i)%macros(j)%s))
                                end do
                                call append_line(lines, ')')
                            end if
                        end if
                    end do
                end if
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

        ! Executable targets
        if (size(executables) > 0) then
            call append_line(lines, "# Executables")

            do i = 1, size(executables)
                exe_name_str = trim(executables(i)%s)

                ! Get sources specific to this executable
                call get_sources_for_exe(sources, exe_name_str, FPM_SCOPE_APP, exe_sources)

                call append_line(lines, 'add_executable('//exe_name_str)
                do j = 1, size(exe_sources)
                    call append_line(lines, '    '//clean_path(exe_sources(j)%s))
                end do
                call append_line(lines, ')')

                ! Enable preprocessing for files with CPP directives
                do j = 1, size(exe_sources)
                    if (file_has_cpp_directives(exe_sources(j)%s)) then
                        call append_line(lines, 'set_source_files_properties('//clean_path(exe_sources(j)%s))
                        call append_line(lines, '    PROPERTIES COMPILE_FLAGS "-cpp"')
                        call append_line(lines, ')')
                    end if
                end do

                if (has_library) then
                    call append_line(lines, 'target_link_libraries('//exe_name_str// &
                                 ' PRIVATE '//lib_name//')')
                else if (size(dependencies) > 0) then
                    ! If no library, link directly to dependencies
                    call append_line(lines, 'target_link_libraries('//exe_name_str//' PRIVATE')
                    do k = 1, size(dependencies)
                        if (dependencies(k)%has_cmake) then
                            call append_line(lines, '    '//trim(dependencies(k)%name)//'::'//trim(dependencies(k)%name))
                        else
                            call append_line(lines, '    '//trim(dependencies(k)%name))
                        end if
                    end do
                    call append_line(lines, ')')
                end if
                call append_line(lines, "")
            end do
        end if

        ! Test targets
        if (include_tests .and. size(tests) > 0) then
            call append_line(lines, "# Tests")
            call append_line(lines, "enable_testing()")
            call append_line(lines, "")

            do i = 1, size(tests)
                exe_name_str = trim(tests(i)%s)

                ! Get sources specific to this test
                call get_sources_for_exe(sources, exe_name_str, FPM_SCOPE_TEST, exe_sources)

                call append_line(lines, 'add_executable('//exe_name_str)
                do j = 1, size(exe_sources)
                    call append_line(lines, '    '//clean_path(exe_sources(j)%s))
                end do
                call append_line(lines, ')')

                ! Enable preprocessing for files with CPP directives
                do j = 1, size(exe_sources)
                    if (file_has_cpp_directives(exe_sources(j)%s)) then
                        call append_line(lines, 'set_source_files_properties('//clean_path(exe_sources(j)%s))
                        call append_line(lines, '    PROPERTIES COMPILE_FLAGS "-cpp"')
                        call append_line(lines, ')')
                    end if
                end do

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
                call append_line(lines, ')')
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

            ! Enable preprocessing for files with CPP directives
            do i = 1, size(dep%sources)
                if (file_has_cpp_directives(dep%sources(i)%s)) then
                    ! Strip the dependency path prefix to make path relative
                    path_len = len_trim(dep%path) + 1
                    if (len_trim(dep%sources(i)%s) > path_len) then
                        rel_path = trim(dep%sources(i)%s(path_len+1:))
                    else
                        rel_path = trim(dep%sources(i)%s)
                    end if
                    call append_line(lines, 'set_source_files_properties('//clean_path(rel_path))
                    call append_line(lines, '    PROPERTIES COMPILE_FLAGS "-cpp"')
                    call append_line(lines, ')')
                end if
            end do

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

    !> Check if a source file contains CPP directives
    function file_has_cpp_directives(filename) result(has_directives)
        character(len=*), intent(in) :: filename
        logical :: has_directives

        type(string_t), allocatable :: file_lines(:)
        integer :: i
        character(:), allocatable :: line_lower

        has_directives = .false.

        ! Only check Fortran source files (not C/C++)
        if (str_ends_with(lower(filename), '.c') .or. &
            str_ends_with(lower(filename), '.cpp') .or. &
            str_ends_with(lower(filename), '.h') .or. &
            str_ends_with(lower(filename), '.hpp')) then
            return
        end if

        file_lines = read_lines(filename)
        if (.not. allocated(file_lines)) return

        do i = 1, size(file_lines)
            line_lower = lower(adjustl(file_lines(i)%s))
            if (index(line_lower, '#ifdef') == 1 .or. &
                index(line_lower, '#ifndef') == 1 .or. &
                index(line_lower, '#if ') == 1 .or. &
                index(line_lower, '#elif') == 1 .or. &
                index(line_lower, '#else') == 1 .or. &
                index(line_lower, '#endif') == 1 .or. &
                index(line_lower, '#define') == 1) then
                has_directives = .true.
                return
            end if
        end do
    end function file_has_cpp_directives

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

end module fpm_cmd_cmake
