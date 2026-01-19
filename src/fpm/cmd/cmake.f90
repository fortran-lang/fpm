!> Implementation of the `fpm generate --cmake` command
module fpm_cmd_cmake
    use fpm_command_line, only: fpm_generate_settings
    use fpm_error, only: error_t, fpm_stop
    use fpm_filesystem, only: dirname
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_model, only: fpm_model_t, srcfile_t, FPM_SCOPE_LIB, FPM_SCOPE_APP, &
                         FPM_SCOPE_TEST, FPM_SCOPE_EXAMPLE, FPM_UNIT_PROGRAM, &
                         FPM_UNIT_CSOURCE, FPM_UNIT_CPPSOURCE
    use fpm, only: build_model
    use fpm_strings, only: string_t
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    implicit none
    private
    public :: cmd_generate

    !> Type to hold dependency information
    type :: dependency_info_t
        character(:), allocatable :: name
        character(:), allocatable :: path
        logical :: has_cmake
        type(string_t), allocatable :: sources(:)
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
        type(fpm_model_t), intent(in) :: model
        type(error_t), allocatable, intent(out) :: error

        type(string_t), allocatable :: lib_sources(:), app_sources(:), test_sources(:)
        type(string_t), allocatable :: executables(:), tests(:)
        type(string_t), allocatable :: cmake_lines(:)
        type(dependency_info_t), allocatable :: dependencies(:)
        character(len=:), allocatable :: version_str
        integer :: i
        logical :: has_library

        ! Collect sources by scope from the root package (index 1)
        call collect_sources(model%packages(1)%sources, lib_sources, app_sources, &
                            test_sources, executables, tests)

        has_library = size(lib_sources) > 0

        ! Build version string
        version_str = package%version%s()
        if (version_str == '0') version_str = "0.1.0"

        ! Collect dependencies
        call collect_dependencies(model, dependencies)

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
                                dependencies)

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
                                  dependencies)
        type(string_t), allocatable, intent(out) :: lines(:)
        character(len=*), intent(in) :: name, version
        type(string_t), intent(in) :: lib_sources(:)
        type(string_t), intent(in) :: executables(:), tests(:)
        logical, intent(in) :: has_library, include_tests
        type(srcfile_t), intent(in) :: sources(:)
        type(dependency_info_t), intent(in) :: dependencies(:)

        integer :: i, j, k
        type(string_t), allocatable :: exe_sources(:)
        character(len=:), allocatable :: lib_name, exe_name_str, languages
        logical :: has_c, has_cpp

        ! Initialize empty lines array
        allocate(lines(0))

        ! Library name - if same as an executable, append _lib
        lib_name = trim(name)

        ! Detect languages used
        has_c = .false.
        has_cpp = .false.
        do k = 1, size(sources)
            if (sources(k)%unit_type == FPM_UNIT_CSOURCE) has_c = .true.
            if (sources(k)%unit_type == FPM_UNIT_CPPSOURCE) has_cpp = .true.
        end do

        ! Build languages string
        languages = 'Fortran'
        if (has_c) languages = trim(languages)//' C'
        if (has_cpp) languages = trim(languages)//' CXX'

        ! Header
        call append_line(lines, "# CMakeLists.txt generated by fpm")
        call append_line(lines, "")
        call append_line(lines, "cmake_minimum_required(VERSION 3.12)")
        call append_line(lines, 'project('//trim(name)//' VERSION '//trim(version)// &
                       ' LANGUAGES '//trim(languages)//')')
        call append_line(lines, "")

        ! Dependencies section
        call write_cmake_dependencies(lines, dependencies, '.')

        ! Library target
        if (has_library) then
            ! Check if any executable has the same name as the library
            do i = 1, size(executables)
                if (trim(executables(i)%s) == trim(name)) then
                    lib_name = trim(name)//'_lib'
                    exit
                end if
            end do

            call append_line(lines, "# Library")
            call append_line(lines, 'add_library('//lib_name)
            do i = 1, size(lib_sources)
                call append_line(lines, '    '//clean_path(lib_sources(i)%s))
            end do
            call append_line(lines, ')')

            ! Link dependencies
            if (size(dependencies) > 0) then
                call append_line(lines, 'target_link_libraries('//lib_name//' PUBLIC')
                do i = 1, size(dependencies)
                    ! Use the correct target name based on CMake support
                    if (dependencies(i)%has_cmake) then
                        ! For toml-f and jonquil, use the :: interface target
                        call append_line(lines, '    '//trim(dependencies(i)%name)//'::'//trim(dependencies(i)%name))
                    else
                        ! For fpm-only deps, use the library target directly
                        call append_line(lines, '    '//trim(dependencies(i)%name))
                    end if
                end do
                call append_line(lines, ')')
            end if

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
                if (has_library) then
                    call append_line(lines, 'target_link_libraries('//exe_name_str// &
                                 ' PRIVATE '//lib_name//')')
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
                if (has_library) then
                    call append_line(lines, 'target_link_libraries('//exe_name_str// &
                                 ' PRIVATE '//lib_name//')')
                end if
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
    subroutine collect_dependencies(model, deps)
        type(fpm_model_t), intent(in) :: model
        type(dependency_info_t), allocatable, intent(out) :: deps(:)

        integer :: i, n_deps, j
        type(dependency_info_t), allocatable :: temp_deps(:)

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

                ! Collect sources if it doesn't have CMake
                if (.not. temp_deps(i)%has_cmake) then
                    allocate(temp_deps(i)%sources(size(model%packages(i+1)%sources)))
                    do j = 1, size(model%packages(i+1)%sources)
                        temp_deps(i)%sources(j)%s = model%packages(i+1)%sources(j)%file_name
                    end do
                end if
            end do

            call move_alloc(temp_deps, deps)
        else
            allocate(deps(0))
        end if

    end subroutine collect_dependencies

    !> Generate CMakeLists.txt for an fpm-only dependency
    subroutine generate_dependency_cmake(dep, base_dir)
        type(dependency_info_t), intent(in) :: dep
        character(len=*), intent(in) :: base_dir

        type(string_t), allocatable :: lines(:)
        character(len=:), allocatable :: cmake_file, rel_path
        integer :: i, path_len

        allocate(lines(0))

        ! Header
        call append_line(lines, "# CMakeLists.txt generated by fpm for "//trim(dep%name))
        call append_line(lines, "cmake_minimum_required(VERSION 3.12)")
        call append_line(lines, 'project('//trim(dep%name)//' LANGUAGES Fortran)')
        call append_line(lines, "")

        ! Library target
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
        call append_line(lines, '    $<INSTALL_INTERFACE:include>')
        call append_line(lines, ')')

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

end module fpm_cmd_cmake
