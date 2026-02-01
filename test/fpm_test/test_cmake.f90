!> Define tests for the `fpm_cmd_cmake` module
module test_cmake
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_cmd_cmake, only : categorize_link_flag, extract_path, extract_libname, &
                             detect_target_language, is_fortran_source, &
                             get_fortran_format_string, link_flags_t, generate_cmake
    use fpm_strings, only : string_t
    use fpm_manifest_fortran, only : fortran_config_t
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_model, only: fpm_model_t
    use fpm, only: build_model
    use fpm_filesystem, only: mkdir, os_delete_dir, join_path, is_dir, read_lines, &
                              delete_file, get_temp_filename
    use fpm_environment, only: os_is_unix
    use fpm_command_line, only: fpm_build_settings
    use fpm_os, only: get_current_directory, change_directory
    implicit none
    private

    public :: collect_cmake

    ! Module-level temp directory constant
    character(*), parameter :: test_dir = 'fpm_test_cmake_tmp'

contains

    !> Collect all exported unit tests
    subroutine collect_cmake(testsuite)
        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("extract-path", test_extract_path), &
            & new_unittest("extract-libname", test_extract_libname), &
            & new_unittest("categorize-library-flags", test_categorize_library_flags), &
            & new_unittest("categorize-linker-options", test_categorize_linker_options), &
            & new_unittest("detect-language-fortran", test_detect_language_fortran), &
            & new_unittest("detect-language-mixed", test_detect_language_mixed), &
            & new_unittest("generate-simple-library", test_generate_simple_library), &
            & new_unittest("generate-with-executable", test_generate_with_executable), &
            & new_unittest("generate-with-dependencies", test_generate_with_dependencies) &
            & ]

    end subroutine collect_cmake

    !> Test path extraction from -L flags
    subroutine test_extract_path(error)
        type(error_t), allocatable, intent(out) :: error
        character(:), allocatable :: result

        ! Test basic path extraction
        result = extract_path("-L/usr/lib")
        if (result /= "/usr/lib") then
            call test_failed(error, "Expected '/usr/lib', got '"//result//"'")
            return
        end if

        ! Test path with spaces
        result = extract_path("-L/path/with spaces")
        if (result /= "/path/with spaces") then
            call test_failed(error, "Expected '/path/with spaces', got '"//result//"'")
            return
        end if

        ! Test minimal -L flag
        result = extract_path("-L/")
        if (result /= "/") then
            call test_failed(error, "Expected '/', got '"//result//"'")
            return
        end if

    end subroutine test_extract_path

    !> Test library name extraction from -l flags
    subroutine test_extract_libname(error)
        type(error_t), allocatable, intent(out) :: error
        character(:), allocatable :: result

        ! Test basic library name extraction
        result = extract_libname("-lm")
        if (result /= "m") then
            call test_failed(error, "Expected 'm', got '"//result//"'")
            return
        end if

        ! Test library name with multiple characters
        result = extract_libname("-lopenblas")
        if (result /= "openblas") then
            call test_failed(error, "Expected 'openblas', got '"//result//"'")
            return
        end if

        ! Test library name with dash
        result = extract_libname("-lgfortran-static")
        if (result /= "gfortran-static") then
            call test_failed(error, "Expected 'gfortran-static', got '"//result//"'")
            return
        end if

    end subroutine test_extract_libname

    !> Test categorization of library flags
    subroutine test_categorize_library_flags(error)
        type(error_t), allocatable, intent(out) :: error
        integer :: category
        logical :: is_framework_arg

        is_framework_arg = .false.

        ! Test library name flag (-l*)
        category = categorize_link_flag("-lm", is_framework_arg)
        if (category /= 3) then
            call test_failed(error, "Expected category 3 for '-lm', got category")
            return
        end if

        ! Test library directory flag (-L*)
        category = categorize_link_flag("-L/usr/lib", is_framework_arg)
        if (category /= 2) then
            call test_failed(error, "Expected category 2 for '-L/usr/lib', got different category")
            return
        end if

        ! Test another library name
        category = categorize_link_flag("-lopenblas", is_framework_arg)
        if (category /= 3) then
            call test_failed(error, "Expected category 3 for '-lopenblas', got different category")
            return
        end if

    end subroutine test_categorize_library_flags

    !> Test categorization of linker options
    subroutine test_categorize_linker_options(error)
        type(error_t), allocatable, intent(out) :: error
        integer :: category
        logical :: is_framework_arg

        is_framework_arg = .false.

        ! Test -Wl,* linker option
        category = categorize_link_flag("-Wl,--as-needed", is_framework_arg)
        if (category /= 1) then
            call test_failed(error, "Expected category 1 for '-Wl,--as-needed', got different category")
            return
        end if

        ! Test -pthread linker option
        category = categorize_link_flag("-pthread", is_framework_arg)
        if (category /= 1) then
            call test_failed(error, "Expected category 1 for '-pthread', got different category")
            return
        end if

        ! Test -framework flag
        category = categorize_link_flag("-framework", is_framework_arg)
        if (category /= 1) then
            call test_failed(error, "Expected category 1 for '-framework', got different category")
            return
        end if

        ! Test framework argument (following -framework)
        is_framework_arg = .true.
        category = categorize_link_flag("CoreFoundation", is_framework_arg)
        if (category /= 1) then
            call test_failed(error, "Expected category 1 for framework argument, got different category")
            return
        end if

    end subroutine test_categorize_linker_options

    !> Test Fortran language detection
    subroutine test_detect_language_fortran(error)
        type(error_t), allocatable, intent(out) :: error
        type(string_t), allocatable :: sources(:)
        integer :: lang

        ! Test pure Fortran sources
        allocate(sources(2))
        sources(1)%s = "src/mod.f90"
        sources(2)%s = "src/sub.f"

        lang = detect_target_language(sources)
        if (lang /= 1) then
            call test_failed(error, "Expected language 1 (Fortran) for .f90/.f files")
            return
        end if

        ! Test with .f03 extension
        deallocate(sources)
        allocate(sources(1))
        sources(1)%s = "src/program.f03"

        lang = detect_target_language(sources)
        if (lang /= 1) then
            call test_failed(error, "Expected language 1 (Fortran) for .f03 file")
            return
        end if

    end subroutine test_detect_language_fortran

    !> Test language detection with mixed sources (priority: C++ > C > Fortran)
    subroutine test_detect_language_mixed(error)
        type(error_t), allocatable, intent(out) :: error
        type(string_t), allocatable :: sources(:)
        integer :: lang

        ! Test C source
        allocate(sources(1))
        sources(1)%s = "src/foo.c"

        lang = detect_target_language(sources)
        if (lang /= 2) then
            call test_failed(error, "Expected language 2 (C) for .c file")
            return
        end if

        ! Test C++ source (.cpp)
        deallocate(sources)
        allocate(sources(1))
        sources(1)%s = "src/foo.cpp"

        lang = detect_target_language(sources)
        if (lang /= 3) then
            call test_failed(error, "Expected language 3 (C++) for .cpp file")
            return
        end if

        ! Test C++ source (.cxx)
        deallocate(sources)
        allocate(sources(1))
        sources(1)%s = "src/bar.cxx"

        lang = detect_target_language(sources)
        if (lang /= 3) then
            call test_failed(error, "Expected language 3 (C++) for .cxx file")
            return
        end if

        ! Test mixed C and Fortran (C should take priority)
        deallocate(sources)
        allocate(sources(2))
        sources(1)%s = "src/foo.c"
        sources(2)%s = "src/bar.f90"

        lang = detect_target_language(sources)
        if (lang /= 2) then
            call test_failed(error, "Expected language 2 (C) for mixed C/Fortran")
            return
        end if

        ! Test mixed C++ and C (C++ should take priority)
        deallocate(sources)
        allocate(sources(2))
        sources(1)%s = "src/foo.cpp"
        sources(2)%s = "src/bar.c"

        lang = detect_target_language(sources)
        if (lang /= 3) then
            call test_failed(error, "Expected language 3 (C++) for mixed C++/C")
            return
        end if

        ! Test all three languages (C++ should take priority)
        deallocate(sources)
        allocate(sources(3))
        sources(1)%s = "src/foo.cpp"
        sources(2)%s = "src/bar.c"
        sources(3)%s = "src/baz.f90"

        lang = detect_target_language(sources)
        if (lang /= 3) then
            call test_failed(error, "Expected language 3 (C++) for mixed C++/C/Fortran")
            return
        end if

    end subroutine test_detect_language_mixed

    !> Helper to cleanup test directory
    subroutine cleanup_test_dir()
        if (is_dir(test_dir)) call os_delete_dir(os_is_unix(), test_dir)
    end subroutine cleanup_test_dir

    !> Helper to verify CMakeLists.txt contains expected string
    function cmake_contains(lines, search_str) result(found)
        type(string_t), intent(in) :: lines(:)
        character(*), intent(in) :: search_str
        logical :: found
        integer :: i

        found = .false.
        do i = 1, size(lines)
            if (index(lines(i)%s, search_str) > 0) then
                found = .true.
                return
            end if
        end do
    end function cmake_contains

    !> Get absolute path by prepending current directory
    subroutine get_absolute_path(relative_path, absolute_path, error)
        character(*), intent(in) :: relative_path
        character(:), allocatable, intent(out) :: absolute_path
        type(error_t), allocatable, intent(out) :: error
        character(:), allocatable :: cwd

        call get_current_directory(cwd, error)
        if (allocated(error)) return
        absolute_path = trim(cwd) // '/' // relative_path
    end subroutine get_absolute_path

    !> Test generation of CMakeLists.txt for simple library package
    subroutine test_generate_simple_library(error)
        type(error_t), allocatable, intent(out) :: error
        type(package_config_t) :: package
        type(fpm_model_t) :: model
        type(fpm_build_settings) :: settings
        type(string_t), allocatable :: cmake_lines(:)
        integer :: unit
        character(len=:), allocatable :: manifest_path, src_file, abs_test_dir, cmake_path
        character(len=:), allocatable :: original_dir

        ! Save current directory
        call get_current_directory(original_dir, error)
        if (allocated(error)) return

        ! Get absolute path for test directory
        call get_absolute_path(test_dir, abs_test_dir, error)
        if (allocated(error)) return

        ! Setup: Create temp directory
        call cleanup_test_dir()
        call mkdir(test_dir)
        call mkdir(join_path(test_dir, 'src'))

        ! Write fpm.toml
        manifest_path = 'fpm.toml'
        open(newunit=unit, file=join_path(test_dir, manifest_path))
        write(unit, '(a)') 'name = "testpkg"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') '[library]'
        close(unit)

        ! Write source file
        src_file = 'src/mylib.f90'
        open(newunit=unit, file=join_path(test_dir, src_file))
        write(unit, '(a)') 'module mylib'
        write(unit, '(a)') '  implicit none'
        write(unit, '(a)') 'end module mylib'
        close(unit)

        ! Change to test directory
        call change_directory(abs_test_dir, error)
        if (allocated(error)) then
            call test_failed(error, "Failed to change to test directory: " // error%message)
            call cleanup_test_dir()
            return
        end if

        ! Parse manifest (now using relative path since we're in test_dir)
        call get_package_data(package, manifest_path, error, apply_defaults=.true.)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        ! Build model (use unique build dir for each test)
        settings%compiler = 'gfortran'
        settings%build_dir = 'build_test'
        call build_model(model, settings, package, error)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        ! Generate CMakeLists.txt
        call generate_cmake(package, model, error)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        ! Read generated file
        cmake_path = 'CMakeLists.txt'
        cmake_lines = read_lines(cmake_path)

        ! Change back to original directory
        call change_directory(original_dir, error)
        if (allocated(error)) return

        ! Verify contents
        if (.not. cmake_contains(cmake_lines, 'cmake_minimum_required')) then
            call test_failed(error, "Missing cmake_minimum_required")
            call cleanup_test_dir()
            return
        end if

        if (.not. cmake_contains(cmake_lines, 'project(testpkg')) then
            call test_failed(error, "Missing project declaration")
            call cleanup_test_dir()
            return
        end if

        if (.not. cmake_contains(cmake_lines, 'add_library(testpkg')) then
            call test_failed(error, "Missing library target")
            call cleanup_test_dir()
            return
        end if

        ! Cleanup
        call cleanup_test_dir()

    end subroutine test_generate_simple_library

    !> Test generation of CMakeLists.txt for package with executable
    subroutine test_generate_with_executable(error)
        type(error_t), allocatable, intent(out) :: error
        type(package_config_t) :: package
        type(fpm_model_t) :: model
        type(fpm_build_settings) :: settings
        type(string_t), allocatable :: cmake_lines(:)
        integer :: unit
        character(len=:), allocatable :: manifest_path, abs_test_dir, original_dir

        ! Save current directory
        call get_current_directory(original_dir, error)
        if (allocated(error)) return

        call get_absolute_path(test_dir, abs_test_dir, error)
        if (allocated(error)) return

        ! Setup
        call cleanup_test_dir()
        call mkdir(test_dir)
        call mkdir(join_path(test_dir, 'src'))
        call mkdir(join_path(test_dir, 'app'))

        ! Write fpm.toml
        manifest_path = 'fpm.toml'
        open(newunit=unit, file=join_path(test_dir, manifest_path))
        write(unit, '(a)') 'name = "testpkg"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') '[library]'
        write(unit, '(a)') '[[executable]]'
        write(unit, '(a)') 'name = "testpkg-exe"'
        close(unit)

        ! Write source files
        open(newunit=unit, file=join_path(test_dir, 'src/mylib.f90'))
        write(unit, '(a)') 'module mylib'
        write(unit, '(a)') 'end module mylib'
        close(unit)

        open(newunit=unit, file=join_path(test_dir, 'app/main.f90'))
        write(unit, '(a)') 'program main'
        write(unit, '(a)') '  use mylib'
        write(unit, '(a)') 'end program main'
        close(unit)

        ! Change to test directory
        call change_directory(abs_test_dir, error)
        if (allocated(error)) then
            call test_failed(error, "Failed to change directory: " // error%message)
            call cleanup_test_dir()
            return
        end if

        ! Parse and build
        call get_package_data(package, manifest_path, error, apply_defaults=.true.)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        settings%compiler = 'gfortran'
        settings%build_dir = 'build_test'
        call build_model(model, settings, package, error)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        call generate_cmake(package, model, error)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        cmake_lines = read_lines('CMakeLists.txt')

        ! Change back
        call change_directory(original_dir, error)
        if (allocated(error)) return

        ! Verify
        if (.not. cmake_contains(cmake_lines, 'add_library(testpkg')) then
            call test_failed(error, "Missing library target")
            call cleanup_test_dir()
            return
        end if

        if (.not. cmake_contains(cmake_lines, 'add_executable(testpkg-exe')) then
            call test_failed(error, "Missing executable target")
            call cleanup_test_dir()
            return
        end if

        ! Cleanup
        call cleanup_test_dir()

    end subroutine test_generate_with_executable

    !> Test generation of CMakeLists.txt for package with dependencies
    subroutine test_generate_with_dependencies(error)
        type(error_t), allocatable, intent(out) :: error
        type(package_config_t) :: package
        type(fpm_model_t) :: model
        type(fpm_build_settings) :: settings
        type(string_t), allocatable :: cmake_lines(:)
        integer :: unit
        character(len=:), allocatable :: manifest_path, abs_test_dir, original_dir

        ! Save current directory
        call get_current_directory(original_dir, error)
        if (allocated(error)) return

        call get_absolute_path(test_dir, abs_test_dir, error)
        if (allocated(error)) return

        ! Setup
        call cleanup_test_dir()
        call mkdir(test_dir)
        call mkdir(join_path(test_dir, 'src'))

        ! Create mock dependency
        call mkdir(join_path(test_dir, 'mock-dep'))
        call mkdir(join_path(test_dir, 'mock-dep', 'src'))

        ! Dependency manifest
        open(newunit=unit, file=join_path(test_dir, 'mock-dep/fpm.toml'))
        write(unit, '(a)') 'name = "mockdep"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') '[library]'
        close(unit)

        ! Dependency source
        open(newunit=unit, file=join_path(test_dir, 'mock-dep/src/mockdep.f90'))
        write(unit, '(a)') 'module mockdep'
        write(unit, '(a)') 'end module mockdep'
        close(unit)

        ! Main package manifest
        manifest_path = 'fpm.toml'
        open(newunit=unit, file=join_path(test_dir, manifest_path))
        write(unit, '(a)') 'name = "testpkg"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') '[library]'
        write(unit, '(a)') '[dependencies]'
        write(unit, '(a)') 'mockdep = { path = "mock-dep" }'
        close(unit)

        ! Main source
        open(newunit=unit, file=join_path(test_dir, 'src/mylib.f90'))
        write(unit, '(a)') 'module mylib'
        write(unit, '(a)') '  use mockdep'
        write(unit, '(a)') 'end module mylib'
        close(unit)

        ! Change to test directory
        call change_directory(abs_test_dir, error)
        if (allocated(error)) then
            call test_failed(error, "Failed to change directory: " // error%message)
            call cleanup_test_dir()
            return
        end if

        ! Parse and build
        call get_package_data(package, manifest_path, error, apply_defaults=.true.)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        settings%compiler = 'gfortran'
        settings%build_dir = 'build_test'
        call build_model(model, settings, package, error)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        call generate_cmake(package, model, error)
        if (allocated(error)) then
            call change_directory(original_dir, error)
            call cleanup_test_dir()
            return
        end if

        cmake_lines = read_lines('CMakeLists.txt')

        ! Change back
        call change_directory(original_dir, error)
        if (allocated(error)) return

        ! Verify dependency handling
        if (.not. (cmake_contains(cmake_lines, 'add_subdirectory') .or. &
                   cmake_contains(cmake_lines, 'mockdep'))) then
            call test_failed(error, "Missing dependency reference")
            call cleanup_test_dir()
            return
        end if

        ! Cleanup
        call cleanup_test_dir()

    end subroutine test_generate_with_dependencies

end module test_cmake
