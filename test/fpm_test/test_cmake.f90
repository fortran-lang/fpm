!> Define tests for the `fpm_cmd_cmake` module
module test_cmake
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_cmd_cmake, only : categorize_link_flag, extract_path, extract_libname, &
                             detect_target_language, is_fortran_source, &
                             get_fortran_format_string, link_flags_t
    use fpm_strings, only : string_t
    use fpm_manifest_fortran, only : fortran_config_t
    implicit none
    private

    public :: collect_cmake

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
            & new_unittest("detect-language-mixed", test_detect_language_mixed) &
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

end module test_cmake
