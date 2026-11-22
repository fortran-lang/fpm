!> Tests for fypp preprocessing: manifest options and output naming rules
module test_fypp
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed, check_string
    use fpm_filesystem, only : get_temp_filename
    use fpm_manifest, only : package_config_t, get_package_data
    use fpm_manifest_preprocess, only : preprocess_config_t
    use fpm_strings, only : string_t
    use fpm_fypp, only : fypp_output_suffix, glob_match
    implicit none
    private

    public :: collect_fypp

contains

    !> Collect all exported unit tests
    subroutine collect_fypp(tests)
        type(unittest_t), allocatable, intent(out) :: tests(:)

        tests = [ &
            & new_unittest("manifest-fypp-options", test_manifest_options), &
            & new_unittest("manifest-fypp-default-suffix", test_manifest_default_suffix), &
            & new_unittest("manifest-fypp-bad-suffix", test_manifest_bad_suffix, should_fail=.true.), &
            & new_unittest("manifest-cpp-only-no-fypp", test_manifest_cpp_only), &
            & new_unittest("merged-config", test_merged_config), &
            & new_unittest("glob-match", test_glob_match), &
            & new_unittest("output-suffix-rules", test_output_suffix) &
            ]

    end subroutine collect_fypp

    !> Read a manifest made of the given lines
    subroutine read_manifest(lines, package, error)
        character(*), intent(in) :: lines(:)
        type(package_config_t), intent(out) :: package
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: manifest
        integer :: unit, i

        manifest = get_temp_filename()
        open(file=manifest, newunit=unit)
        write(unit, '(a)') 'name = "example"'
        do i = 1, size(lines)
            write(unit, '(a)') trim(lines(i))
        end do
        close(unit)

        call get_package_data(package, manifest, error)

        open(file=manifest, newunit=unit)
        close(unit, status='delete')

    end subroutine read_manifest

    !> Find the fypp table of a package
    subroutine get_fypp(package, fypp, error)
        type(package_config_t), intent(in) :: package
        type(preprocess_config_t), intent(out) :: fypp
        type(error_t), allocatable, intent(out) :: error
        integer :: i

        if (allocated(package%preprocess)) then
            do i = 1, size(package%preprocess)
                if (package%preprocess(i)%is_fypp()) then
                    fypp = package%preprocess(i)
                    return
                end if
            end do
        end if
        call test_failed(error, "no fypp preprocessor in package")

    end subroutine get_fypp

    subroutine test_manifest_options(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        type(preprocess_config_t) :: fypp

        call read_manifest([character(60) :: &
            & '[preprocess]', &
            & '[preprocess.fypp]', &
            & 'macros = ["MAXRANK=4"]', &
            & 'output_suffix = "f90"', &
            & 'force_cpp_preprocessing = ["mod_a", "*_cpp"]', &
            & 'skip_cpp_preprocessing = ["mod_b"]'], package, error)
        if (allocated(error)) return

        call get_fypp(package, fypp, error)
        if (allocated(error)) return

        call check_string(error, fypp%output_suffix, "f90", "output_suffix")
        if (allocated(error)) return
        if (.not.allocated(fypp%force_cpp_preprocessing)) then
            call test_failed(error, "force_cpp_preprocessing was not read")
            return
        end if
        if (size(fypp%force_cpp_preprocessing) /= 2) then
            call test_failed(error, "force_cpp_preprocessing should have 2 entries")
            return
        end if
        call check_string(error, fypp%force_cpp_preprocessing(2)%s, "*_cpp", "force_cpp_preprocessing(2)")
        if (allocated(error)) return
        if (.not.allocated(fypp%skip_cpp_preprocessing)) then
            call test_failed(error, "skip_cpp_preprocessing was not read")
            return
        end if
        call check_string(error, fypp%skip_cpp_preprocessing(1)%s, "mod_b", "skip_cpp_preprocessing(1)")
        if (allocated(error)) return

        call package%test_serialization('test_fypp_manifest_options', error)

    end subroutine test_manifest_options

    subroutine test_manifest_default_suffix(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        type(preprocess_config_t) :: fypp

        call read_manifest([character(30) :: '[preprocess]', '[preprocess.fypp]'], package, error)
        if (allocated(error)) return
        call get_fypp(package, fypp, error)
        if (allocated(error)) return
        if (.not.fypp%has_fypp()) then
            call test_failed(error, "fypp should be enabled")
            return
        end if
        call check_string(error, fypp%output_suffix, "F90", "default output_suffix")

    end subroutine test_manifest_default_suffix

    subroutine test_manifest_bad_suffix(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package

        call read_manifest([character(30) :: '[preprocess]', '[preprocess.fypp]', &
                            'output_suffix = "f"'], package, error)

    end subroutine test_manifest_bad_suffix

    !> A cpp table must not enable fypp, even with fypp-only keys in it
    subroutine test_manifest_cpp_only(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        type(preprocess_config_t) :: merged
        integer :: i

        call read_manifest([character(60) :: &
            & '[preprocess]', &
            & '[preprocess.cpp]', &
            & 'output_suffix = "f90"', &
            & 'force_cpp_preprocessing = ["src/a.F90"]'], package, error)
        if (allocated(error)) return

        do i = 1, size(package%preprocess)
            call merged%add_config(package%preprocess(i))
        end do
        if (merged%has_fypp()) call test_failed(error, "a cpp-only table enabled fypp")

    end subroutine test_manifest_cpp_only

    !> cpp + fypp tables merge into one package configuration
    subroutine test_merged_config(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        type(preprocess_config_t) :: merged
        integer :: i

        call read_manifest([character(60) :: &
            & '[preprocess]', &
            & '[preprocess.cpp]', &
            & 'macros = ["MAXRANK=7"]', &
            & '[preprocess.fypp]', &
            & 'macros = ["VERSION=1"]', &
            & 'output_suffix = "f90"', &
            & 'force_cpp_preprocessing = ["mod_a"]'], package, error)
        if (allocated(error)) return

        do i = 1, size(package%preprocess)
            call merged%add_config(package%preprocess(i))
        end do

        if (.not.merged%has_fypp()) then
            call test_failed(error, "merged configuration should have fypp enabled")
            return
        end if
        if (size(merged%macros) /= 2) then
            call test_failed(error, "merged configuration should have the cpp and fypp macros")
            return
        end if
        call check_string(error, merged%output_suffix, "f90", "merged output_suffix")
        if (allocated(error)) return
        call check_suffix(error, "math/mod_a.fypp", merged, "F90", "mod_a suffix")

    end subroutine test_merged_config

    subroutine test_glob_match(error)
        type(error_t), allocatable, intent(out) :: error

        call expect(glob_match("stdlib_math", "stdlib_math"), "exact")
        call expect(.not.glob_match("stdlib_math", "stdlib_mat"), "prefix is not a match")
        call expect(glob_match("stdlib_math", "stdlib_*"), "prefix*")
        call expect(glob_match("stdlib_math", "*_math"), "*suffix")
        call expect(glob_match("stdlib_math", "*lib_m*"), "*middle*")
        call expect(glob_match("stdlib_math", "std*_m?th"), "* and ?")
        call expect(glob_match("", "*"), "empty string, *")
        call expect(.not.glob_match("abc", ""), "non-empty string, empty pattern")
        call expect(.not.glob_match("stdlib_math", "*_sort"), "*suffix mismatch")

    contains

        subroutine expect(condition, what)
            logical, intent(in) :: condition
            character(*), intent(in) :: what
            if (allocated(error)) return
            if (.not.condition) call test_failed(error, "glob_match failed: "//what)
        end subroutine expect

    end subroutine test_glob_match

    subroutine test_output_suffix(error)
        type(error_t), allocatable, intent(out) :: error

        type(preprocess_config_t) :: config

        config%name = "fypp"
        config%output_suffix = "f90"
        config%force_cpp_preprocessing = [string_t("stdlib_math"), string_t("sorting/*_sort"), &
                                          string_t("both")]
        config%skip_cpp_preprocessing = [string_t("legacy.fypp"), string_t("both")]

        call check_suffix(error, "plain.fypp", config, "f90", "default")
        if (allocated(error)) return
        call check_suffix(error, "math/stdlib_math.fypp", config, "F90", &
                          "force by stem")
        if (allocated(error)) return
        call check_suffix(error, "sorting/stdlib_sort.fypp", config, "F90", &
                          "force by relative path")
        if (allocated(error)) return
        call check_suffix(error, "other/stdlib_sort.fypp", config, "f90", &
                          "relative path pattern does not match another directory")
        if (allocated(error)) return
        call check_suffix(error, "both.fypp", config, "F90", &
                          "force wins over skip")
        if (allocated(error)) return

        config%output_suffix = "F90"
        call check_suffix(error, "io/legacy.fypp", config, "f90", &
                          "skip by file name")

    end subroutine test_output_suffix

    !> Check the output suffix chosen for a `.fypp` file
    subroutine check_suffix(error, rel_path, config, expected, name)
        type(error_t), allocatable, intent(out) :: error
        character(*), intent(in) :: rel_path, expected, name
        type(preprocess_config_t), intent(in) :: config

        character(:), allocatable :: actual, expected_

        actual = fypp_output_suffix(rel_path, config)
        expected_ = expected
        call check_string(error, actual, expected_, name)

    end subroutine check_suffix

end module test_fypp
