!> Unit tests for FPM feature and feature collection functionality
module test_features
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_manifest_feature, only: feature_config_t
    use fpm_manifest_feature_collection, only: feature_collection_t
    use fpm_manifest_platform, only: platform_config_t
    use fpm_environment, only: OS_ALL, OS_LINUX, OS_MACOS, OS_WINDOWS
    use fpm_compiler, only: id_all, id_gcc, id_intel_classic_nix, id_intel_classic_windows, id_intel_llvm_nix, match_compiler_type
    use fpm_strings, only: string_t
    use fpm_filesystem, only: get_temp_filename
    implicit none
    private

    public :: collect_features

contains

    !> Collect all feature tests
    subroutine collect_features(testsuite)
        !> Collection of tests
        type(unittest_t), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            & new_unittest("feature-collection-basic", test_feature_collection_basic), &
            & new_unittest("feature-collection-flexible", test_feature_collection_flexible), &
            & new_unittest("feature-collection-invalid", test_feature_collection_invalid, should_fail=.true.), &
            & new_unittest("feature-collection-duplicates", test_feature_collection_duplicates, should_fail=.true.), &
            & new_unittest("feature-collection-extract", test_feature_collection_extract), &
            & new_unittest("feature-collection-platform-validation", test_feature_collection_platform_validation, should_fail=.true.), &
            & new_unittest("feature-collection-complex", test_feature_collection_complex), &
            & new_unittest("feature-allocatable-conflict", test_feature_allocatable_conflict, should_fail=.true.), &
            & new_unittest("feature-flag-addition", test_feature_flag_addition), &
            & new_unittest("feature-metapackage-addition", test_feature_metapackage_addition), &
            & new_unittest("feature-extract-gfortran-linux", test_feature_extract_gfortran_linux), &
            & new_unittest("feature-extract-ifort-windows", test_feature_extract_ifort_windows), &
            & new_unittest("feature-extract-dependencies-examples", test_feature_extract_dependencies_examples), &
            & new_unittest("feature-extract-build-configs", test_feature_extract_build_configs), &
            & new_unittest("feature-extract-test-configs", test_feature_extract_test_configs), &
            & new_unittest("feature-extract-example-configs", test_feature_extract_example_configs) &
            & ]

    end subroutine collect_features

    !> Test basic feature collection functionality
    !> This should create two variants: gfortran+OS_ALL and ifort+OS_ALL (NOT duplicates)
    subroutine test_feature_collection_basic(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit,i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "feature-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'debug.gfortran.flags = "-Wall -g -fcheck=bounds"', &
            & 'debug.ifort.flags = "/warn:all /check:all /traceback"', &
            & 'release.flags = "-O3"'
        close(unit)

        call get_package_data(package, temp_file, error)

        if (allocated(error)) return

        ! Check that feature collections were created
        if (.not. allocated(package%features)) then
            call test_failed(error, "Feature collections were not created")
            return
        end if
        
        ! Verify we have at least one collection
        if (size(package%features) /= 2) then
            call test_failed(error, "Invalid feature collections found, should be 2")
            return
        end if

        ! Check that the debug collection has variants
        do i=1,2
            
            if (package%features(i)%base%name=="debug") then 
        
                if (.not. allocated(package%features(i)%variants)) then
                    call test_failed(error, "Debug collection variants were not created")
                    return
                end if

                ! Should have exactly 2 variants: gfortran and ifort
                if (size(package%features(i)%variants) /= 2) then
                    call test_failed(error, "Debug collection should have exactly 2 variants")
                    return
                end if
                
                ! Check that variants have different platform configurations
                if (package%features(i)%variants(1)%platform == package%features(i)%variants(2)%platform) then
                    call test_failed(error, "Variants should have different platform configurations: " &
                                   //"variant1.compiler="//package%features(i)%variants(1)%platform%compiler_name() &
                                   //" variant1.os="//package%features(i)%variants(1)%platform%os_name() &
                                   //" variant2.compiler="//package%features(i)%variants(2)%platform%compiler_name() &
                                   //" variant2.os="//package%features(i)%variants(2)%platform%os_name())
                    return
                end if
                
            else
                
                if (allocated(package%features(i)%variants)) then
                    if (size(package%features(i)%variants) > 0) then
                    call test_failed(error, "Release collection variants should not be created")
                    return
                    endif
                end if
            endif
        
        end do

    end subroutine test_feature_collection_basic

    !> Test flexible feature collection parsing with OS and compiler constraints
    subroutine test_feature_collection_flexible(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "flexible-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'myfeature.linux.gfortran.flags = "-fPIC -Wall"', &
            & 'myfeature.windows.ifort.flags = "/fPIC /warn:all"', &
            & 'myfeature.macos.flags = "-framework CoreFoundation"', &
            & 'myfeature.preprocess.cpp.macros = ["-DMYFEATURE"]'
        close(unit)

        call get_package_data(package, temp_file, error)

        if (allocated(error)) return

        ! Check that feature collections were created
        if (.not. allocated(package%features)) then
            call test_failed(error, "Feature collections were not created for flexible test")
            return
        end if

        ! Verify we have at least one collection
        if (size(package%features) < 1) then
            call test_failed(error, "No feature collections found in flexible test")
            return
        end if

        ! Check that base feature has been set
        if (.not. allocated(package%features(1)%base%name)) then
            call test_failed(error, "Base feature name not set in flexible test")
            return
        end if
        

    end subroutine test_feature_collection_flexible

    !> Test invalid feature collection configuration (should fail)
    subroutine test_feature_collection_invalid(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "invalid-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'badfeature.unknownos.badcompiler.flags = "-invalid"', &
            & 'badfeature.invalid-key-format = "should fail"'
        close(unit)
                
        call get_package_data(package, temp_file, error)
                        
    end subroutine test_feature_collection_invalid

    !> Test feature collection duplicate platform detection
    subroutine test_feature_collection_duplicates(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "duplicate-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'debug.gfortran.flags = "-g"', &
            & 'debug.gfortran.c-flags = "-DDEBUG"'  ! Duplicate gfortran platform
        close(unit)
                
        call get_package_data(package, temp_file, error)
        
        ! This should fail due to duplicate gfortran platform
        if (.not. allocated(error)) then
            call test_failed(error, "Expected error for duplicate platform configurations was not generated")
            return
        end if
        
        ! Clear the expected error
        deallocate(error)
                        
    end subroutine test_feature_collection_duplicates

    !> Test feature collection extract_for_target functionality  
    subroutine test_feature_collection_extract(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "extract-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'debug.flags = "-g"', &
            & 'debug.gfortran.flags = "-Wall"', &
            & 'debug.ifort.flags = "/debug"'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Should have debug feature collection
        if (.not. allocated(package%features) .or. size(package%features) < 1) then
            call test_failed(error, "No feature collections found for extract test")
            return
        end if
        
        ! Find debug feature collection
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "debug") then
                
                ! Test extraction for gfortran on linux
                target_platform%compiler = id_gcc
                target_platform%os_type = OS_LINUX
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                ! Should have both base and gfortran-specific flags
                if (.not. allocated(extracted_feature%flags)) then
                    call test_failed(error, "Extracted feature missing flags")
                    return
                end if
                
                ! The extracted flags should contain both base and gfortran flags
                ! (implementation details may vary but should have both -g and -Wall)
                if (index(extracted_feature%flags, "-g") == 0) then
                    call test_failed(error, "Extracted feature missing base flags (-g)")
                    return
                end if
                
                ! Test extraction for ifort
                target_platform%compiler = id_intel_classic_nix
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                if (.not. allocated(extracted_feature%flags)) then
                    call test_failed(error, "Extracted ifort feature missing flags")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "Debug feature collection not found for extract test")
                        
    end subroutine test_feature_collection_extract

    !> Test feature collection platform validation
    subroutine test_feature_collection_platform_validation(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "validation-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'test.invalidcompiler.flags = "-test"'  ! Invalid compiler name
        close(unit)
                
        ! Should return error
        call get_package_data(package, temp_file, error)
                        
    end subroutine test_feature_collection_platform_validation

    !> Test complex feature collection hierarchy
    subroutine test_feature_collection_complex(error)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        integer :: i, debug_variants, release_variants

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "complex-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'debug.flags = "-g"', &
            & 'debug.gfortran.flags = "-Wall"', &
            & 'debug.ifort.flags = "/debug"', &
            & 'debug.linux.flags = "-DLINUX"', &
            & 'debug.windows.ifort.flags = "/DEBUG:FULL"', &
            & 'release.flags = "-O3"', &
            & 'release.gfortran.flags = "-march=native"'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Should have 2 feature collections (debug and release)
        if (.not. allocated(package%features) .or. size(package%features) /= 2) then
            call test_failed(error, "Expected 2 feature collections for complex test")
            return
        end if
        
        debug_variants = 0
        release_variants = 0
        
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "debug") then
                if (allocated(package%features(i)%variants)) then
                    debug_variants = size(package%features(i)%variants)
                end if
            else if (package%features(i)%base%name == "release") then
                if (allocated(package%features(i)%variants)) then
                    release_variants = size(package%features(i)%variants)
                end if
            end if
        end do
        
        ! Debug should have multiple variants (gfortran, ifort, linux, windows+ifort)
        if (debug_variants < 3) then
            call test_failed(error, "Debug feature should have at least 3 variants in complex test")
            return
        end if
        
        ! Release should have 1 variant (gfortran)
        if (release_variants /= 1) then
            call test_failed(error, "Release feature should have exactly 1 variant in complex test")
            return
        end if
                        
    end subroutine test_feature_collection_complex

    !> Test that allocatable configurations cannot appear in multiple variants (should fail)
    subroutine test_feature_allocatable_conflict(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "conflict-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'myfeature.gfortran.fortran.source-dir = "src/gfortran"', &
            & 'myfeature.linux.fortran.source-dir = "src/linux"'  
        close(unit)
                
        ! Conflict: both would apply to gfortran+linux
        call get_package_data(package, temp_file, error)

    end subroutine test_feature_allocatable_conflict

    !> Test that flags are properly added together (additive)
    subroutine test_feature_flag_addition(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "addition-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'myfeature.flags = "-DBASE"', &
            & 'myfeature.gfortran.flags = "-DGFORTRAN"', &
            & 'myfeature.linux.flags = "-DLINUX"'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Should have feature collection
        if (.not. allocated(package%features) .or. size(package%features) < 1) then
            call test_failed(error, "No feature collections found for addition test")
            return
        end if
        
        ! Find myfeature collection
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "myfeature") then
                
                ! Test extraction for gfortran on linux (should get all three flags)
                target_platform%compiler = id_gcc
                target_platform%os_type = OS_LINUX
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                ! Should have flags from base, gfortran, and linux variants
                if (.not. allocated(extracted_feature%flags)) then
                    call test_failed(error, "Extracted feature missing flags")
                    return
                end if
                
                ! Should contain all three flags (additive behavior)
                if (index(extracted_feature%flags, "-DBASE") == 0) then
                    call test_failed(error, "Missing base flags (-DBASE)")
                    return
                end if
                
                if (index(extracted_feature%flags, "-DGFORTRAN") == 0) then
                    call test_failed(error, "Missing compiler-specific flags (-DGFORTRAN)")
                    return
                end if
                
                if (index(extracted_feature%flags, "-DLINUX") == 0) then
                    call test_failed(error, "Missing OS-specific flags (-DLINUX)")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "myfeature collection not found for addition test")
                        
    end subroutine test_feature_flag_addition

    !> Test that metapackages are properly combined with OR logic (additive)
    subroutine test_feature_metapackage_addition(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "meta-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'myfeature.dependencies.openmp = "*"', &
            & 'myfeature.gfortran.dependencies.stdlib = "*"', &
            & 'myfeature.linux.dependencies.mpi = "*"'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Should have feature collection
        if (.not. allocated(package%features) .or. size(package%features) < 1) then
            call test_failed(error, "No feature collections found for metapackage test")
            return
        end if
        
        ! Find myfeature collection  
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "myfeature") then
                
                ! Test extraction for gfortran on linux (should get all three metapackages)
                target_platform%compiler = id_gcc
                target_platform%os_type = OS_LINUX
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                if (.not. package%features(i)%base%meta%openmp%on) then
                    call test_failed(error, "Missing base openmp metapackage")
                    return
                end if
                                
                ! Should have all three metapackages enabled (OR logic)
                if (.not. extracted_feature%meta%openmp%on) then
                    call test_failed(error, "Missing openmp metapackage")
                    return
                end if
                
                if (.not. extracted_feature%meta%stdlib%on) then
                    call test_failed(error, "Missing stdlib metapackage") 
                    return
                end if
                
                if (.not. extracted_feature%meta%mpi%on) then
                    call test_failed(error, "Missing mpi metapackage")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "myfeature collection not found for metapackage test")
                        
    end subroutine test_feature_metapackage_addition

    !> Test comprehensive feature extraction for gfortran+Linux target (flags + executables + build)
    subroutine test_feature_extract_gfortran_linux(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "extract-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'debug.flags = "-g"', &
            & 'debug.gfortran.flags = "-Wall -fcheck=bounds"', &
            & 'debug.linux.flags = "-DLINUX"', &
            & 'debug.linux.gfortran.flags = "-fbacktrace"', &
            & '[[features.debug.executable]]', &
            & 'name = "base_prog"', &
            & 'source-dir = "app"', &
            & '[[features.debug.gfortran.executable]]', &
            & 'name = "gfortran_prog"', &
            & 'source-dir = "app/gfortran"', &
            & '[[features.debug.linux.executable]]', &
            & 'name = "linux_prog"', &
            & 'source-dir = "app/linux"', &
            & '[features.debug.gfortran.build]', &
            & 'auto-executables = false'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Find debug collection and extract for gfortran+Linux
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "debug") then
                target_platform = platform_config_t(id_gcc, OS_LINUX)
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                ! Check flags are combined correctly
                if (.not. allocated(extracted_feature%flags)) then
                    call test_failed(error, "No flags in extracted gfortran+Linux feature")
                    return
                end if
                
                if (index(extracted_feature%flags, "-g") == 0 .or. &
                    index(extracted_feature%flags, "-Wall") == 0 .or. &
                    index(extracted_feature%flags, "-DLINUX") == 0 .or. &
                    index(extracted_feature%flags, "-fbacktrace") == 0) then
                    call test_failed(error, "Missing expected flags in gfortran+Linux extraction")
                    return
                end if
                
                ! Check that all executables are combined (base + gfortran + linux)
                if (.not. allocated(extracted_feature%executable) .or. size(extracted_feature%executable) < 3) then
                    call test_failed(error, "Wrong number of executables in gfortran+Linux (expected 3)")
                    return
                end if
                
                ! Check that build config is set (only gfortran variant has it)
                if (.not. allocated(extracted_feature%build)) then
                    call test_failed(error, "Missing build config in gfortran+Linux extraction")
                    return
                end if
                
                if (extracted_feature%build%auto_executables) then
                    call test_failed(error, "Build config not applied correctly")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "debug collection not found")
                        
    end subroutine test_feature_extract_gfortran_linux

    !> Test comprehensive feature extraction for ifort+Windows target (flags + tests + library)  
    subroutine test_feature_extract_ifort_windows(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "extract-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'debug.flags = "/debug"', &
            & 'debug.ifort.flags = "/warn:all"', &
            & 'debug.windows.flags = "/DWINDOWS"', &
            & 'debug.linux.flags = "-DLINUX"', & ! Should not be included for Windows target
            & '[[features.debug.test]]', &
            & 'name = "base_test"', &
            & 'source-dir = "test"', &
            & '[[features.debug.ifort.test]]', &
            & 'name = "ifort_test"', &
            & 'source-dir = "test/ifort"', &
            & '[[features.debug.windows.test]]', &
            & 'name = "windows_test"', &
            & 'source-dir = "test/windows"', &
            & '[features.debug.windows.library]', &
            & 'source-dir = "src/windows"'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Find debug collection and extract for gfortran+Windows
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "debug") then
                target_platform = platform_config_t("ifort", OS_WINDOWS)
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                ! Check flags: should have base + ifort + windows (but NOT linux)
                if (.not. allocated(extracted_feature%flags)) then
                    call test_failed(error, "No flags in extracted ifort+Windows feature")
                    return
                end if
                
                if (index(extracted_feature%flags, "/debug") == 0 .or. &
                    index(extracted_feature%flags, "/warn:all") == 0 .or. &
                    index(extracted_feature%flags, "/DWINDOWS") == 0) then
                    call test_failed(error, "Missing expected flags in ifort+Windows extraction. Got: '"//extracted_feature%flags//"'")
                    return
                end if
                
                ! Should NOT have linux-specific flag
                if (index(extracted_feature%flags, "-DLINUX") > 0) then
                    call test_failed(error, "Incorrectly included Linux flag -DLINUX in ifort+Windows")
                    return
                end if
                
                ! Check that all tests are combined (base + ifort + windows)
                if (.not. allocated(extracted_feature%test) .or. size(extracted_feature%test) < 3) then
                    call test_failed(error, "Wrong number of tests in ifort+Windows (expected 3)")
                    return
                end if
                
                ! Check that library config is set (only windows variant has it)
                if (.not. allocated(extracted_feature%library)) then
                    call test_failed(error, "Missing library config in ifort+Windows extraction")
                    return
                end if
                
                if (extracted_feature%library%source_dir /= "src/windows") then
                    call test_failed(error, "Library config not applied correctly")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "debug collection not found")
                        
    end subroutine test_feature_extract_ifort_windows

    !> Test feature extraction with dependencies and examples for gfortran+macOS target
    subroutine test_feature_extract_dependencies_examples(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i, j
        logical :: has_gfortran_dep, has_macos_dep

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "deps-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & '[features.testing.dependencies]', &
            & 'base_dep.git = "https://github.com/example/base"', &
            & '[features.testing.gfortran.dependencies]', &
            & 'gfortran_dep.git = "https://github.com/example/gfortran"', &
            & '[features.testing.macos.dependencies]', &
            & 'macos_dep.git = "https://github.com/example/macos"', &
            & '[[features.testing.example]]', &
            & 'name = "base_example"', &
            & 'source-dir = "example"', &
            & '[[features.testing.gfortran.example]]', &
            & 'name = "gfortran_example"', &
            & 'source-dir = "example/gfortran"', &
            & '[[features.testing.macos.example]]', &
            & 'name = "macos_example"', &
            & 'source-dir = "example/macos"', &
            & '[features.testing.macos.fortran]', &
            & 'implicit-typing = false'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Find testing collection and extract for gfortran+macOS
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "testing") then
                target_platform = platform_config_t(id_gcc, OS_MACOS)
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                ! Check that all dependencies are combined (base + gfortran + macos)
                if (.not. allocated(extracted_feature%dependency)) then
                    call test_failed(error, "Missing dependencies in gfortran+macOS extraction")
                    return
                end if
                
                ! Verify that specific dependencies are present by checking names
                has_gfortran_dep = .false.
                has_macos_dep = .false.
                
                do j = 1, size(extracted_feature%dependency)
                    if (extracted_feature%dependency(j)%name == "gfortran_dep") then
                        has_gfortran_dep = .true.
                    end if
                    if (extracted_feature%dependency(j)%name == "macos_dep") then
                        has_macos_dep = .true.
                    end if
                end do
                
                if (.not. has_gfortran_dep) then
                    call test_failed(error, "Missing gfortran_dep dependency in gfortran+macOS extraction")
                    return
                end if
                if (.not. has_macos_dep) then
                    call test_failed(error, "Missing macos_dep dependency in gfortran+macOS extraction")
                    return
                end if
                
                ! Check that all examples are combined (base + gfortran + macos)
                if (.not. allocated(extracted_feature%example) .or. size(extracted_feature%example) < 3) then
                    call test_failed(error, "Wrong number of examples in gfortran+macOS (expected 3)")
                    return
                end if
                
                ! Check that fortran config is set (only macOS variant has it)
                if (.not. allocated(extracted_feature%fortran)) then
                    call test_failed(error, "Missing fortran config in gfortran+macOS extraction")
                    return
                end if
                
                if (extracted_feature%fortran%implicit_typing) then
                    call test_failed(error, "Fortran config not applied correctly - implicit typing should be false")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "testing collection not found")
                        
    end subroutine test_feature_extract_dependencies_examples

    !> Test feature extraction with build configurations for ifort+Linux target
    subroutine test_feature_extract_build_configs(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "build-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & '[features.optimization.ifort.linux.build]', &
            & 'auto-executables = false', &
            & 'auto-tests = false', &
            & 'link = ["mylib"]', &
            & 'external-modules = ["external_mod"]'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Find optimization collection and extract for ifort+Linux
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "optimization") then
                target_platform = platform_config_t(id_intel_classic_nix, OS_LINUX)
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                ! Check that build config is present
                if (.not. allocated(extracted_feature%build)) then
                    call test_failed(error, "Missing build config in ifort+Linux extraction")
                    return
                end if
                
                ! Check that auto-executables is set correctly
                if (extracted_feature%build%auto_executables) then
                    call test_failed(error, "Build config auto-executables should be false")
                    return
                end if
                
                ! Check that auto-tests is set correctly
                if (extracted_feature%build%auto_tests) then
                    call test_failed(error, "Build config auto-tests should be false")
                    return
                end if
                
                ! Check that link libraries are present
                if (.not. allocated(extracted_feature%build%link) .or. size(extracted_feature%build%link) < 1) then
                    call test_failed(error, "Missing link libraries in ifort+Linux build config")
                    return
                end if
                
                ! Check that external modules are present
                if (.not. allocated(extracted_feature%build%external_modules) .or. size(extracted_feature%build%external_modules) < 1) then
                    call test_failed(error, "Missing external modules in ifort+Linux build config")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "optimization collection not found")
                        
    end subroutine test_feature_extract_build_configs

    !> Test feature extraction with test configurations for gfortran+Windows target
    subroutine test_feature_extract_test_configs(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i, j
        logical :: has_base, has_gfortran, has_windows, has_specific

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "test-configs"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & '[[features.testing.test]]', &
            & 'name = "base_test"', &
            & 'source-dir = "test"', &
            & '[[features.testing.gfortran.test]]', &
            & 'name = "gfortran_test"', &
            & 'source-dir = "test/gfortran"', &
            & '[[features.testing.windows.test]]', &
            & 'name = "windows_test"', &
            & 'source-dir = "test/windows"', &
            & '[[features.testing.gfortran.windows.test]]', &
            & 'name = "gfortran_windows_test"', &
            & 'source-dir = "test/gfortran_windows"'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Find testing collection and extract for gfortran+Windows
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "testing") then
                target_platform = platform_config_t(id_gcc, OS_WINDOWS)
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                ! Check that all test configs are combined (base + gfortran + windows + gfortran.windows)
                if (.not. allocated(extracted_feature%test) .or. size(extracted_feature%test) < 4) then
                    call test_failed(error, "Wrong number of test configs in gfortran+Windows (expected 4)")
                    return
                end if
                
                ! Verify that specific test configs are present by checking names
                has_base = .false.
                has_gfortran = .false.
                has_windows = .false.
                has_specific = .false.
                
                do j = 1, size(extracted_feature%test)
                    select case (extracted_feature%test(j)%name)
                        case ("base_test")
                            has_base = .true.
                        case ("gfortran_test")
                            has_gfortran = .true.
                        case ("windows_test")
                            has_windows = .true.
                        case ("gfortran_windows_test")
                            has_specific = .true.
                    end select
                end do
                
                if (.not. has_base) then
                    call test_failed(error, "Missing base_test in gfortran+Windows extraction")
                    return
                end if
                if (.not. has_gfortran) then
                    call test_failed(error, "Missing gfortran_test in gfortran+Windows extraction")
                    return
                end if
                if (.not. has_windows) then
                    call test_failed(error, "Missing windows_test in gfortran+Windows extraction")
                    return
                end if
                if (.not. has_specific) then
                    call test_failed(error, "Missing gfortran_windows_test in gfortran+Windows extraction")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "testing collection not found")
                        
    end subroutine test_feature_extract_test_configs

    !> Test feature extraction with example configurations for ifx+macOS target
    subroutine test_feature_extract_example_configs(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        type(feature_config_t) :: extracted_feature
        type(platform_config_t) :: target_platform
        integer :: i, j
        logical :: has_base, has_ifx, has_macos, has_specific

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "example-configs"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & '[[features.showcase.example]]', &
            & 'name = "base_example"', &
            & 'source-dir = "examples"', &
            & '[[features.showcase.ifx.example]]', &
            & 'name = "ifx_example"', &
            & 'source-dir = "examples/ifx"', &
            & '[[features.showcase.macos.example]]', &
            & 'name = "macos_example"', &
            & 'source-dir = "examples/macos"', &
            & '[[features.showcase.ifx.macos.example]]', &
            & 'name = "ifx_macos_example"', &
            & 'source-dir = "examples/ifx_macos"'
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Find showcase collection and extract for ifx+macOS
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "showcase") then
                target_platform = platform_config_t(id_intel_llvm_nix, OS_MACOS)
                extracted_feature = package%features(i)%extract_for_target(target_platform)
                
                ! Check that all example configs are combined (base + ifx + macos + ifx.macos)
                if (.not. allocated(extracted_feature%example) .or. size(extracted_feature%example) < 4) then
                    call test_failed(error, "Wrong number of example configs in ifx+macOS (expected 4)")
                    return
                end if
                
                ! Verify that specific example configs are present by checking names
                has_base = .false.
                has_ifx = .false.
                has_macos = .false.
                has_specific = .false.
                
                do j = 1, size(extracted_feature%example)
                    select case (extracted_feature%example(j)%name)
                        case ("base_example")
                            has_base = .true.
                        case ("ifx_example")
                            has_ifx = .true.
                        case ("macos_example")
                            has_macos = .true.
                        case ("ifx_macos_example")
                            has_specific = .true.
                    end select
                end do
                
                if (.not. has_base) then
                    call test_failed(error, "Missing base_example in ifx+macOS extraction")
                    return
                end if
                if (.not. has_ifx) then
                    call test_failed(error, "Missing ifx_example in ifx+macOS extraction")
                    return
                end if
                if (.not. has_macos) then
                    call test_failed(error, "Missing macos_example in ifx+macOS extraction")
                    return
                end if
                if (.not. has_specific) then
                    call test_failed(error, "Missing ifx_macos_example in ifx+macOS extraction")
                    return
                end if
                
                return ! Test passed
            end if
        end do
        
        call test_failed(error, "showcase collection not found")
                        
    end subroutine test_feature_extract_example_configs

end module test_features
