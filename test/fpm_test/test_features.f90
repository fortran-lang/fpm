!> Unit tests for FPM feature and feature collection functionality
module test_features
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_manifest_feature, only: feature_config_t
    use fpm_manifest_feature_collection, only: feature_collection_t
    use fpm_manifest_platform, only: platform_config_t
    use fpm_environment, only: OS_ALL, OS_LINUX, OS_MACOS, OS_WINDOWS
    use fpm_compiler, only: id_all, id_gcc, id_intel_classic_nix, id_intel_classic_windows, id_intel_llvm_nix, &
        match_compiler_type
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
            & new_unittest("feature-collection-platform-validation", test_feature_collection_platform_validation, &
            &                                                        should_fail=.true.), &
            & new_unittest("feature-collection-complex", test_feature_collection_complex), &
            & new_unittest("feature-allocatable-conflict", test_feature_allocatable_conflict, should_fail=.true.), &
            & new_unittest("feature-flag-addition", test_feature_flag_addition), &
            & new_unittest("feature-metapackage-addition", test_feature_metapackage_addition), &
            & new_unittest("feature-extract-gfortran-linux", test_feature_extract_gfortran_linux), &
            & new_unittest("feature-extract-ifort-windows", test_feature_extract_ifort_windows), &
            & new_unittest("feature-extract-dependencies-examples", test_feature_extract_dependencies_examples), &
            & new_unittest("feature-extract-build-configs", test_feature_extract_build_configs), &
            & new_unittest("feature-extract-test-configs", test_feature_extract_test_configs), &
            & new_unittest("feature-extract-example-configs", test_feature_extract_example_configs), &
            & new_unittest("dependency-feature-propagation", test_dependency_feature_propagation), &
            & new_unittest("dependency-features-specification", test_dependency_features_specification), &
            & new_unittest("feature-chained-os-commands", test_feature_chained_os_commands, should_fail=.true.), &
            & new_unittest("feature-chained-compiler-commands", &
            &              test_feature_chained_compiler_commands, should_fail=.true.), &
            & new_unittest("feature-complex-chain-compiler-os-compiler", &
            &              test_feature_complex_chain_compiler_os_compiler, should_fail=.true.), &
            & new_unittest("feature-complex-chain-os-compiler-os", &
            &              test_feature_complex_chain_os_compiler_os, should_fail=.true.), &
            & new_unittest("feature-mixed-valid-chains", test_feature_mixed_valid_chains), &
            & new_unittest("feature-compiler-flags-integration", test_feature_compiler_flags_integration), &
            & new_unittest("default-profile-with-debug", test_default_profile_with_debug), &
            & new_unittest("default-profile-with-release", test_default_profile_with_release), &
            & new_unittest("default-profile-implicit-debug", test_default_profile_implicit_debug), &
            & new_unittest("default-profile-skipped-custom", test_default_profile_skipped_custom) &
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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
            & 'release.gfortran.flags = "-mtune=generic -funroll-loops"'
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
                ! Check flags: should have base + ifort + windows (but NOT linux)
                if (.not. allocated(extracted_feature%flags)) then
                    call test_failed(error, "No flags in extracted ifort+Windows feature")
                    return
                end if
                
                if (index(extracted_feature%flags, "/debug") == 0 .or. &
                    index(extracted_feature%flags, "/warn:all") == 0 .or. &
                    index(extracted_feature%flags, "/DWINDOWS") == 0) then
                    call test_failed(error, "Missing expected flags in ifort+Windows extraction. Got: '" &
                                            //extracted_feature%flags//"'")
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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
                    call test_failed(error, "Fortran config not applied correctly - "// &
                                            " implicit typing should be false")
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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
                if (.not. allocated(extracted_feature%build%external_modules) &
                    .or. size(extracted_feature%build%external_modules) < 1) then
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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
                extracted_feature = package%features(i)%extract_for_target(target_platform, error=error)
                if (allocated(error)) return
                
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

    !> Test that dependency features are correctly propagated and applied
    subroutine test_dependency_feature_propagation(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: dependency_config, exported_config
        character(:), allocatable :: temp_file
        integer :: unit
        type(platform_config_t) :: target_platform
        type(string_t), allocatable :: test_features(:)

        allocate(temp_file, source=get_temp_filename())

        ! Create a dependency manifest with features
        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "test-dependency"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'debug.flags = "-g -DDEBUG"', &
            & 'debug.gfortran.flags = "-fcheck=bounds"', &
            & 'mpi.flags = "-DUSE_MPI"', &
            & 'mpi.dependencies.mpi = "*"', &
            & '[[features.debug.executable]]', &
            & 'name = "debug_tool"', &
            & 'source-dir = "debug_tools"'
        close(unit)

        ! Load the dependency configuration
        call get_package_data(dependency_config, temp_file, error)
        if (allocated(error)) return

        ! Simulate dependency requesting specific features (like dep%features from build_model)
        allocate(test_features(2))
        test_features(1)%s = "debug" 
        test_features(2)%s = "mpi"

        ! Test export_config with these features (mimics line 132-133 in fpm.f90)
        target_platform = platform_config_t(id_gcc, OS_LINUX)
        exported_config = dependency_config%export_config(target_platform, test_features, verbose=.false., error=error)
        if (allocated(error)) return

        ! Verify that debug feature flags were applied
        if (.not. allocated(exported_config%flags)) then
            call test_failed(error, "Dependency export_config missing flags from debug feature")
            return
        end if

        if (index(exported_config%flags, "-g") == 0 .or. &
            index(exported_config%flags, "-DDEBUG") == 0) then
            call test_failed(error, "Dependency missing debug flags: got '" // exported_config%flags // "'")
            return
        end if

        if (index(exported_config%flags, "-fcheck=bounds") == 0) then
            call test_failed(error, "Dependency missing gfortran-specific debug flags")
            return
        end if

        ! Verify that mpi feature flags were applied  
        if (index(exported_config%flags, "-DUSE_MPI") == 0) then
            call test_failed(error, "Dependency missing mpi flags")
            return
        end if

        ! Verify that mpi metapackage was enabled
        if (.not. exported_config%meta%mpi%on) then
            call test_failed(error, "Dependency mpi metapackage not enabled")
            return
        end if

        ! Verify that debug executable was included
        if (.not. allocated(exported_config%executable)) then
            call test_failed(error, "Dependency debug executable not included")
            return  
        end if

        if (size(exported_config%executable) < 1) then
            call test_failed(error, "Dependency should have debug executable")
            return
        end if

        if (exported_config%executable(1)%name /= "debug_tool") then
            call test_failed(error, "Dependency debug executable has wrong name")
            return
        end if

    end subroutine test_dependency_feature_propagation

    !> Test that main package can specify features for its dependencies
    subroutine test_dependency_features_specification(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: main_package
        character(:), allocatable :: temp_file
        integer :: unit, i
        logical :: found_tomlf_dep

        allocate(temp_file, source=get_temp_filename())

        ! Create a main package manifest that specifies features for dependencies
        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "main-package"', &
            & 'version = "0.1.0"', &
            & '[dependencies]', &
            & '"dep-a" = { path = "../dep-a", features = ["openmp", "json"] }', &
            & '"dep-b" = { path = "../dep-b", features = ["debug"] }'
        close(unit)

        ! Load the main package configuration
        call get_package_data(main_package, temp_file, error)
        if (allocated(error)) return

        ! Verify dependencies were parsed correctly
        if (.not. allocated(main_package%dependency)) then
            call test_failed(error, "Main package dependencies not allocated")
            return
        end if

        if (size(main_package%dependency) /= 2) then
            call test_failed(error, "Expected 2 dependencies, got " // & 
                           char(size(main_package%dependency) + ichar('0')))
            return
        end if

        ! Find and verify dep-a dependency with features
        found_tomlf_dep = .false.
        do i = 1, size(main_package%dependency)
            if (main_package%dependency(i)%name == "dep-a") then
                found_tomlf_dep = .true.

                ! Verify path configuration exists
                if (.not. allocated(main_package%dependency(i)%path)) then
                    call test_failed(error, "dep-a dependency missing path configuration")
                    return
                end if

                ! Path gets canonicalized, so just check it ends with the relative path
                if (index(main_package%dependency(i)%path, "dep-a") == 0) then
                    call test_failed(error, "dep-a dependency path should contain 'dep-a', got: '" // &
                                          main_package%dependency(i)%path // "'")
                    return
                end if

                ! Verify features array - this is the key test
                if (.not. allocated(main_package%dependency(i)%features)) then
                    call test_failed(error, "dep-a dependency features not allocated")
                    return
                end if

                if (size(main_package%dependency(i)%features) /= 2) then
                    call test_failed(error, "dep-a dependency should have 2 features")
                    return
                end if

                if (main_package%dependency(i)%features(1)%s /= "openmp" .or. &
                    main_package%dependency(i)%features(2)%s /= "json") then
                    call test_failed(error, "dep-a dependency has wrong feature names")
                    return
                end if
                exit
            end if
        end do

        if (.not. found_tomlf_dep) then
            call test_failed(error, "dep-a dependency not found")
            return
        end if

        ! Verify dep-b dependency has features
        do i = 1, size(main_package%dependency)
            if (main_package%dependency(i)%name == "dep-b") then
                if (.not. allocated(main_package%dependency(i)%features)) then
                    call test_failed(error, "dep-b dependency features not allocated")
                    return
                end if

                if (size(main_package%dependency(i)%features) /= 1) then
                    call test_failed(error, "dep-b dependency should have 1 feature")
                    return
                end if

                if (main_package%dependency(i)%features(1)%s /= "debug") then
                    call test_failed(error, "dep-b dependency has wrong feature name")
                    return
                end if
                exit
            end if
        end do

    end subroutine test_dependency_features_specification

    !> Test that chained OS commands are rejected (should fail)
    subroutine test_feature_chained_os_commands(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "chained-os-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'myfeature.windows.linux.flags = "-invalid"'  ! Chained OS: windows.linux
        close(unit)
                
        call get_package_data(package, temp_file, error)
        
        ! This should fail due to chained OS commands
                        
    end subroutine test_feature_chained_os_commands

    !> Test that chained compiler commands are rejected (should fail)  
    subroutine test_feature_chained_compiler_commands(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "chained-compiler-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'myfeature.gfortran.ifort.flags = "-invalid"'  ! Chained compiler: gfortran.ifort
        close(unit)
                
        call get_package_data(package, temp_file, error)
        
        ! This should fail due to chained compiler commands
                        
    end subroutine test_feature_chained_compiler_commands

    !> Test complex chaining: compiler.os.compiler (should fail)
    subroutine test_feature_complex_chain_compiler_os_compiler(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "complex-chain-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'myfeature.gfortran.windows.ifort.flags = "-invalid"'  ! gfortran.windows.ifort chain
        close(unit)
                
        call get_package_data(package, temp_file, error)
        
        ! This should fail due to chained compiler constraints: gfortran -> windows (OK) -> ifort (ERROR)
                        
    end subroutine test_feature_complex_chain_compiler_os_compiler

    !> Test complex chaining: os.compiler.os (should fail)
    subroutine test_feature_complex_chain_os_compiler_os(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "complex-chain-test2"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'myfeature.windows.ifx.macos.flags = "-invalid"'  ! windows.ifx.macos chain
        close(unit)
                
        call get_package_data(package, temp_file, error)
        
        ! This should fail due to chained OS constraints: windows -> ifx (OK) -> macos (ERROR)
                        
    end subroutine test_feature_complex_chain_os_compiler_os

    !> Test mixed valid chains (should pass)
    subroutine test_feature_mixed_valid_chains(error)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package
        character(:), allocatable :: temp_file
        integer :: unit
        integer :: i

        allocate(temp_file, source=get_temp_filename())

        open(file=temp_file, newunit=unit)
        write(unit, '(a)') &
            & 'name = "valid-chains-test"', &
            & 'version = "0.1.0"', &
            & '[features]', &
            & 'debug.flags = "-g"', &  ! Base feature (all OS, all compilers)
            & 'debug.gfortran.flags = "-Wall"', &  ! Compiler-specific (gfortran, all OS)
            & 'debug.windows.flags = "-DWINDOWS"', &  ! OS-specific (all compilers, Windows)
            & 'debug.gfortran.windows.flags = "-fbacktrace"', &  ! Target-specific (gfortran + Windows)
            & 'debug.linux.ifort.flags = "-check all"', &  ! Target-specific (Linux + ifort)
            & 'release.ifx.macos.flags = "-O3"'  ! Another valid target-specific (ifx + macOS)
        close(unit)
                
        call get_package_data(package, temp_file, error)
        if (allocated(error)) return
        
        ! Verify that valid chains are accepted and collections created
        if (.not. allocated(package%features)) then
            call test_failed(error, "No feature collections found for valid chains test")
            return
        end if
        
        ! Should have debug and release features
        if (size(package%features) < 2) then
            call test_failed(error, "Expected at least 2 feature collections for valid chains")
            return
        end if
        
        ! Check that debug feature has multiple variants
        do i = 1, size(package%features)
            if (package%features(i)%base%name == "debug") then
                if (.not. allocated(package%features(i)%variants)) then
                    call test_failed(error, "Debug collection should have variants for valid chains")
                    return
                end if
                
                ! Should have multiple variants: gfortran, windows, gfortran.windows, linux.ifort
                if (size(package%features(i)%variants) < 4) then
                    call test_failed(error, "Debug collection should have at least 4 variants for valid chains")
                    return
                end if
                exit
            end if
        end do
                        
    end subroutine test_feature_mixed_valid_chains

    !> Test integration of feature compiler flags with new_compiler_flags
    subroutine test_feature_compiler_flags_integration(error)
        use fpm, only: new_compiler_flags
        use fpm_model, only: fpm_model_t
        use fpm_command_line, only: fpm_build_settings
        use fpm_compiler, only: new_compiler, id_gcc

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package_config,package
        type(fpm_model_t) :: model
        type(fpm_build_settings) :: settings
        type(platform_config_t) :: target_platform
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        ! Create a test package with feature-based compiler flags
        open(newunit=unit, file=temp_file, status='unknown')
        write(unit, '(a)') 'name = "test_flags"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[library]'
        write(unit, '(a)') 'source-dir = "src"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[features]'
        write(unit, '(a)') 'debug.gfortran.flags = "-g -Wall -fcheck=bounds"'
        write(unit, '(a)') 'debug.flags = "-g"'
        write(unit, '(a)') 'release.gfortran.flags = "-O3 -mtune=generic -funroll-loops"'
        write(unit, '(a)') 'release.flags = "-O2"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[profiles]'
        write(unit, '(a)') 'development = ["debug"]'
        write(unit, '(a)') 'production = ["release"]'
        close(unit)

        ! Set up build settings without CLI flags
        settings%flag = ""
        settings%cflag = ""
        settings%cxxflag = ""
        settings%ldflag = ""

        ! Load the package configuration
        call get_package_data(package_config, temp_file, error, apply_defaults=.true.)
        if (allocated(error)) return

        ! 1) Choose first desired target platform: gfortran on Linux with development profile
        target_platform = platform_config_t(id_gcc, OS_LINUX)
        settings%profile = "development"  ! This should activate debug features

        ! Extract the current package configuration request
        package = package_config%export_config(target_platform, profile=settings%profile, verbose=.false., error=error)
        if (allocated(error)) return

        ! Set up model with mock compiler
        call new_compiler(model%compiler, "gfortran", "gcc", "g++", echo=.false., verbose=.false.)

        ! Test that package flags are used when no CLI flags provided
        call new_compiler_flags(model, settings, package)

        ! 2) Ensure flags are picked from gfortran platform (should include both base debug and gfortran-specific)
        if (.not. allocated(model%fortran_compile_flags)) then
            call test_failed(error, "Expected fortran_compile_flags to be allocated for gfortran")
            return
        end if

        if (index(model%fortran_compile_flags, "-g") == 0) then
            call test_failed(error, "Expected debug flags to contain '-g' for gfortran platform")
            return
        end if

        if (index(model%fortran_compile_flags, "-Wall") == 0) then
            call test_failed(error, "Expected gfortran-specific flags to contain '-Wall'")
            return
        end if

        if (index(model%fortran_compile_flags, "-fcheck=bounds") == 0) then
            call test_failed(error, "Expected gfortran-specific flags to contain '-fcheck=bounds'")
            return
        end if

        ! 3) Choose another target platform: gfortran on Linux with production profile
        settings%profile = "production"  ! This should activate release features

        ! Extract the new package configuration request
        package = package_config%export_config(target_platform, profile=settings%profile, verbose=.false., error=error)
        if (allocated(error)) return

        ! Reset flags and test production profile
        call new_compiler_flags(model, settings, package)

        ! 4) Ensure flags are picked from the release platform (should include release flags)
        if (.not. allocated(model%fortran_compile_flags)) then
            call test_failed(error, "Expected fortran_compile_flags to be allocated for release")
            return
        end if

        if (index(model%fortran_compile_flags, "-O3") == 0) then
            call test_failed(error, "Expected release gfortran flags to contain '-O3'")
            return
        end if

        if (index(model%fortran_compile_flags, "-mtune") == 0) then
            call test_failed(error, "Expected release gfortran flags to contain '-mtune'")
            return
        end if

        if (index(model%fortran_compile_flags, "-funroll-loops") == 0) then
            call test_failed(error, "Expected release gfortran flags to contain '-funroll-loops'")
            return
        end if

        if (index(model%fortran_compile_flags, "-O2") == 0) then
            call test_failed(error, "Expected base release flags to contain '-O2'")
            return
        end if

        ! Test CLI flags still override package flags
        settings%flag = "-O1 -DCUSTOM"
        call new_compiler_flags(model, settings, package)

        if (index(model%fortran_compile_flags, "-O1") == 0) then
            call test_failed(error, "Expected CLI flags to be used when provided")
            return
        end if

        if (index(model%fortran_compile_flags, "-DCUSTOM") == 0) then
            call test_failed(error, "Expected CLI flags to contain custom flags")
            return
        end if

        ! Clean up - file was already closed after writing

    end subroutine test_feature_compiler_flags_integration

    !> Test that "default" profile features are applied alongside --profile debug
    subroutine test_default_profile_with_debug(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package_config, package
        type(platform_config_t) :: target_platform
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(newunit=unit, file=temp_file, status='unknown')
        write(unit, '(a)') 'name = "test_default"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') '[library]'
        write(unit, '(a)') 'source-dir = "src"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[features]'
        write(unit, '(a)') 'baseline.flags = "-fPIC"'
        write(unit, '(a)') 'debug.flags = "-g"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[profiles]'
        write(unit, '(a)') 'default = ["baseline"]'
        write(unit, '(a)') 'debug = ["debug"]'
        close(unit)

        call get_package_data(package_config, temp_file, error, apply_defaults=.true.)
        if (allocated(error)) return

        target_platform = platform_config_t(id_gcc, OS_LINUX)

        ! Export with --profile debug: should get both default (baseline) and debug features
        package = package_config%export_config(target_platform, profile="debug", verbose=.false., error=error)
        if (allocated(error)) return

        ! Should have baseline flags from default profile
        if (index(package%flags, "-fPIC") == 0) then
            call test_failed(error, "Expected default profile flags '-fPIC' to be present with --profile debug")
            return
        end if

        ! Should also have debug flags
        if (index(package%flags, "-g") == 0) then
            call test_failed(error, "Expected debug flags '-g' to be present with --profile debug")
            return
        end if

    end subroutine test_default_profile_with_debug

    !> Test that "default" profile features are applied alongside --profile release
    subroutine test_default_profile_with_release(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package_config, package
        type(platform_config_t) :: target_platform
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(newunit=unit, file=temp_file, status='unknown')
        write(unit, '(a)') 'name = "test_default"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') '[library]'
        write(unit, '(a)') 'source-dir = "src"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[features]'
        write(unit, '(a)') 'baseline.flags = "-fPIC"'
        write(unit, '(a)') 'release.flags = "-O3"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[profiles]'
        write(unit, '(a)') 'default = ["baseline"]'
        write(unit, '(a)') 'release = ["release"]'
        close(unit)

        call get_package_data(package_config, temp_file, error, apply_defaults=.true.)
        if (allocated(error)) return

        target_platform = platform_config_t(id_gcc, OS_LINUX)

        ! Export with --profile release: should get both default (baseline) and release features
        package = package_config%export_config(target_platform, profile="release", verbose=.false., error=error)
        if (allocated(error)) return

        ! Should have baseline flags from default profile
        if (index(package%flags, "-fPIC") == 0) then
            call test_failed(error, "Expected default profile flags '-fPIC' to be present with --profile release")
            return
        end if

        ! Should also have release flags
        if (index(package%flags, "-O3") == 0) then
            call test_failed(error, "Expected release flags '-O3' to be present with --profile release")
            return
        end if

    end subroutine test_default_profile_with_release

    !> Test that "default" profile features are applied when no profile is specified (implicit debug)
    subroutine test_default_profile_implicit_debug(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package_config, package
        type(platform_config_t) :: target_platform
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(newunit=unit, file=temp_file, status='unknown')
        write(unit, '(a)') 'name = "test_default"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') '[library]'
        write(unit, '(a)') 'source-dir = "src"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[features]'
        write(unit, '(a)') 'baseline.flags = "-fPIC"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[profiles]'
        write(unit, '(a)') 'default = ["baseline"]'
        close(unit)

        call get_package_data(package_config, temp_file, error, apply_defaults=.true.)
        if (allocated(error)) return

        target_platform = platform_config_t(id_gcc, OS_LINUX)

        ! Export with no profile/features: should still get default profile features
        package = package_config%export_config(target_platform, verbose=.false., error=error)
        if (allocated(error)) return

        ! Should have baseline flags from default profile
        if (index(package%flags, "-fPIC") == 0) then
            call test_failed(error, "Expected default profile flags '-fPIC' to be present with no explicit profile")
            return
        end if

    end subroutine test_default_profile_implicit_debug

    !> Test that "default" profile features are NOT applied when a custom profile is requested
    subroutine test_default_profile_skipped_custom(error)
        type(error_t), allocatable, intent(out) :: error

        type(package_config_t) :: package_config, package
        type(platform_config_t) :: target_platform
        character(:), allocatable :: temp_file
        integer :: unit

        allocate(temp_file, source=get_temp_filename())

        open(newunit=unit, file=temp_file, status='unknown')
        write(unit, '(a)') 'name = "test_default"'
        write(unit, '(a)') 'version = "0.1.0"'
        write(unit, '(a)') '[library]'
        write(unit, '(a)') 'source-dir = "src"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[features]'
        write(unit, '(a)') 'baseline.flags = "-fPIC"'
        write(unit, '(a)') 'custom_opt.flags = "-O1"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[profiles]'
        write(unit, '(a)') 'default = ["baseline"]'
        write(unit, '(a)') 'myprofile = ["custom_opt"]'
        close(unit)

        call get_package_data(package_config, temp_file, error, apply_defaults=.true.)
        if (allocated(error)) return

        target_platform = platform_config_t(id_gcc, OS_LINUX)

        ! Export with --profile myprofile: should NOT get default profile features
        package = package_config%export_config(target_platform, profile="myprofile", verbose=.false., error=error)
        if (allocated(error)) return

        ! Should have custom profile flags
        if (index(package%flags, "-O1") == 0) then
            call test_failed(error, "Expected custom profile flags '-O1' to be present")
            return
        end if

        ! Should NOT have baseline flags from default profile
        if (index(package%flags, "-fPIC") /= 0) then
            call test_failed(error, "Default profile flags '-fPIC' should NOT be present with custom profile")
            return
        end if

    end subroutine test_default_profile_skipped_custom

end module test_features
