!> Unit tests for FPM feature and feature collection functionality
module test_features
    use testsuite, only : new_unittest, unittest_t, error_t, test_failed
    use fpm_manifest, only: package_config_t, get_package_data
    use fpm_manifest_feature, only: feature_config_t
    use fpm_manifest_feature_collection, only: feature_collection_t
    use fpm_manifest_platform, only: platform_config_t
    use fpm_environment, only: OS_ALL, OS_LINUX, OS_MACOS, OS_WINDOWS
    use fpm_compiler, only: id_all, id_gcc, id_intel_classic_nix
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
            & new_unittest("feature-metapackage-addition", test_feature_metapackage_addition) &
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

end module test_features
