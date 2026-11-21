# Phase 2: fypp Source Discovery & Integration

**Status:** Ready to implement  
**Estimated Duration:** 2-3 hours  
**Previous Phase:** ✅ Configuration & Suffix Management (4 commits)

---

## Overview

Phase 2 integrates the fypp configuration system (Phase 1) with fpm's source discovery and build system. This phase transforms fypp from a "hypothetical preprocessor" into an integrated build step.

### What Phase 2 Does

1. **Discovers .fypp files** in source directories
2. **Creates preprocessing targets** for each discovered file
3. **Schedules preprocessing** before compilation
4. **Updates source lists** with preprocessed output files
5. **Integrates build graph** to ensure proper dependencies

### Key Deliverables

- ✅ .fypp file discovery in source scanning
- ✅ Automatic preprocessing target creation
- ✅ Build graph scheduling with dependencies
- ✅ Working integration with existing build system
- ✅ Support for both `skip_cpp_preprocessing` and `force_cpp_preprocessing`

---

## Architecture Overview

```
fpm.toml (Configuration)
   ↓
fpm_model.f90 (Load model)
   ↓
fpm_sources.f90 (Discover sources) ← PHASE 2 WORK: Add .fypp discovery
   ↓
fpm_targets.f90 (Create targets) ← PHASE 2 WORK: Create preprocessing targets
   ↓
fpm_backend.F90 (Execute build) ← PHASE 1 DONE: Ready to execute
   ↓
Compiled .f90/.F90 files (Output)
```

---

## Implementation Steps

### Step 1: Modify Source Discovery (`src/fpm_sources.f90`)

**Goal:** Identify .fypp files separately from other source files

**Current behavior:**
- `add_sources_from_dir()` reads all files in directories
- Separates by suffix: .f90, .F90, .f95, .F95, etc.
- Returns arrays of source_t objects

**Required changes:**

1. Create a new field in the model/state to track .fypp files
2. Extend `add_sources_from_dir()` to separate .fypp files
3. Store .fypp files separately for later preprocessing target creation

**Key locations:**
- `src/fpm_sources.f90` - Main source discovery module
- `src/fpm_model.f90` - Add field to store .fypp files and preprocessing config

**Pseudocode:**
```fortran
subroutine add_sources_from_dir(files, dir, recurse, preprocessor)
    ! Existing: read all files from directory
    
    ! NEW: Check if file has .fypp suffix
    if (ends_with(file, ".fypp")) then
        ! NEW: Separate fypp files from other sources
        allocate(fypp_files(n_fypp))
        fypp_files(i) = file
    else
        ! EXISTING: Handle other sources as before
    end if
end subroutine
```

### Step 2: Create Preprocessing Targets (`src/fpm_targets.f90`)

**Goal:** Generate preprocessing target for each .fypp file

**What needs to happen:**

1. For each .fypp file discovered in Step 1:
   - Determine output suffix using `get_output_suffix_for_file()`
   - Build output file path
   - Create `FPM_TARGET_PREPROCESS` target
   - Add as dependency for compilation targets

**Key locations:**
- `src/fpm_targets.f90` - Create new subroutine

**Pseudocode:**
```fortran
subroutine create_preprocess_targets(preproc, fypp_files, targets, model)
    ! For each .fypp file:
    do i = 1, size(fypp_files)
        ! Determine output file
        input_file = fypp_files(i)
        suffix = get_output_suffix_for_file(input_file, &
                                             preproc%output_suffix, &
                                             preproc%skip_cpp_preprocessing, &
                                             preproc%force_cpp_preprocessing)
        
        ! Create preprocessing target
        output_file = build_preprocess_dir // "/" // base_name(input_file) // "." // suffix
        
        ! Build target structure
        target%type = FPM_TARGET_PREPROCESS
        target%output_files = [output_file]
        target%compile_flags = build_fypp_command(input_file, output_file, macros)
        
        ! Add to targets list
        targets = [targets, target]
    end do
end subroutine
```

### Step 3: Integrate into Build Model (`src/fpm.f90`)

**Goal:** Coordinate source discovery, target creation, and build scheduling

**Changes needed:**

1. After source discovery completes:
   - Collect all .fypp files from each package
   - Call preprocessing target creation
   
2. Build graph integration:
   - Add preprocessing targets to build
   - Schedule before compilation targets
   - Set up dependencies: compilation depends on preprocessing

**Key locations:**
- `src/fpm.f90` - Main driver
- `src/fpm_model.f90` - Model structure for fypp files

**Pseudocode:**
```fortran
subroutine build_model(model)
    ! EXISTING: source discovery
    call add_sources_from_dir(model%sources, ...)
    
    ! NEW: Handle preprocessing
    if (allocated(model%packages(i)%preprocess%fypp)) then
        call create_preprocess_targets(preproc, fypp_files, &
                                       model%targets, model)
        
        ! Compiler targets depend on preprocessing targets
        do j = 1, size(model%targets)
            if (model%targets(j)%type == FPM_TARGET_COMPILE) then
                ! Add preprocessing targets as dependencies
                model%targets(j)%dependencies = &
                    [model%targets(j)%dependencies, preproc_targets]
            end if
        end do
    end if
end subroutine
```

### Step 4: Output Directory Management

**Goal:** Create preprocessed files in a separate directory

**Strategy:**
- Create `build/preprocess/` directory
- Place all preprocessed files there
- Compilation targets look for .f90/.F90 in preprocess directory

**Key implementation:**
```fortran
! In preprocessing target creation
preprocess_dir = join_path(model%build_dir, "preprocess")
call mkdir(preprocess_dir)

! Output file path
output_file = join_path(preprocess_dir, &
              basename(input_file) // "." // suffix)
```

---

## Code Changes Required

### File: `src/fpm_sources.f90`

**Changes:**
- Add helper function: `is_fypp_file(filename)` - Check if file is .fypp
- Modify: `add_sources_from_dir()` - Separate .fypp files
- Add: `.fypp` files to model for later processing

**Lines to modify:** ~20-30 lines

```fortran
! NEW: Helper function
logical function is_fypp_file(filename)
    character(*), intent(in) :: filename
    is_fypp_file = ends_with(filename, ".fypp")
end function

! MODIFY: In add_sources_from_dir()
if (is_fypp_file(file%name)) then
    ! Separate fypp files for preprocessing
    model%fypp_files = [model%fypp_files, file]
else
    ! EXISTING: Handle other sources
end if
```

### File: `src/fpm_targets.f90`

**Changes:**
- Add new subroutine: `create_preprocess_targets()`
- For each .fypp file: create `FPM_TARGET_PREPROCESS` target

**Lines to add:** ~40-50 lines

```fortran
subroutine create_preprocess_targets(package, model, error)
    ! Implementation from pseudocode above
    ! Create targets for all .fypp files in package
end subroutine
```

### File: `src/fpm.f90`

**Changes:**
- Add call to preprocessing target creation
- Integrate dependencies into build graph

**Lines to modify:** ~10-15 lines

```fortran
! After source discovery:
call create_preprocess_targets(package, model, error)

! Set up dependencies:
! Compilation targets depend on preprocessing targets
```

### File: `src/fpm_model.f90`

**Changes:**
- Add field to package/model: `fypp_files(:)` for discovered .fypp files

**Lines to add:** ~5-10 lines

---

## Testing Strategy

### Unit Tests (Recommended)

```fortran
! Test 1: .fypp file discovery
files = discover_sources("test_dir")
assert_true(any(files%name == "test.fypp"))

! Test 2: Suffix determination with skip_cpp
suffix = get_output_suffix_for_file("test_array.fypp", "F90", skip_patterns)
assert_equal(suffix, "f90")

! Test 3: Suffix determination with force_cpp
suffix = get_output_suffix_for_file("stdlib_const.fypp", "f90", "", force_patterns)
assert_equal(suffix, "F90")

! Test 4: Target creation
targets = create_preprocess_targets(preproc, fypp_files, model)
assert_equal(size(targets), size(fypp_files))
```

### Integration Tests (Simple Project)

Create minimal test project:
```
test_fypp_project/
├── fpm.toml
├── src/
│   ├── constants.fypp
│   └── utils.fypp
└── test/
    └── test_check.f90
```

Build and verify:
```bash
cd test_fypp_project
fpm build      # Should:
               # 1. Discover .fypp files
               # 2. Create preprocessing targets
               # 3. Run fypp on each file
               # 4. Compile preprocessed .f90/.F90 files
               # 5. Create executables
```

### Real-World Test: stdlib

Once Phase 2 is complete:
```bash
cd /path/to/stdlib
fpm build      # Should build stdlib using fypp preprocessing
```

---

## Key Functions to Use

From Phase 1 (Already Implemented):

```fortran
! Determine output suffix
suffix = get_output_suffix_for_file(fypp_file, default_suffix, &
                                   skip_patterns, force_patterns)

! Check if file matches pattern
match = matches_glob_pattern(filename, pattern)

! Build fypp command
cmd = build_fypp_command(fypp_exe, input_file, output_file, macros)

! Run fypp
call run_fypp(input_file, output_file, macros, error)
```

---

## Potential Issues & Solutions

### Issue 1: Output Directory Path

**Problem:** Where to put preprocessed files?

**Solution:** 
- Create `build/preprocess/` directory
- All `.fypp` → `build/preprocess/file.f90` or `.F90`
- Compilation looks in preprocess directory

### Issue 2: Dependencies Between Preprocessing Targets

**Problem:** Some .fypp files might depend on others

**Solution (Future phase):**
- For now, assume independence
- Phase 3 can add dependency tracking
- Run all preprocessing in parallel (safe if independent)

### Issue 3: Incremental Builds

**Problem:** Recompile fypp files unnecessarily?

**Solution (Deferred to Phase 3):**
- Track .fypp file modification time
- Only reprocess if source changed
- For now: Always reprocess (simpler)

### Issue 4: Error Handling

**Problem:** fypp fails during preprocessing?

**Solution:**
- Call `run_fypp()` which returns error_t
- Propagate error to build system
- fpm build fails with clear error message

---

## Debugging Tips

### View discovered .fypp files:
```bash
cd /home/jose/Dev/fpm
grep -n "fypp" src/fpm_model.f90      # Check model structure
grep -n "fypp" src/fpm_sources.f90    # Check discovery logic
```

### View preprocessing targets:
```bash
# In fpm_targets.f90, add debug output:
write(*,*) "Created preprocessing target:", target%name
```

### Test individual functions:
```fortran
! Test suffix logic
suffix = get_output_suffix_for_file("test_foo.fypp", "F90", skip_pats, force_pats)
print *, "Suffix:", suffix   ! Should be "f90"
```

---

## Checklist

- [ ] Review this document thoroughly
- [ ] Update `src/fpm_sources.f90` for .fypp discovery
- [ ] Create preprocessing target creation logic
- [ ] Integrate into build model
- [ ] Verify compilation (should still build fpm)
- [ ] Create simple test project
- [ ] Build simple test project
- [ ] Test suffix management (both skip and force)
- [ ] Clean up and commit changes
- [ ] Test with stdlib project

---

## Expected Outcome

After Phase 2 completion:

✅ Users can list .fypp files in `fpm.toml`:
```toml
[preprocess.fypp]
suffixes = [".fypp"]
directories = ["src"]
```

✅ fpm automatically:
1. Discovers .fypp files
2. Creates preprocessing targets
3. Runs fypp on each file
4. Compiles preprocessed .f90/.F90 files
5. Links everything together

✅ Both configurations work:
```toml
# Option A: Default to F90
output_suffix = "F90"
skip_cpp_preprocessing = ["test_*.fypp"]

# Option B: Default to f90
output_suffix = "f90"
force_cpp_preprocessing = ["stdlib_*.fypp"]
```

✅ Build succeeds for projects using fypp

---

## Next Phase (Phase 3): Validation & Testing

- Incremental build support
- Comprehensive test suite
- stdlib integration testing
- Performance optimization

---

**Ready to start Phase 2 implementation!**
