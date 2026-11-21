# Phase 2 Implementation - Work Log

**Start Time:** 2026-11-21 15:18 UTC  
**Branch:** fypp  
**Goal:** Source discovery and preprocessing target integration

---

## Task 1: .fypp File Discovery

### Objective
Modify `src/fpm_sources.f90` to:
1. Identify `.fypp` files separately from other source files
2. Store .fypp file information for preprocessing
3. Keep existing source discovery logic intact

### Files to Modify
- `src/fpm_sources.f90` - Main source discovery
- `src/fpm_model.f90` - Add fypp_files field to package_t

### Implementation Strategy

Currently, `add_sources_from_dir()`:
1. Lists all files in directory
2. Filters by known suffixes (.f90, .F90, .c, .h, etc.)
3. Parses and adds to sources array

NEW behavior:
1. Add .fypp to recognized suffixes
2. During filtering, separate .fypp files from other sources
3. Store .fypp files separately for preprocessing
4. DON'T parse .fypp files as regular Fortran (they're not yet)

### Key Changes Needed

1. **Helper function:** `is_fypp_file(filename)`
2. **Modify filter:** Separate .fypp from regular sources
3. **Store separately:** Add fypp_files to package model
4. **Don't parse:** .fypp files aren't parsed yet (preprocessing happens later)

---

## Task 2: Preprocessing Target Creation

### Objective
Create `FPM_TARGET_PREPROCESS` targets for each discovered .fypp file

### Files to Modify
- `src/fpm_targets.f90` - Create new subroutine

### Implementation Strategy

For each .fypp file:
1. Use `get_output_suffix_for_file()` to determine output suffix
2. Build preprocessing target structure
3. Set up dependencies: compilation targets depend on preprocessing

---

## Task 3: Build System Integration

### Objective
Integrate preprocessing into build system

### Files to Modify
- `src/fpm.f90` - Call preprocessing target creation

---

## Implementation Progress

- [ ] Task 1: .fypp file discovery
- [ ] Task 2: Preprocessing target creation
- [ ] Task 3: Build integration
- [ ] Testing: Simple project
- [ ] Testing: stdlib

