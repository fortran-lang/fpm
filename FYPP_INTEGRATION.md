# FPM fypp Integration Guide

## Overview

This document describes the fypp preprocessor integration for fpm. This integration allows Fortran projects to use the fypp meta-programming preprocessor directly through fpm configuration, without requiring external Python preprocessing scripts.

## Status

**Current Phase:** Configuration & Infrastructure Support

### Completed Features
- ✓ Extended preprocess configuration to support fypp
- ✓ Added output_suffix configuration for fypp output naming
- ✓ Created fypp_config_t type for executing preprocessor
- ✓ Command building utilities for fypp invocation
- ✓ TOML serialization/deserialization for fypp config

### In Development
- [ ] Automatic .fypp file discovery and tracking
- [ ] Integration with fpm build targets
- [ ] Preprocessing execution in build backend
- [ ] Incremental compilation support

## Configuration

### TOML Manifest Format

Add a `[preprocess.fypp]` section to your `fpm.toml`:

```toml
[package]
name = "example"
version = "0.1.0"

[preprocess]
# CPP preprocessing (existing)
[preprocess.cpp]
suffixes = [".F90"]
directories = ["src"]
macros = ["MAXRANK=7"]

# fypp preprocessing (new)
[preprocess.fypp]
suffixes = [".fypp"]
directories = ["src", "test"]
output_suffix = "F90"  # or "f90"
macros = [
  "MAXRANK=7",
  "PROJECT_VERSION_MAJOR=0",
  "PROJECT_VERSION_MINOR=1",
  "PROJECT_VERSION_PATCH=0",
  "WITH_QP=True"
]
```

### Configuration Options

- **suffixes**: Array of file extensions to preprocess (e.g., `[".fypp"]`)
- **directories**: Directories to search for fypp files (e.g., `["src", "test"]`)
- **output_suffix**: Output file suffix after preprocessing (default: "F90")
  - Use "F90" for files that need C preprocessing after fypp
  - Use "f90" for files that don't need further preprocessing
- **macros**: Array of macro definitions passed to fypp
  - Format: `"MACRO=value"` or just `"MACRO"` for boolean flags
  - Converted to `-DMACRO=value` command-line arguments

## Module Structure

### fpm_manifest_preprocess
Enhanced to support fypp:
- `preprocess_config_t%output_suffix` - New field
- `preprocess_config_t%is_fypp()` - Check if preprocessor is fypp
- Methods: `load_from_toml()`, `dump_to_toml()`, `add_config()`

### fpm_manifest_fypp (New Module)
Utilities for executing fypp:
- `fypp_config_t` - Configuration and state management
- `run_fypp()` - Execute fypp on a single file
- `build_fypp_command()` - Construct fypp command string
- `fypp_config_t%process_file()` - Process file with error handling

## Implementation Notes

### Design Decisions

1. **System Call Approach**: fypp is invoked via system calls, not embedded
2. **Coexistence with CPP**: CPP and fypp can be used in the same project
3. **Macro Format**: Uses `-DMACRO=value` for fypp compatibility
4. **Output Naming**: Configurable suffixes allow both .f90 and .F90 output
5. **Build Integration**: Preprocessing happens as a build target type

### Preprocessing Flow (Planned)

```
Source Discovery
    ↓
Parse Configuration
    ↓
Create Preprocessing Targets
    ↓
Schedule Build
    ↓
Execute Preprocessing (fypp)
    ↓
Execute Compilation (gfortran, ifort, etc.)
    ↓
Link
    ↓
Success
```

### Technical Considerations

- **Macro Handling**: All macros are converted to `-DMACRO=value` format
- **Output Directory**: Preprocessed files are placed in `build/temp/` subdirectories
- **Caching**: Output file hashes enable incremental compilation
- **Parallelization**: Multiple fypp files can be preprocessed in parallel
- **Error Handling**: fypp compilation errors are reported with source line information

## Example: Building stdlib with fpm

Once full integration is complete, stdlib can be built with:

```bash
fpm build
```

Instead of requiring:

```bash
python config/fypp_deployment.py --maxrank 7 --build
```

The `fpm.toml` would include:

```toml
[preprocess.fypp]
suffixes = [".fypp"]
directories = ["src", "test"]
output_suffix = "F90"
macros = ["MAXRANK=7"]
```

## Development Roadmap

### Phase 1: Configuration Support ✓
- [x] Extend preprocess_config_t with output_suffix
- [x] Add fypp-specific module with utilities
- [x] TOML serialization/deserialization
- [x] Build validation

### Phase 2: Source Discovery (In Progress)
- [ ] Track .fypp files during source discovery
- [ ] Store preprocessing metadata
- [ ] Extend package_t to track preprocessing

### Phase 3: Build Target Integration
- [ ] Create FPM_TARGET_PREPROCESS target type
- [ ] Generate preprocessing targets from sources
- [ ] Link preprocessing targets as dependencies

### Phase 4: Backend Integration
- [ ] Add preprocessing execution in build_target()
- [ ] Handle preprocessing errors gracefully
- [ ] Support incremental builds

### Phase 5: Testing & Documentation
- [ ] Unit tests for fypp utilities
- [ ] Integration tests with stdlib
- [ ] User documentation
- [ ] Example project

## References

- fypp Project: https://github.com/aradi/fypp
- stdlib: https://github.com/fortran-lang/stdlib
- Original stdlib preprocessing: config/fypp_deployment.py

## Contributing

This is an active development effort. See the main fpm repository for contribution guidelines.

For discussion and feedback on fypp integration, please open issues or pull requests on the fpm repository.
