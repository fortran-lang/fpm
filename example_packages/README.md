# Example packages

See the table below for a list of the example packages provided in this directory including
the features demonstrated in each package and which versions of fpm are supported.

| Name                        | Features                                                      | Bootstrap (Haskell) fpm | fpm |
| --------------------------- | ------------------------------------------------------------- | :---------------------: | :-: |
| app_with_c                  | C files located in app directory (not src)                    |            N            |  Y  |
| app_with_submodule          | Submodules located in app directory (not src)                 |            N            |  Y  |
| auto_discovery_off          | Default layout with auto-discovery disabled                   |            N            |  Y  |
| c_header_only               | C header-only library                                         |            N            |  Y  |
| c_includes                  | C library with c include directory and dependency includes    |            N            |  Y  |
| circular_example            | Local path dependency; circular dependency                    |            Y            |  Y  |
| circular_test               | Local path dependency; circular dependency                    |            Y            |  Y  |
| c_main                      | C App                                                         |            N            |  Y  |
| c_main_preprocess           | C App; propagate command line preprocessor macros to the app  |            N            |  Y  |
| cpp_files                   | C++ files get compiled using fpm                              |            N            |  Y  |
| fortran_includes            | Fortran library with explicit include directory               |            Y            |  N  |
| fpm_test_exe_issues         | Test parse order of module files and apps                     |            N            |  Y  |
| hello_complex               | Non-standard directory layout; multiple tests and executables |            Y            |  Y  |
| hello_complex_2             | Auto-discovery of tests and executables with modules          |            N            |  Y  |
| hello_fpm                   | App-only; local path dependency                               |            Y            |  Y  |
| hello_fpm_path              | Define local path dependencies                                |            N            |  Y  |
| hello_world                 | App-only                                                      |            Y            |  Y  |
| link_executable             | Link external library to a single executable                  |            N            |  Y  |
| link_external               | Link external library                                         |            N            |  Y  |
| makefile_complex            | External build command (makefile); local path dependency      |            Y            |  N  |
| preprocess_cpp              | Lib only; C preprocessing; Macro parsing                      |            N            |  Y  |
| preprocess_cpp_c            | C App; progate macros from fpm.toml to app                    |            N            |  Y  |
| preprocess_cpp_deps         | App; cpp preprocessor settings in local path dependency only  |            N            |  Y  |
| preprocess_hello            | App only; Macros remain local to the package                  |            N            |  Y  |
| preprocess_hello_dependency | Lib only; Macros not getting passed here from root            |            N            |  Y  |
| program_with_module         | App-only; module+program in single source file                |            Y            |  Y  |
| submodules                  | Lib-only; submodules (3 levels)                               |            N            |  Y  |
| submodule_tree_shake        | Test tree-shaking/pruning with submodules dependencies        |            N            |  Y  |
| tree_shake                  | Test tree-shaking/pruning of unused module dependencies       |            N            |  Y  |
| version_file                | Read version number from a file in the project root           |            N            |  Y  |
| with_c                      | Compile with `c` source files                                 |            N            |  Y  |
| with_examples               | Example-only                                                  |            Y            |  Y  |
| with_makefile               | External build command (makefile)                             |            Y            |  N  |
