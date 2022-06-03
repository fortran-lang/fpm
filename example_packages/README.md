# Example packages

See the table below for a list of the example packages provided in this directory including
the features demonstrated in each package and which versions of fpm are supported.


| Name                | Features                                                      | Bootstrap (Haskell) fpm | fpm |
|---------------------|---------------------------------------------------------------|:-----------------------:|:---:|
| auto_discovery_off  | Default layout with auto-discovery disabled                   |            N            |  Y  |
| c_header_only       | C header-only library                                         |            N            |  Y  |
| c_includes          | C library with c include directory and dependency includes    |            N            |  Y  |
| circular_example    | Local path dependency; circular dependency                    |            Y            |  Y  |
| circular_test       | Local path dependency; circular dependency                    |            Y            |  Y  |
| fortran_includes    | Fortran library with explicit include directory               |            Y            |  N  |
| hello_complex       | Non-standard directory layout; multiple tests and executables |            Y            |  Y  |
| hello_complex_2     | Auto-discovery of tests and executables with modules          |            N            |  Y  |
| hello_fpm           | App-only; local path dependency                               |            Y            |  Y  |
| hello_world         | App-only                                                      |            Y            |  Y  |
| with_examples       | Example-only                                                  |            Y            |  Y  |
| makefile_complex    | External build command (makefile); local path dependency      |            Y            |  N  |
| program_with_module | App-only; module+program in single source file                |            Y            |  Y  |
| submodules          | Lib-only; submodules (3 levels)                               |            N            |  Y  |
| tree_shake          | Test tree-shaking/pruning of unused module dependencies       |            N            |  Y  |
| submodule_tree_shake| Test tree-shaking/pruning with submodules dependencies        |            N            |  Y  |
| link_external       | Link external library                                         |            N            |  Y  |
| link_executable     | Link external library to a single executable                  |            N            |  Y  |
| version_file        | Read version number from a file in the project root           |            N            |  Y  |
| with_c              | Compile with `c` source files                                 |            N            |  Y  |
| with_makefile       | External build command (makefile)                             |            Y            |  N  |
