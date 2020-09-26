# Example packages

See the table below for a list of the example packages provided in this directory including
the features demonstrated in each package and which versions of fpm are supported.


| Name                | Features                                                      | Bootstrap (Haskell) fpm | fpm |
|---------------------|---------------------------------------------------------------|:-----------------------:|:---:|
| circular_example    | Local path dependency; circular dependency                    |            Y            |  N  |
| circular_test       | Local path dependency; circular dependency                    |            Y            |  N  |
| hello_complex       | Non-standard directory layout; multiple tests and executables |            Y            |  Y  |
| hello_complex_2     | Auto-discovery of tests and executables with modules          |            N            |  Y  |
| hello_fpm           | App-only; local path dependency                               |            Y            |  N  |
| hello_world         | App-only                                                      |            Y            |  Y  |
| makefile_complex    | External build command (makefile); local path dependency      |            Y            |  N  |
| program_with_module | App-only; module+program in single source file                |            Y            |  Y  |
| submodules          | Lib-only; submodules (3 levels)                               |            N            |  Y  |
| with_c              | Compile with `c` source files                                 |            N            |  Y  |
| with_makefile       | External build command (makefile)                             |            Y            |  N  |