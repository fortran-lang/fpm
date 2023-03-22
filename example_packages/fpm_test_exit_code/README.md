# fpm_test_exit_code
Test program for application exit codes
see https://github.com/fortran-lang/fpm/issues/848

This app expects to receive an integer command line argument, to check whether it is odd or even.
It returns 0 on success (odd input), or among a few error codes otherwise.
