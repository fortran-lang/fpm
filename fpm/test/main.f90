!> Driver for unit testing
program fpm_testing
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testsuite, only : run_testsuite
    use test_toml, only : collect_toml
    use test_manifest, only : collect_manifest
    use test_source_parsing, only : collect_source_parsing
    implicit none
    integer :: stat
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    write(error_unit, fmt) "Testing:", "fpm_toml"
    call run_testsuite(collect_toml, error_unit, stat)

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "tests failed!"
        error stop 1
    end if

    write(error_unit, fmt) "Testing:", "fpm_manifest"
    call run_testsuite(collect_manifest, error_unit, stat)

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "tests failed!"
        error stop 1
    end if

    write(error_unit, fmt) "Testing:", "fpm_sources (parsing)"
    call run_testsuite(collect_source_parsing, error_unit, stat)

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "tests failed!"
        error stop 1
    end if

end program fpm_testing
