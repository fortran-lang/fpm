program metapackage_netcdf
    use netcdf4_f03
    implicit none

    integer(c_int) :: ncid, retval

    retval = nf_create("dummy.nc", NF_INMEMORY, ncid)
    if (retval .ne. nf_noerr) error stop
    stop 0
end program metapackage_netcdf
