program metapackage_netcdf
    use netcdf4_f03
    implicit none

    integer(c_int) :: ncid, retval
    integer(c_int) :: ncsize = 16
    character(c_char), allocatable, target :: memfile(:)

    allocate (memfile(ncsize))

    retval = nf_create("dummy.nc", NF_INMEMORY, ncid)
    if (retval .ne. nf_noerr) error stop
    stop 0
end program metapackage_netcdf
