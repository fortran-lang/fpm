program main
    use metapackage_dep_child, only: get_num_threads
    use omp_lib
    implicit none

    integer :: n

    ! This uses the child module which requires openmp
    n = get_num_threads()
    print '(a,i0)', "Threads from child module: ", n

    ! This also uses omp_lib directly - should work because openmp
    ! metapackage is propagated from the child dependency
    print '(a,i0)', "Max threads available: ", omp_get_max_threads()

end program main
