module metapackage_dep_child
    use omp_lib
    implicit none
contains
    function get_num_threads() result(n)
        integer :: n
        !$omp parallel
        n = omp_get_num_threads()
        !$omp end parallel
    end function get_num_threads
end module metapackage_dep_child
