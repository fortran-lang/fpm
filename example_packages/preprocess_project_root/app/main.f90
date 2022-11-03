!> from https://stackoverflow.com/a/46342008
#ifdef __GFORTRAN__
# define STRINGIFY_START(X) "&
# define STRINGIFY_END(X) &X"
#else /* default stringification */
# define STRINGIFY_(X) #X
# define STRINGIFY_START(X) &
# define STRINGIFY_END(X) STRINGIFY_(X)
#endif

program preprocess_project_root
    implicit none
    character(len=*), parameter :: root = STRINGIFY_START(PROJECT_ROOT)
                                          STRINGIFY_END(PROJECT_ROOT)

    print *, root

end program preprocess_project_root
