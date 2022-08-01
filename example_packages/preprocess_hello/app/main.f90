program preprocess_hello
    use preprocess_hello_dependency, only: say_hello

    implicit none

!> The build will not break because we have FOO defined as macro in fpm.toml file
!> of this package.
#ifndef FOO
    This breaks the build inside the root package.
#endif
    call say_hello()
end program preprocess_hello
