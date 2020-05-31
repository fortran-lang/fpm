program greet_test
    use greet_m, only: make_greeting
    use iso_fortran_env, only: error_unit, output_unit

    implicit none

    character(len=:), allocatable :: greeting

    allocate(character(len=0) :: greeting)
    greeting = make_greeting("World")

    if (greeting == "Hello, World!") then
        write(output_unit, *) "Passed"
    else
        write(error_unit, *) "Failed"
        call exit(1)
    end if
end program greet_test
