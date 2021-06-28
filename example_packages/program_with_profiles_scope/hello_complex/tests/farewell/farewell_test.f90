program farewell_test
    use farewell_m, only: make_farewell
    use iso_fortran_env, only: error_unit, output_unit

    implicit none

    character(len=:), allocatable :: farewell

    allocate(character(len=0) :: farewell)
    farewell = make_farewell("World")

    if (farewell == "Goodbye, World!") then
        write(output_unit, *) "Passed"
    else
        write(error_unit, *) "Failed"
        call exit(1)
    end if
end program farewell_test
