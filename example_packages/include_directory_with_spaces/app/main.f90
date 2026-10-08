program main
    use iso_c_binding, only: c_int
    implicit none
    interface
        function c_payload() bind(c) result(value)
            import c_int
            integer(c_int) :: value
        end function
    end interface
    if (c_payload() /= 29) error stop 1
end program
