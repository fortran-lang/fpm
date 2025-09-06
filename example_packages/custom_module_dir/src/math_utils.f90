module math_utils
    implicit none
    private
    public :: add_numbers, multiply_numbers

contains

    function add_numbers(a, b) result(sum)
        integer, intent(in) :: a, b
        integer :: sum
        sum = a + b
    end function add_numbers

    function multiply_numbers(a, b) result(product)
        integer, intent(in) :: a, b
        integer :: product
        product = a * b
    end function multiply_numbers

end module math_utils