program main_package
    use d1_m, only: say_hi
    use d2_m, only: count_to_ten
    
    if (not_defined /= 1) stop 1
    call say_hi()
    call count_to_ten()
end program main_package

