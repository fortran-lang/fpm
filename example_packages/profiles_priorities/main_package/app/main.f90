program main_package
    use d1_m, only: say_hi
    use d2_m, only: count_to_ten
    
    call say_hi()
    call count_to_ten()
end program main_package

