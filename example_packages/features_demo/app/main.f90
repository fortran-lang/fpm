program main
    use features_demo
    implicit none
    
    call show_features()
    
    write(*,*) ''
    write(*,*) get_build_info()
    write(*,*) ''
    write(*,*) 'Demo completed successfully!'
    
end program main