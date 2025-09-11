program debug_demo
    use features_demo
    implicit none
    
    write(*,*) 'Debug Demo Program'
    write(*,*) '=================='
    
#ifdef DEBUG
    write(*,*) 'Debug mode: ON'
#else
    write(*,*) 'Debug mode: OFF'
#endif
    
    call show_features()
    
end program debug_demo
