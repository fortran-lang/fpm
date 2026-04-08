program features_with_dependency_demo
    use features_with_dependency, only: show_features
    implicit none
    
    print *, "=== Features with Dependency Demo ==="
    print *, ""
    
    call show_features()
    
    print *, ""
    print *, "This demonstrates feature propagation to dependencies."
    
end program features_with_dependency_demo
