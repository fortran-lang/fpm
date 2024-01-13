program test_preprocess_suffix
   use preprocess_cpp
#ifndef TESTMACRO
   stop -1
#else
   stop 0
#endif   
end program test_preprocess_suffix
