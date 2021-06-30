    module fpm_global
    type modes
       logical :: verbose=.true.
       character(len=10) :: color_mode='color'
    end type modes
    type(modes),public :: config
    end module fpm_global
