    module fpm_global
    type modes
       logical :: verbose=.true.
    end type modes
    type(modes),public :: config
    end module fpm_global
