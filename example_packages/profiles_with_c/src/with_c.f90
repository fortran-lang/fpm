module with_c
    use iso_c_binding, only: c_char, c_int, c_null_char
    implicit none

contains

    function system_isdir(dirname)
        ! Source (Public domain): https://github.com/urbanjost/M_system
        !
        implicit none
        character(len=*),intent(in) :: dirname
        logical                     :: system_isdir
        
        interface
            function c_isdir(dirname) bind (C,name="my_isdir") result (c_ierr)
            import c_char,c_int
            character(kind=c_char,len=1),intent(in) :: dirname(*)
            integer(kind=c_int)                     :: c_ierr
            end function c_isdir
        end interface
        
        system_isdir= c_isdir(trim(dirname)//c_null_char) == 1
        
    end function system_isdir

end module with_c