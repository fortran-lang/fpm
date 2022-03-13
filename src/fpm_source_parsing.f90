!># Parsing of package source files
!>
!> This module exposes two functions, `[[parse_f_source]]` and `[[parse_c_source]]`,
!> which perform a rudimentary parsing of fortran and c source files
!> in order to extract information required for module dependency tracking.
!>
!> Both functions additionally calculate and store a file digest (hash) which
!> is used by the backend ([[fpm_backend]]) to skip compilation of unmodified sources.
!>
!> Both functions return an instance of the [[srcfile_t]] type.
!>
!> For more information, please read the documentation for each function:
!>
!> - `[[parse_f_source]]`
!> - `[[parse_c_source]]`
!>
module fpm_source_parsing
use fpm_error, only: error_t, file_parse_error, fatal_error
use fpm_strings, only: string_t, string_cat, len_trim, split, lower, str_ends_with, fnv_1a, is_fortran_name
use fpm_model, only: srcfile_t, &
                    FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                    FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                    FPM_UNIT_CSOURCE, FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, &
                    FPM_SCOPE_LIB, FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST
use fpm_filesystem, only: read_lines, read_lines_expanded
implicit none

private
public :: parse_f_source, parse_c_source

character(15), parameter :: INTRINSIC_MODULE_NAMES(*) =  &
                             ['iso_c_binding  ', &
                              'iso_fortran_env', &
                              'ieee_arithmetic', &
                              'ieee_exceptions', &
                              'ieee_features  ', &
                              'omp_lib        ']

contains

!> Parsing of free-form fortran source files
!>
!> The following statements are recognised and parsed:
!>
!> - `Module`/`submodule`/`program` declaration
!> - Module `use` statement
!> - `include` statement
!>
!> @note Intrinsic modules used by sources are not listed in
!> the `modules_used` field of source objects.
!>
!> @note Submodules are treated as normal modules which `use` their
!> corresponding parent modules.
!>
!>### Parsing limitations
!>
!> __Statements must not continued onto another line
!>  except for an `only:` list in the `use` statement.__
!>
!> This is supported:
!>
!>```fortran
!> use my_module, only: &
!>      my_var, my_function, my_subroutine
!>```
!>
!> This is __NOT supported:__
!>
!>```fortran
!> use &
!>    my_module
!>```
!>
function parse_f_source(f_filename,error) result(f_source)
    character(*), intent(in) :: f_filename
    type(srcfile_t) :: f_source
    type(error_t), allocatable, intent(out) :: error

    logical :: inside_module
    integer :: stat
    integer :: fh, n_use, n_include, n_mod, n_parent, i, j, ic, pass
    type(string_t), allocatable :: file_lines(:), file_lines_lower(:)
    character(:), allocatable :: temp_string, mod_name, string_parts(:)

    f_source%file_name = f_filename

    open(newunit=fh,file=f_filename,status='old')
    file_lines = read_lines_expanded(fh)
    close(fh)

    ! for efficiency in parsing make a lowercase left-adjusted copy of the file
    ! Need a copy because INCLUDE (and #include) file arguments are case-sensitive
    file_lines_lower=file_lines
    do i=1,size(file_lines_lower)
       file_lines_lower(i)%s=adjustl(lower(file_lines_lower(i)%s))
    enddo

    ! Ignore empty files, returned as FPM_UNIT_UNKNOWN
    if (len_trim(file_lines_lower) < 1) return

    f_source%digest = fnv_1a(file_lines)

    do pass = 1,2
        n_use = 0
        n_include = 0
        n_mod = 0
        n_parent = 0
        inside_module = .false.
        file_loop: do i=1,size(file_lines_lower)

            ! Skip comment lines
            if (index(file_lines_lower(i)%s,'!') == 1 .or. &
                len_trim(file_lines_lower(i)%s) < 1) then
                cycle
            end if

            ! Skip lines that are continued: not statements
            if (i > 1) then
                ic = index(file_lines_lower(i-1)%s,'!')
                if (ic < 1) then
                    ic = len(file_lines_lower(i-1)%s)
                end if
                temp_string = trim(file_lines_lower(i-1)%s(1:ic))
                if (len(temp_string) > 0 .and. index(temp_string,'&') == len(temp_string)) then
                    cycle
                end if
            end if

            ! Process 'USE' statements
            if (index(file_lines_lower(i)%s,'use ') == 1 .or. &
                index(file_lines_lower(i)%s,'use::') == 1) then

                if (index(file_lines_lower(i)%s,'::') > 0) then

                    temp_string = split_n(file_lines_lower(i)%s,delims=':',n=2,stat=stat)
                    if (stat /= 0) then
                        call file_parse_error(error,f_filename, &
                                'unable to find used module name',i, &
                                file_lines_lower(i)%s,index(file_lines_lower(i)%s,'::'))
                        return
                    end if

                    mod_name = split_n(temp_string,delims=' ,',n=1,stat=stat)
                    if (stat /= 0) then
                        call file_parse_error(error,f_filename, &
                                 'unable to find used module name',i, &
                                 file_lines_lower(i)%s)
                        return
                    end if

                else

                    mod_name = split_n(file_lines_lower(i)%s,n=2,delims=' ,',stat=stat)
                    if (stat /= 0) then
                        call file_parse_error(error,f_filename, &
                                'unable to find used module name',i, &
                                file_lines_lower(i)%s)
                        return
                    end if

                end if

                if (.not.is_fortran_name(mod_name)) then
                    cycle
                end if

                if (any([(index(mod_name,trim(INTRINSIC_MODULE_NAMES(j)))>0, &
                            j=1,size(INTRINSIC_MODULE_NAMES))])) then
                    cycle
                end if

                n_use = n_use + 1

                if (pass == 2) then

                    f_source%modules_used(n_use)%s = mod_name

                end if

                cycle

            end if

            ! Process 'INCLUDE' statements
            ic = index(file_lines_lower(i)%s,'include')
            if ( ic == 1 ) then
                ic = index(lower(file_lines(i)%s),'include')
                if (index(adjustl(file_lines(i)%s(ic+7:)),'"') == 1 .or. &
                    index(adjustl(file_lines(i)%s(ic+7:)),"'") == 1 ) then

                    n_include = n_include + 1

                    if (pass == 2) then
                        f_source%include_dependencies(n_include)%s = &
                         & split_n(file_lines(i)%s,n=2,delims="'"//'"',stat=stat)
                        if (stat /= 0) then
                            call file_parse_error(error,f_filename, &
                                  'unable to find include file name',i, &
                                  file_lines(i)%s)
                            return
                        end if
                    end if

                    cycle

                end if
            end if

            ! Extract name of module if is module
            if (index(file_lines_lower(i)%s,'module ') == 1) then

                ! Remove any trailing comments
                ic = index(file_lines_lower(i)%s,'!')-1
                if (ic < 1) then
                    ic = len(file_lines_lower(i)%s)
                end if
                temp_string = trim(file_lines_lower(i)%s(1:ic))

                ! R1405 module-stmt := "MODULE" module-name
                ! module-stmt has two space-delimited parts only
                ! (no line continuations)
                call split(temp_string,string_parts,' ')
                if (size(string_parts) /= 2) then
                    cycle
                end if

                mod_name = trim(adjustl(string_parts(2)))
                if (scan(mod_name,'=(&')>0 ) then
                    ! Ignore these cases:
                    ! module <something>&
                    ! module =*
                    ! module (i)
                    cycle
                end if

                if (.not.is_fortran_name(mod_name)) then
                    call file_parse_error(error,f_filename, &
                          'empty or invalid name for module',i, &
                          file_lines_lower(i)%s, index(file_lines_lower(i)%s,mod_name))
                    return
                end if

                n_mod = n_mod + 1

                if (pass == 2) then
                    f_source%modules_provided(n_mod) = string_t(mod_name)
                end if

                if (f_source%unit_type == FPM_UNIT_UNKNOWN) then
                    f_source%unit_type = FPM_UNIT_MODULE
                end if

                if (.not.inside_module) then    
                    inside_module = .true.
                else
                    ! Must have missed an end module statement (can't assume a pure module)
                    if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
                        f_source%unit_type = FPM_UNIT_SUBPROGRAM
                    end if
                end if

                cycle

            end if

            ! Extract name of submodule if is submodule
            if (index(file_lines_lower(i)%s,'submodule') == 1) then

                mod_name = split_n(file_lines_lower(i)%s,n=3,delims='()',stat=stat)
                if (stat /= 0) then
                    call file_parse_error(error,f_filename, &
                          'unable to get submodule name',i, &
                          file_lines_lower(i)%s)
                    return
                end if
                if (.not.is_fortran_name(mod_name)) then
                    call file_parse_error(error,f_filename, &
                          'empty or invalid name for submodule',i, &
                          file_lines_lower(i)%s, index(file_lines_lower(i)%s,mod_name))
                    return
                end if

                n_mod = n_mod + 1

                temp_string = split_n(file_lines_lower(i)%s,n=2,delims='()',stat=stat)
                if (stat /= 0) then
                    call file_parse_error(error,f_filename, &
                          'unable to get submodule ancestry',i, &
                          file_lines_lower(i)%s)
                    return
                end if
                
                if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
                    f_source%unit_type = FPM_UNIT_SUBMODULE
                end if

                n_use = n_use + 1

                inside_module = .true.

                n_parent = n_parent + 1

                if (pass == 2) then

                    if (index(temp_string,':') > 0) then

                        temp_string = temp_string(index(temp_string,':')+1:)

                    end if

                    if (.not.is_fortran_name(temp_string)) then
                        call file_parse_error(error,f_filename, &
                          'empty or invalid name for submodule parent',i, &
                          file_lines_lower(i)%s, index(file_lines_lower(i)%s,temp_string))
                        return
                    end if

                    f_source%modules_used(n_use)%s = temp_string
                    f_source%parent_modules(n_parent)%s = temp_string
                    f_source%modules_provided(n_mod)%s = mod_name

                end if

                cycle

            end if

            ! Detect if contains a program
            !  (no modules allowed after program def)
            if (index(file_lines_lower(i)%s,'program ') == 1) then

                temp_string = split_n(file_lines_lower(i)%s,n=2,delims=' ',stat=stat)
                if (stat == 0) then

                    if (scan(temp_string,'=(')>0 ) then
                        ! Ignore:
                        ! program =*
                        ! program (i) =*
                        cycle
                    end if

                end if

                f_source%unit_type = FPM_UNIT_PROGRAM

                cycle

            end if

            ! Parse end module statement
            !  (to check for code outside of modules)
            if (index(file_lines_lower(i)%s,'end') == 1) then

                temp_string = split_n(file_lines_lower(i)%s,n=2,delims=' ',stat=stat)

                if (stat == 0) then
                    if (temp_string == 'module' .or. temp_string == 'submodule') then

                        inside_module = .false.
                        cycle

                    end if
                end if

            end if

            ! Any statements not yet parsed are assumed to be other code statements
            if (.not.inside_module .and. f_source%unit_type /= FPM_UNIT_PROGRAM) then

                f_source%unit_type = FPM_UNIT_SUBPROGRAM

            end if

        end do file_loop

        ! If unable to parse end of module statement, then can't assume pure module
        !  (there could be non-module subprograms present)
        if (inside_module .and. f_source%unit_type == FPM_UNIT_MODULE) then
            f_source%unit_type = FPM_UNIT_SUBPROGRAM
        end if

        if (pass == 1) then
            allocate(f_source%modules_used(n_use))
            allocate(f_source%include_dependencies(n_include))
            allocate(f_source%modules_provided(n_mod))
            allocate(f_source%parent_modules(n_parent))
        end if

    end do

end function parse_f_source


!> Parsing of c source files
!>
!> The following statements are recognised and parsed:
!>
!> - `#include` preprocessor statement
!>
function parse_c_source(c_filename,error) result(c_source)
    character(*), intent(in) :: c_filename
    type(srcfile_t) :: c_source
    type(error_t), allocatable, intent(out) :: error

    integer :: fh, n_include, i, pass, stat
    type(string_t), allocatable :: file_lines(:)

    c_source%file_name = c_filename

    if (str_ends_with(lower(c_filename), ".c")) then

        c_source%unit_type = FPM_UNIT_CSOURCE

    elseif (str_ends_with(lower(c_filename), ".h")) then

        c_source%unit_type = FPM_UNIT_CHEADER

    end if

    allocate(c_source%modules_used(0))
    allocate(c_source%modules_provided(0))

    open(newunit=fh,file=c_filename,status='old')
    file_lines = read_lines(fh)
    close(fh)

    ! Ignore empty files, returned as FPM_UNIT_UNKNOWN
    if (len_trim(file_lines) < 1) then
        c_source%unit_type = FPM_UNIT_UNKNOWN
        return
    end if

    c_source%digest = fnv_1a(file_lines)

    do pass = 1,2
        n_include = 0
        file_loop: do i=1,size(file_lines)

            ! Process 'INCLUDE' statements
            if (index(adjustl(lower(file_lines(i)%s)),'#include') == 1 .and. &
                index(file_lines(i)%s,'"') > 0) then

                n_include = n_include + 1

                if (pass == 2) then

                    c_source%include_dependencies(n_include)%s = &
                     &   split_n(file_lines(i)%s,n=2,delims='"',stat=stat)
                    if (stat /= 0) then
                        call file_parse_error(error,c_filename, &
                            'unable to get c include file',i, &
                            file_lines(i)%s,index(file_lines(i)%s,'"'))
                        return
                    end if

                end if

            end if

        end do file_loop

        if (pass == 1) then
            allocate(c_source%include_dependencies(n_include))
        end if

    end do

end function parse_c_source

!> Split a string on one or more delimeters
!>  and return the nth substring if it exists
!>
!> n=0  will return the last item
!> n=-1 will return the penultimate item etc.
!>
!> stat = 1 on return if the index
!>  is not found
!>
function split_n(string,delims,n,stat) result(substring)

    character(*), intent(in) :: string
    character(*), intent(in) :: delims
    integer, intent(in) :: n
    integer, intent(out) :: stat
    character(:), allocatable :: substring

    integer :: i
    character(:), allocatable :: string_parts(:)

    call split(string,string_parts,delims)

    if (n<1) then
        i = size(string_parts) + n
        if (i < 1) then
            stat = 1
            return
        end if
    else
        i = n
    end if

    if (i>size(string_parts)) then
        stat = 1
        return
    end if

    substring = trim(adjustl(string_parts(i)))
    stat = 0

end function split_n

end module fpm_source_parsing
