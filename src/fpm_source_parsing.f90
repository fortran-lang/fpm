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
use fpm_error, only: error_t, file_parse_error, fatal_error, file_not_found_error
use fpm_strings, only: string_t, string_cat, len_trim, split, lower, str_ends_with, fnv_1a, is_fortran_name
use fpm_model, only: srcfile_t, &
                    FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                    FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                    FPM_UNIT_CSOURCE, FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, &
                    FPM_SCOPE_LIB, FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST, &
                    FPM_UNIT_CPPSOURCE
use fpm_filesystem, only: read_lines, read_lines_expanded, exists
implicit none

private
public :: parse_f_source, parse_c_source, parse_use_statement

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

    logical :: inside_module, inside_interface, using, intrinsic_module
    integer :: stat
    integer :: fh, n_use, n_include, n_mod, n_parent, i, j, ic, pass
    type(string_t), allocatable :: file_lines(:), file_lines_lower(:)
    character(:), allocatable :: temp_string, mod_name, string_parts(:)

    if (.not. exists(f_filename)) then
        call file_not_found_error(error, f_filename)
        return
    end if

    f_source%file_name = f_filename

    file_lines = read_lines_expanded(f_filename)

    ! for efficiency in parsing make a lowercase left-adjusted copy of the file
    ! Need a copy because INCLUDE (and #include) file arguments are case-sensitive
    file_lines_lower=file_lines
    do i=1,size(file_lines_lower)
       file_lines_lower(i)%s=adjustl(lower(file_lines_lower(i)%s))
    enddo

    ! fnv_1a can only be applied to non-zero-length arrays
    if (len_trim(file_lines_lower) > 0) f_source%digest = fnv_1a(file_lines)

    do pass = 1,2
        n_use = 0
        n_include = 0
        n_mod = 0
        n_parent = 0
        inside_module = .false.
        inside_interface = .false.
        file_loop: do i=1,size(file_lines_lower)

            ! Skip comment lines and preprocessor directives
            if (index(file_lines_lower(i)%s,'!') == 1 .or. &
                index(file_lines_lower(i)%s,'#') == 1 .or. &
                len_trim(file_lines_lower(i)%s) < 1) then
                cycle
            end if

            ! Detect exported C-API via bind(C)
            if (.not.inside_interface .and. &
                parse_subsequence(file_lines_lower(i)%s,'bind','(','c')) then

                do j=i,1,-1

                    if (index(file_lines_lower(j)%s,'function') > 0 .or. &
                        index(file_lines_lower(j)%s,'subroutine') > 0) then
                        f_source%unit_type = FPM_UNIT_SUBPROGRAM
                        exit
                    end if

                    if (j>1) then

                        ic = index(file_lines_lower(j-1)%s,'!')
                        if (ic < 1) then
                            ic = len(file_lines_lower(j-1)%s)
                        end if

                        temp_string = trim(file_lines_lower(j-1)%s(1:ic))
                        if (index(temp_string,'&') /= len(temp_string)) then
                            exit
                        end if

                    end if

                end do

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

            ! Detect beginning of interface block
            if (index(file_lines_lower(i)%s,'interface') == 1 &
                .or. parse_sequence(file_lines_lower(i)%s,'abstract','interface')) then

                inside_interface = .true.
                cycle

            end if

            ! Detect end of interface block
            if (parse_sequence(file_lines_lower(i)%s,'end','interface')) then

                inside_interface = .false.
                cycle

            end if

            ! Process 'USE' statements
            call parse_use_statement(f_filename,i,file_lines_lower(i)%s,using,intrinsic_module,mod_name,error)
            if (allocated(error)) return

            if (using) then

                ! Not a valid module name?
                if (.not.is_fortran_name(mod_name)) cycle

                ! Valid intrinsic module: not a dependency
                if (intrinsic_module) cycle

                n_use = n_use + 1

                if (pass == 2) f_source%modules_used(n_use)%s = mod_name

                cycle

            endif

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
            ! - no modules allowed after program def
            ! - program header may be missing (only "end program" statement present)
            if (index(file_lines_lower(i)%s,'program ')==1 .or. &
                parse_sequence(file_lines_lower(i)%s,'end','program')) then

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
            if (parse_sequence(file_lines_lower(i)%s,'end','module') .or. &
                parse_sequence(file_lines_lower(i)%s,'end','submodule')) then

                inside_module = .false.
                cycle

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


!> Parsing of c, cpp source files
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

    else if (str_ends_with(lower(c_filename), ".h")) then

        c_source%unit_type = FPM_UNIT_CHEADER

    else if (str_ends_with(lower(c_filename), ".cpp")) then

        c_source%unit_type = FPM_UNIT_CPPSOURCE

    end if

    allocate(c_source%modules_used(0))
    allocate(c_source%modules_provided(0))
    allocate(c_source%parent_modules(0))

    file_lines = read_lines(c_filename)

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
            allocate(character(len=0) :: substring) ! ifort bus error otherwise
            stat = 1
            return
        end if
    else
        i = n
    end if

    if (i>size(string_parts)) then
        allocate(character(len=0) :: substring) ! ifort bus error otherwise
        stat = 1
        return
    end if

    substring = trim(adjustl(string_parts(i)))
    stat = 0

end function split_n


!> Parse a subsequence of blank-separated tokens within a string
!>  (see parse_sequence)
function parse_subsequence(string,t1,t2,t3,t4) result(found)
    character(*), intent(in) :: string
    character(*), intent(in) :: t1
    character(*), intent(in), optional :: t2, t3, t4
    logical :: found

    integer :: offset, i

    found = .false.
    offset = 1

    do

        i = index(string(offset:),t1)

        if (i == 0) return

        offset = offset + i - 1

        found = parse_sequence(string(offset:),t1,t2,t3,t4)

        if (found) return

        offset = offset + len(t1)

        if (offset > len(string)) return

    end do

end function parse_subsequence

!> Helper utility to parse sequences of tokens
!> that may be optionally separated by zero or more spaces
function parse_sequence(string,t1,t2,t3,t4) result(found)
    character(*), intent(in) :: string
    character(*), intent(in) :: t1
    character(*), intent(in), optional :: t2, t3, t4
    logical :: found

    integer :: post, n, incr, pos, token_n
    logical :: match

    n = len(string)
    found = .false.
    pos = 1

    do token_n=1,4

        do while (pos <= n)
            if (string(pos:pos) /= ' ') then
                exit
            end if
            pos = pos + 1
        end do

        select case(token_n)
        case(1)
            incr = len(t1)
            if (pos+incr-1>n) return
            match = string(pos:pos+incr-1) == t1
        case(2)
            if (.not.present(t2)) exit
            incr = len(t2)
            if (pos+incr-1>n) return
            match = string(pos:pos+incr-1) == t2
        case(3)
            if (.not.present(t3)) exit
            incr = len(t3)
            if (pos+incr-1>n) return
            match = string(pos:pos+incr-1) == t3
        case(4)
            if (.not.present(t4)) exit
            incr = len(t4)
            if (pos+incr-1>n) return
            match = string(pos:pos+incr-1) == t4
        case default
            exit
        end select

        if (.not.match) then
            return
        end if

        pos = pos + incr

    end do

    found = .true.

end function parse_sequence

! USE [, intrinsic] :: module_name [, only: only_list]
! USE [, non_intrinsic] :: module_name [, only: only_list]
subroutine parse_use_statement(f_filename,i,line,use_stmt,is_intrinsic,module_name,error)

    !> Current file name and line number (for error messaging)
    character(*), intent(in) :: f_filename
    integer, intent(in) :: i

    !> The line being parsed. MUST BE preprocessed with trim(adjustl()
    character(*), intent(in) :: line

    !> Does this line contain a `use` statement?
    logical, intent(out) :: use_stmt

    !> Is the module in this statement intrinsic?
    logical, intent(out) :: is_intrinsic

    !> used module name
    character(:), allocatable, intent(out) :: module_name

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    character(15), parameter :: INTRINSIC_NAMES(*) =  &
                                 ['iso_c_binding  ', &
                                  'iso_fortran_env', &
                                  'ieee_arithmetic', &
                                  'ieee_exceptions', &
                                  'ieee_features  ', &
                                  'omp_lib        ']

    character(len=:), allocatable :: temp_string
    integer :: colons,intr,nonintr,j,stat
    logical :: has_intrinsic_name

    use_stmt      = .false.
    is_intrinsic  = .false.
    if (len_trim(line)<=0) return

    ! Quick check that the line is preprocessed
    if (line(1:1)==' ') then
        call fatal_error(error,'internal_error: source file line is not trim(adjustl()) on input to parse_use_statement')
        return
    end if

    ! 'use' should be the first string in the adjustl line
    use_stmt = index(line,'use ')==1 .or. index(line,'use::')==1 .or. index(line,'use,')==1
    if (.not.use_stmt) return
    colons   = index(line,'::')
    nonintr  = 0
    intr     = 0

    have_colons: if (colons>3) then

        ! there may be an intrinsic/non-intrinsic spec
        nonintr = index(line(1:colons-1),'non_intrinsic')
        if (nonintr==0) intr = index(line(1:colons-1),'intrinsic')


        temp_string = split_n(line,delims=':',n=2,stat=stat)
        if (stat /= 0) then
            call file_parse_error(error,f_filename, &
                    'unable to find used module name',i, &
                    line,colons)
            return
        end if

        module_name = split_n(temp_string,delims=' ,',n=1,stat=stat)
        if (stat /= 0) then
            call file_parse_error(error,f_filename, &
                     'unable to find used module name',i, &
                     line)
            return
        end if

    else

        module_name = split_n(line,n=2,delims=' ,',stat=stat)
        if (stat /= 0) then
            call file_parse_error(error,f_filename, &
                    'unable to find used module name',i, &
                    line)
            return
        end if

    end if have_colons

    ! If declared intrinsic, check that it is true
    has_intrinsic_name = any([(index(module_name,trim(INTRINSIC_NAMES(j)))>0, &
                             j=1,size(INTRINSIC_NAMES))])
    if (intr>0 .and. .not.has_intrinsic_name) then

        ! An intrinsic module was not found. Its name could be in the next line,
        ! in which case, we just skip this check. The compiler will do the job if the name is invalid.

        ! Module name was not read: it's in the next line
        if (index(module_name,'&')<=0) then
            call file_parse_error(error,f_filename, &
                                  'module '//module_name//' is declared intrinsic but it is not ',i, &
                                  line)
            return
        endif
    endif

    ! Should we treat this as an intrinsic module
    is_intrinsic = nonintr==0 .and. & ! not declared non-intrinsic
                   (intr>0 .or. has_intrinsic_name)

end subroutine parse_use_statement



end module fpm_source_parsing

