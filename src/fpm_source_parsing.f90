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
use fpm_strings, only: string_t, string_cat, len_trim, split, lower, str_ends_with, fnv_1a, &
    is_fortran_name, operator(.in.), operator(==)
use fpm_model, only: srcfile_t, &
                    FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                    FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                    FPM_UNIT_CSOURCE, FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, &
                    FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST, FPM_UNIT_CPPSOURCE
use fpm_manifest_preprocess, only: preprocess_config_t
use fpm_filesystem, only: read_lines, read_lines_expanded, exists, join_path, parent_dir
implicit none

private
public :: parse_f_source, parse_c_source, parse_use_statement

type :: cpp_block
    ! Nested block total depth
    integer :: depth = 0
    ! Whether currently inside an inactive conditional block
    logical :: inside_inactive_block = .false.
    ! Depth at which we became inactive (0 if active)
    integer :: inactive_depth = 0
    ! Current macro
    character(:), allocatable :: name
end type cpp_block

contains

!> Case-insensitive check if macro_name is in the macros list
logical function macro_in_list(macro_name, macros)
    character(*), intent(in) :: macro_name
    type(string_t), optional, intent(in) :: macros(:)
    integer :: i
    
    type(string_t) :: lmacro
    
    macro_in_list = .false.
    if (.not.present(macros)) return
    
    macro_in_list = macro_name .in. macros

end function macro_in_list

!> Check if a line is a CPP #include or Fortran include statement
!> Returns true if it's an include, and sets include_name to the file name
subroutine is_include_line(line, is_include, include_name)
    character(*), intent(in) :: line
    logical, intent(out) :: is_include
    character(:), allocatable, intent(out) :: include_name

    character(:), allocatable :: line_lower
    integer :: stat, ic

    is_include = .false.
    include_name = ""

    line_lower = adjustl(lower(line))

    ! Check for CPP #include directive: #include "file"
    if (index(line_lower, '#include') == 1 .and. index(line, '"') > 0) then
        include_name = split_n(line, n=2, delims='"', stat=stat)
        if (stat == 0 .and. len_trim(include_name) > 0) is_include = .true.
        return
    end if

    ! Check for Fortran include statement: include "file" or include 'file'
    if (index(line_lower, 'include') == 1) then
        ic = verify(line, ' ')  ! Position of first non-space (where 'include' starts)
        if (ic == 0) return
        stat = verify(line(ic+7:), ' ')  ! Find first non-space after 'include'
        if (stat == 0) return  ! Nothing after 'include'
        ic = ic + 6 + stat  ! Position of first non-space after 'include'
        if (line(ic:ic) == '"' .or. line(ic:ic) == "'") then
            include_name = split_n(line, n=2, delims="'"//'"', stat=stat)
            if (stat == 0 .and. len_trim(include_name) > 0) is_include = .true.
        end if
    end if

end subroutine is_include_line

!> Find include file in standard locations
!> Searches: source directory first, then package's include directory
function find_include_file(name, src_dir, pkg_dir) result(path)
    character(*), intent(in) :: name, src_dir, pkg_dir
    character(:), allocatable :: path

    ! Try source directory first
    path = join_path(src_dir, name)
    if (exists(path)) return

    ! Try package's include directory (../include from source dir)
    path = join_path(pkg_dir, join_path("include", name))
    if (exists(path)) return

    ! Not found
    path = ""
end function find_include_file

!> Get the value of a macro from the macros list
!> Returns empty string if macro is not defined
!> Macros are stored as "NAME" (defined, no value) or "NAME=VALUE"
function get_macro_value(macro_name, macros, found) result(value)
    character(*), intent(in) :: macro_name
    type(string_t), intent(in), optional :: macros(:)
    logical, intent(out), optional :: found
    character(:), allocatable :: value

    integer :: i, eq_pos
    character(:), allocatable :: name_part

    value = ""
    if (present(found)) found = .false.
    if (.not. present(macros)) return

    do i = 1, size(macros)
        eq_pos = index(macros(i)%s, '=')
        if (eq_pos > 0) then
            ! Macro with value: "NAME=VALUE"
            name_part = macros(i)%s(1:eq_pos-1)
            if (lower(name_part) == lower(macro_name)) then
                value = macros(i)%s(eq_pos+1:)
                if (present(found)) found = .true.
                return
            end if
        else
            ! Macro without value: "NAME" (treated as defined but empty value)
            if (lower(macros(i)%s) == lower(macro_name)) then
                value = ""
                if (present(found)) found = .true.
                return
            end if
        end if
    end do

end function get_macro_value

!> Parse a macro comparison condition (MACRO == VALUE or MACRO != VALUE)
!> Returns is_active based on comparison result, macro_name is set to the LHS
subroutine parse_macro_comparison(condition, preprocess_macros, defined_macros, is_active, macro_name)
    character(*), intent(in) :: condition
    type(string_t), optional, intent(in) :: preprocess_macros(:)
    type(string_t), optional, intent(in) :: defined_macros(:)
    logical, intent(out) :: is_active
    character(:), allocatable, intent(out) :: macro_name

    integer :: eq_pos, neq_pos
    logical :: is_equality, macro_found
    character(:), allocatable :: lhs, rhs, macro_value

    eq_pos = index(condition, '==')
    neq_pos = index(condition, '!=')

    is_equality = eq_pos > 0 .and. (neq_pos == 0 .or. eq_pos < neq_pos)

    if (is_equality) then
        lhs = trim(adjustl(condition(1:eq_pos-1)))
        rhs = trim(adjustl(condition(eq_pos+2:)))
    else
        lhs = trim(adjustl(condition(1:neq_pos-1)))
        rhs = trim(adjustl(condition(neq_pos+2:)))
    end if

    macro_name = lhs

    ! Get macro value - first check defined_macros, then preprocess_macros
    macro_value = get_macro_value(lhs, defined_macros, macro_found)
    if (.not. macro_found) then
        macro_value = get_macro_value(lhs, preprocess_macros, macro_found)
    end if

    ! Undefined macros evaluate to 0 per CPP behavior
    if (.not. macro_found) macro_value = "0"

    ! Compare values (case-insensitive)
    if (is_equality) then
        is_active = lower(macro_value) == lower(rhs)
    else
        is_active = lower(macro_value) /= lower(rhs)
    end if

end subroutine parse_macro_comparison

!> Check if condition contains a comparison operator (== or !=)
logical function has_comparison_operator(condition)
    character(*), intent(in) :: condition
    has_comparison_operator = index(condition, '==') > 0 .or. index(condition, '!=') > 0
end function has_comparison_operator

!> Parse #if or #elif condition (handles defined(), !defined(), comparisons, and simple macros)
subroutine parse_if_condition(lower_line, line, offset, heading_blanks, preprocess_macros, defined_macros, is_active, macro_name)
    character(*), intent(in) :: lower_line, line
    integer, intent(in) :: offset, heading_blanks
    type(string_t), optional, intent(in) :: preprocess_macros(:), defined_macros(:)
    logical, intent(out) :: is_active
    character(:), allocatable, intent(out) :: macro_name

    integer :: start_pos, end_pos
    character(:), allocatable :: condition

    if (index(lower_line, 'defined(') > 0) then
        ! #if/#elif defined(MACRO) or !defined(MACRO)
        start_pos = index(lower_line, 'defined(') + 8
        end_pos = index(lower_line(start_pos:), ')') - 1

        start_pos = start_pos + heading_blanks
        end_pos = end_pos + heading_blanks

        if (end_pos > 0) then
            macro_name = line(start_pos:start_pos + end_pos - 1)
            is_active = macro_in_list(macro_name, preprocess_macros) .or. &
                        macro_in_list(macro_name, defined_macros)
            if (index(lower_line, '!defined(') > 0) is_active = .not. is_active
        else
            is_active = .false.
            macro_name = ""
        end if
    else
        condition = trim(adjustl(lower_line(offset:)))

        if (has_comparison_operator(condition)) then
            call parse_macro_comparison(condition, preprocess_macros, defined_macros, is_active, macro_name)
        else
            ! Simple macro check
            start_pos = offset + heading_blanks
            end_pos = len_trim(lower_line) + heading_blanks
            macro_name = trim(adjustl(line(start_pos:end_pos)))
            is_active = macro_in_list(macro_name, preprocess_macros) .or. &
                        macro_in_list(macro_name, defined_macros)
        end if
    end if

end subroutine parse_if_condition

!> Replace a single line with a chunk of lines (in-place)
!> Replaces array(at_line) with chunk(:)
pure subroutine insert_lines(array, chunk, at_line)
    type(string_t), allocatable, intent(inout) :: array(:)
    type(string_t), intent(in) :: chunk(:)
    integer, intent(in) :: at_line

    type(string_t), allocatable :: new_array(:)
    integer :: n, m, new_size

    n = size(array)
    m = size(chunk)

    ! Bounds check: at_line must be in valid range [1, n]
    if (at_line < 1 .or. at_line > n) return

    new_size = n - 1 + m  ! Remove 1 line, add m lines

    allocate(new_array(new_size))

    ! Copy lines before at_line
    if (at_line > 1) new_array(1:at_line-1) = array(1:at_line-1)

    ! Insert chunk
    if (m > 0) new_array(at_line:at_line+m-1) = chunk

    ! Copy lines after at_line
    if (at_line < n) new_array(at_line+m:new_size) = array(at_line+1:n)

    call move_alloc(new_array, array)

end subroutine insert_lines

!> Read source file lines with include files embedded inline
!> Replaces both CPP `#include "file"` and Fortran `include "file"` with file contents
!> Searches in: source directory, ../include (default fpm include dir)
function read_lines_with_includes(filename) result(lines)
    character(*), intent(in) :: filename
    type(string_t), allocatable :: lines(:)

    type(string_t), allocatable :: include_lines(:)
    character(:), allocatable :: include_name, include_path, source_dir, pkg_dir
    integer :: i
    logical :: found_include

    lines = read_lines_expanded(filename)
    if (.not. allocated(lines) .or. size(lines) == 0) then
        allocate(lines(0))
        return
    end if

    source_dir = parent_dir(filename)
    pkg_dir = parent_dir(source_dir)

    ! Process in reverse order so indices don't shift for unprocessed lines
    do i = size(lines), 1, -1
        call is_include_line(lines(i)%s, found_include, include_name)

        if (found_include) then
            include_path = find_include_file(include_name, source_dir, pkg_dir)
            if (len_trim(include_path) > 0) then
                include_lines = read_lines(include_path)
                if (allocated(include_lines) .and. size(include_lines) > 0) then
                    call insert_lines(lines, include_lines, i)
                end if
            end if
        end if
    end do

end function read_lines_with_includes

!> Parse a #define directive and extract macro name and optional value
!> Returns macro in "NAME=VALUE" or "NAME" format, empty string if not a #define
function parse_define_directive(line) result(macro)
    character(*), intent(in) :: line
    character(:), allocatable :: macro

    character(:), allocatable :: line_lower, macro_name, macro_value
    integer :: start_pos, end_pos

    macro = ""
    line_lower = adjustl(lower(line))

    if (index(line_lower, '#define') /= 1) return

    ! Extract macro name and optional value
    ! Format: #define NAME or #define NAME VALUE
    ! Note: bounds checks are short-circuited to avoid out-of-bounds access (issue #1222)
    start_pos = 8
    do while (start_pos <= len(line))
        if (line(start_pos:start_pos) /= ' ') exit
        start_pos = start_pos + 1
    end do

    ! Find end of macro name (space or end of line)
    end_pos = start_pos
    do while (end_pos <= len(line))
        if (line(end_pos:end_pos) == ' ') exit
        end_pos = end_pos + 1
    end do
    end_pos = end_pos - 1

    if (end_pos < start_pos) return

    macro_name = line(start_pos:end_pos)

    ! Check for value after the name
    start_pos = end_pos + 1
    do while (start_pos <= len(line))
        if (line(start_pos:start_pos) /= ' ') exit
        start_pos = start_pos + 1
    end do

    if (start_pos <= len(line)) then
        macro_value = trim(line(start_pos:))
        macro = macro_name // '=' // macro_value
    else
        macro = macro_name
    end if

end function parse_define_directive

!> Add a macro to a macros array (grows the array)
subroutine add_macro(macros, macro)
    type(string_t), allocatable, intent(inout) :: macros(:)
    character(*), intent(in) :: macro

    type(string_t), allocatable :: new_macros(:)
    integer :: n

    if (.not. allocated(macros)) then
        allocate(macros(1))
        macros(1)%s = macro
    else
        n = size(macros)
        allocate(new_macros(n + 1))
        new_macros(1:n) = macros
        new_macros(n + 1)%s = macro
        call move_alloc(new_macros, macros)
    end if

end subroutine add_macro

!> Start a CPP conditional block (active or inactive)
subroutine start_cpp_block(blk, lower_line, line, preprocess, defined_macros)
    type(cpp_block), intent(inout) :: blk
    character(*), intent(in) :: lower_line, line
    type(preprocess_config_t), optional, intent(in) :: preprocess
    type(string_t), optional, intent(in) :: defined_macros(:)

    logical :: is_active
    character(:), allocatable :: macro_name

    call parse_cpp_condition(lower_line, line, preprocess, is_active, macro_name, defined_macros)
    
    blk%depth = blk%depth + 1
    
    ! If we're not already in an inactive block, check this condition
    enter_inactive: if (.not. blk%inside_inactive_block) then
        blk%name = macro_name    
        if (.not. is_active) then
            ! This condition is false, so we enter an inactive block
            blk%inside_inactive_block = .true.
            blk%inactive_depth = blk%depth            
        end if
    end if enter_inactive
    
    ! If we're already in an inactive block, stay inactive regardless of this condition
        
end subroutine start_cpp_block

!> End a CPP conditional block  
subroutine end_cpp_block(blk)
    type(cpp_block), intent(inout) :: blk
    
    ! If we're ending the block where we became inactive, reactivate
    if (blk%inside_inactive_block .and. blk%depth == blk%inactive_depth) then
        blk%inside_inactive_block = .false.
        blk%inactive_depth = 0
    end if
    
    blk%depth = max(0, blk%depth - 1)
        
end subroutine end_cpp_block

!> Handle #else directive by flipping the current condition
subroutine handle_else_block(blk)
    type(cpp_block), intent(inout) :: blk
    
    ! #else only matters if we're at the same level where we became inactive
    if (blk%inside_inactive_block .and. blk%depth == blk%inactive_depth) then
        ! We're in an inactive block at this level, #else makes it active
        blk%inside_inactive_block = .false.
        blk%inactive_depth = 0
    elseif (.not. blk%inside_inactive_block .and. blk%depth > 0) then
        ! We're in an active block at this level, #else makes it inactive
        blk%inside_inactive_block = .true.
        blk%inactive_depth = blk%depth
    end if
         
end subroutine handle_else_block

!> Parse CPP conditional directive and determine if block should be active
!> defined_macros: optional additional macros from #define directives in source/includes
subroutine parse_cpp_condition(lower_line, line, preprocess, is_active, macro_name, defined_macros)
    character(*), intent(in) :: lower_line, line
    type(preprocess_config_t), optional, intent(in) :: preprocess
    character(:), allocatable, intent(out) :: macro_name
    logical, intent(out) :: is_active
    type(string_t), optional, intent(in) :: defined_macros(:)

    integer :: start_pos, heading_blanks, i

    ! Always active if CPP preprocessor is not active
    if (.not. present(preprocess)) then
        is_active = .true.
        macro_name = ""
        return
    endif

    ! If CPP is not enabled, always active
    if (.not. preprocess%is_cpp()) then
        is_active = .true.
        macro_name = ""
        return
    endif

    ! Find offset between lowercase adjustl and standard line
    heading_blanks = 0
    do i=1,len(line)
        if (line(i:i)==' ') then
            heading_blanks = heading_blanks+1
        else
            exit
        end if
    end do

    ! There are macros: test if active
    if (index(lower_line, '#ifdef') == 1) then
        start_pos = index(lower_line, ' ') + heading_blanks + 1
        macro_name = trim(adjustl(line(start_pos:)))
        is_active = macro_in_list(macro_name, preprocess%macros) .or. &
                    macro_in_list(macro_name, defined_macros)

    elseif (index(lower_line, '#ifndef') == 1) then
        start_pos = index(lower_line, ' ') + heading_blanks + 1
        macro_name = trim(adjustl(line(start_pos:)))
        is_active = .not. (macro_in_list(macro_name, preprocess%macros) .or. &
                           macro_in_list(macro_name, defined_macros))

    elseif (index(lower_line, '#if ') == 1) then
        call parse_if_condition(lower_line, line, 4, heading_blanks, &
                                preprocess%macros, defined_macros, is_active, macro_name)

    elseif (index(lower_line, '#elif') == 1) then
        call parse_if_condition(lower_line, line, 6, heading_blanks, &
                                preprocess%macros, defined_macros, is_active, macro_name)
    else
        is_active = .false.
        macro_name = ""
    end if

end subroutine parse_cpp_condition

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
function parse_f_source(f_filename,error,preprocess) result(f_source)
    character(*), intent(in) :: f_filename
    type(error_t), allocatable, intent(out) :: error
    type(preprocess_config_t), optional, intent(in) :: preprocess
    type(srcfile_t) :: f_source

    logical :: inside_module, inside_interface, using, intrinsic_module
    logical :: cpp_conditional_parsing
    integer :: stat
    integer :: fh, n_use, n_include, n_mod, n_parent, i, j, ic, pass
    type(cpp_block) :: cpp_blk
    type(string_t), allocatable :: file_lines(:), file_lines_lower(:)
    type(string_t), allocatable :: defined_macros(:)
    character(:), allocatable :: temp_string, mod_name, string_parts(:)
    character(:), allocatable :: parsed_macro

    if (.not. exists(f_filename)) then
        call file_not_found_error(error, f_filename)
        return
    end if

    f_source%file_name = f_filename

    ! Only use conditional parsing if preprocessing is enabled with CPP
    cpp_conditional_parsing = .false.
    if (present(preprocess)) cpp_conditional_parsing = preprocess%is_cpp()

    ! Read file with #include directives expanded inline (for CPP parsing)
    if (cpp_conditional_parsing) then
        file_lines = read_lines_with_includes(f_filename)
    else
        file_lines = read_lines_expanded(f_filename)
    end if

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
        cpp_blk = cpp_block()  ! Initialize with default values
        if (allocated(defined_macros)) deallocate(defined_macros)  ! Reset macros each pass
        file_loop: do i=1,size(file_lines_lower)

            ! Skip comment lines and empty lines
            if (index(file_lines_lower(i)%s,'!') == 1 .or. &
                len_trim(file_lines_lower(i)%s) < 1) then
                cycle
            end if

            ! Handle preprocessor directives 
            if (index(file_lines_lower(i)%s,'#') == 1) then
                
                ! If conditional parsing is enabled, track preprocessor blocks
                if (cpp_conditional_parsing) then
                    
                    ! Check for conditional compilation directives
                    if (index(file_lines_lower(i)%s,'#ifdef') == 1 .or. &
                        index(file_lines_lower(i)%s,'#ifndef') == 1 .or. &
                        index(file_lines_lower(i)%s,'#if ') == 1) then

                        ! Determine if this conditional block should be active
                        call start_cpp_block(cpp_blk, file_lines_lower(i)%s, file_lines(i)%s, preprocess, defined_macros)

                    elseif (index(file_lines_lower(i)%s,'#endif') == 1) then

                        call end_cpp_block(cpp_blk)

                    elseif (index(file_lines_lower(i)%s,'#else') == 1) then

                        call handle_else_block(cpp_blk)

                    elseif (index(file_lines_lower(i)%s,'#elif') == 1) then

                        ! Treat #elif as #else followed by #if
                        call handle_else_block(cpp_blk)
                        call start_cpp_block(cpp_blk, file_lines_lower(i)%s, file_lines(i)%s, preprocess, defined_macros)

                    elseif (index(file_lines_lower(i)%s,'#define') == 1) then

                        ! Parse #define and add to defined_macros (only if not in inactive block)
                        if (.not. cpp_blk%inside_inactive_block) then
                            parsed_macro = parse_define_directive(file_lines(i)%s)
                            if (len_trim(parsed_macro) > 0) then
                                call add_macro(defined_macros, parsed_macro)
                            end if
                        end if

                    end if

                end if

                ! Skip all preprocessor directive lines (both old and new behavior)
                cycle
                
            end if

            ! Skip content inside conditional blocks when conditional parsing is enabled
            if (cpp_conditional_parsing .and. cpp_blk%inside_inactive_block) cycle

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
            if (index(file_lines_lower(i)%s,'include') == 1) then
                ic = verify(file_lines(i)%s, ' ')  ! Position of first non-space (where 'include' starts)
                if (ic == 0) cycle
                j = verify(file_lines(i)%s(ic+7:), ' ')  ! Find first non-space after 'include'
                if (j == 0) cycle  ! Nothing after 'include'
                ic = ic + 6 + j  ! Position of first non-space after 'include'
                if (file_lines(i)%s(ic:ic) == '"' .or. file_lines(i)%s(ic:ic) == "'") then

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

