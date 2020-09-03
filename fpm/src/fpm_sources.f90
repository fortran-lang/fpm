module fpm_sources
use fpm_filesystem, only: read_lines, list_files
use fpm_strings, only: lower, split, str_ends_with, string_t
implicit none

private
public :: srcfile_ptr, srcfile_t
public :: scan_sources, resolve_dependencies
public :: FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
          FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
          FPM_UNIT_CHEADER

integer, parameter :: FPM_UNIT_UNKNOWN = -1
integer, parameter :: FPM_UNIT_PROGRAM = 1
integer, parameter :: FPM_UNIT_MODULE = 2
integer, parameter :: FPM_UNIT_SUBMODULE = 3
integer, parameter :: FPM_UNIT_SUBPROGRAM = 4
integer, parameter :: FPM_UNIT_CSOURCE = 5
integer, parameter :: FPM_UNIT_CHEADER = 6

character(15), parameter :: INTRINSIC_MODULE_NAMES(*) =  &
                             ['iso_c_binding  ', &
                              'iso_fortran_env']

type srcfile_ptr
    ! For constructing arrays of src_file pointers
    type(srcfile_t), pointer :: ptr => null()
end type srcfile_ptr

type srcfile_t
    ! Type for encapsulating a source file 
    !  and it's metadata
    character(:), allocatable :: file_name
        ! File path relative to cwd
    type(string_t), allocatable :: modules_provided(:)
        ! Modules provided by this source file (lowerstring)
    integer :: unit_type = FPM_UNIT_UNKNOWN
        ! Type of program unit
    type(string_t), allocatable :: modules_used(:)
        ! Modules USEd by this source file (lowerstring)
    type(string_t), allocatable :: include_dependencies(:)
        ! Files INCLUDEd by this source file
    type(srcfile_ptr), allocatable :: file_dependencies(:)
        ! Resolved source file dependencies

    logical :: built = .false.
    logical :: touched = .false.
end type srcfile_t


contains

subroutine scan_sources(sources,directories)
    ! Enumerate Fortran sources and resolve file
    !  dependencies
    !
    type(srcfile_t), allocatable, intent(out), target :: sources(:)
    type(string_t), intent(in) :: directories(:)

    integer :: i, j
    logical, allocatable :: is_source(:)
    type(string_t), allocatable :: dir_files(:)
    type(string_t), allocatable :: file_names(:)
    type(string_t), allocatable :: src_file_names(:)

    ! Scan directories for sources
    allocate(file_names(0))
    do i=1,size(directories)

        call list_files(directories(i)%s, dir_files)
        file_names = [file_names,(string_t(directories(i)%s//'/'//dir_files(j)%s),j=1,size(dir_files))]

    end do

    is_source = [(str_ends_with(lower(file_names(i)%s), ".f90") .or. &
                  str_ends_with(lower(file_names(i)%s), ".c") .or. &
                  str_ends_with(lower(file_names(i)%s), ".h"),i=1,size(file_names))]
    src_file_names = pack(file_names,is_source)

    allocate(sources(size(src_file_names)))

    do i = 1, size(src_file_names)

        if (str_ends_with(lower(src_file_names(i)%s), ".f90")) then
            sources(i) = parse_f_source(src_file_names(i)%s)
        end if

        if (str_ends_with(lower(src_file_names(i)%s), ".c") .or. &
            str_ends_with(lower(src_file_names(i)%s), ".h")) then
            sources(i) = parse_c_source(src_file_names(i)%s)
        end if

    end do

    ! do i=1,size(sources)
    !     write(*,*) 'Filename: "',sources(i)%file_name,'"'
    !     do j=1,size(sources(i)%modules_provided)
    !         write(*,*) ' Provides: "',sources(i)%modules_provided(j)%s,'"'
    !     end do
    !     do j=1,size(sources(i)%modules_used)
    !         write(*,*) ' Uses: "',sources(i)%modules_used(j)%s,'"'
    !     end do
    !     do j=1,size(sources(i)%include_dependencies)
    !         write(*,*) ' Includes: "',sources(i)%include_dependencies(j)%s,'"'
    !     end do
    ! end do

end subroutine scan_sources


function parse_f_source(f_filename) result(f_source)
    ! Rudimentary scan of Fortran source file and 
    !  extract program unit name and use/include dependencies
    !
    character(*), intent(in) :: f_filename
    type(srcfile_t) :: f_source

    integer :: fh, n_use, n_include, n_mod, i, j, ic, pass
    type(string_t), allocatable :: file_lines(:)
    character(:), allocatable :: line_parts(:)
    character(:), allocatable :: temp_string, mod_name

    f_source%file_name = f_filename

    open(newunit=fh,file=f_filename,status='old')
    file_lines = read_lines(fh)
    close(fh)

    do pass = 1,2
        n_use = 0
        n_include = 0
        n_mod = 0
        file_loop: do i=1,size(file_lines)

            ! Skip lines that are continued: not statements
            if (i > 1) then
                ic = index(file_lines(i-1)%s,'!')
                if (ic < 1) then
                    ic = len(file_lines(i-1)%s)
                end if
                temp_string = trim(file_lines(i-1)%s(1:ic))
                if (len(temp_string) > 0 .and. index(temp_string,'&') == len(temp_string)) then
                    cycle
                end if
            end if

            ! Process 'USE' statements
            if (index(adjustl(lower(file_lines(i)%s)),'use ') == 1 .or. &
                index(adjustl(lower(file_lines(i)%s)),'use::') == 1) then

                if (index(file_lines(i)%s,'::') > 0) then

                    call split(file_lines(i)%s,line_parts,delimiters=':')
                    temp_string = line_parts(2)
                    call split(temp_string,line_parts,delimiters=' ,')
                    mod_name = trim(lower(line_parts(1)))

                else

                    call split(file_lines(i)%s,line_parts,delimiters=' ,')
                    mod_name = trim(lower(line_parts(2)))
    
                end if

                if (.not.validate_name(mod_name)) then
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

            end if

            ! Process 'INCLUDE' statements
            if (index(adjustl(lower(file_lines(i)%s)),'include') == 1) then
    
                n_include = n_include + 1

                if (pass == 2) then
                    call split(file_lines(i)%s,line_parts,delimiters="'"//'"')
                    f_source%include_dependencies(n_include)%s = line_parts(2)
                end if

            end if

            ! Extract name of module if is module
            if (index(adjustl(lower(file_lines(i)%s)),'module ') == 1) then

                call split(file_lines(i)%s,line_parts,delimiters=' ')

                mod_name = adjustl(trim(lower(line_parts(2))))

                if (.not.validate_name(mod_name)) then
                    cycle
                end if

                n_mod = n_mod + 1

                if (pass == 2) then
                    f_source%modules_provided(n_mod) = string_t(mod_name)
                end if

                f_source%unit_type = FPM_UNIT_MODULE

            end if

            ! Extract name of submodule if is submodule
            if (index(adjustl(lower(file_lines(i)%s)),'submodule') == 1) then

                call split(file_lines(i)%s,line_parts,delimiters=' ()')

                f_source%unit_type = FPM_UNIT_SUBMODULE

                n_use = n_use + 1

                if (pass == 2) then

                    if (index(line_parts(2),':') > 0) then

                        line_parts(2) = line_parts(2)(index(line_parts(2),':')+1:)

                    end if

                    f_source%modules_used(n_use)%s = adjustl(trim(lower(line_parts(2))))

                end if

            end if

            ! Extract name of program if is program
            if (f_source%unit_type == FPM_UNIT_UNKNOWN .and. &
                index(adjustl(lower(file_lines(i)%s)),'program') == 1) then

                call split(file_lines(i)%s,line_parts,delimiters=' ')

                f_source%unit_type = FPM_UNIT_PROGRAM

            end if

        end do file_loop

        ! Default to subprogram unit type
        if (f_source%unit_type == FPM_UNIT_UNKNOWN) then
            f_source%unit_type = FPM_UNIT_SUBPROGRAM
        end if

        if (pass == 1) then
            allocate(f_source%modules_used(n_use))
            allocate(f_source%include_dependencies(n_include))
            allocate(f_source%modules_provided(n_mod))
        end if

    end do

    contains

    function validate_name(name) result(valid)
        character(*), intent(in) :: name
        logical :: valid

        integer :: i

        if (trim(lower(name)) == 'procedure' .or. &
            trim(lower(name)) == 'subroutine' .or. &
            trim(lower(name)) == 'function') then

            valid = .false.
            return
        end if

        if (lower(name(1:1)) < 'a' .or. &
            lower(name(1:1)) > 'z') then

            valid = .false.
            return
        end if

        do i=1,len(name)

            if (.not.( &
                (name(i:i) >= '0' .and. name(i:i) <= '9').or. &
                (lower(name(i:i)) >= 'a' .and. lower(name(i:i)) <= 'z').or. &
                name(i:i) == '_') ) then
                    
                valid = .false.
                return
            end if

        end do

        valid = .true.
        return

    end function validate_name

end function parse_f_source


function parse_c_source(c_filename) result(c_source)
    ! Rudimentary scan of c source file and 
    !  extract include dependencies
    !
    character(*), intent(in) :: c_filename
    type(srcfile_t) :: c_source

    integer :: fh, n_include, i, pass
    type(string_t), allocatable :: file_lines(:)
    character(:), allocatable :: line_parts(:)

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

    do pass = 1,2
        n_include = 0
        file_loop: do i=1,size(file_lines)

            ! Process 'INCLUDE' statements
            if (index(adjustl(lower(file_lines(i)%s)),'#include') == 1 .and. &
                index(file_lines(i)%s,'"') > 0) then
                            
                n_include = n_include + 1

                if (pass == 2) then
                    call split(file_lines(i)%s,line_parts,delimiters='"')
                    c_source%include_dependencies(n_include)%s = line_parts(2)
                end if

            end if

        end do file_loop

        if (pass == 1) then
            allocate(c_source%include_dependencies(n_include))
        end if

    end do

end function parse_c_source


subroutine resolve_dependencies(sources)
    ! After enumerating all source files: resolve file dependencies
    !  by searching on module names
    !
    type(srcfile_t), intent(inout), target :: sources(:)

    integer :: n_depend, i, j

    do i=1,size(sources)
        
        n_depend = size(sources(i)%modules_used)

        allocate(sources(i)%file_dependencies(n_depend))

        do j=1,n_depend

            sources(i)%file_dependencies(j)%ptr => &
                 find_module_dependency(sources,sources(i)%modules_used(j)%s)

            if (.not.associated(sources(i)%file_dependencies(j)%ptr)) then
                write(*,*) '(!) Unable to find source for module dependency: ',sources(i)%modules_used(j)%s
                ! stop
            end if

        end do

    end do    

end subroutine resolve_dependencies

function find_module_dependency(sources,module_name) result(src_ptr)
    type(srcfile_t), intent(in), target :: sources(:)
    character(*), intent(in) :: module_name
    type(srcfile_t), pointer :: src_ptr

    integer :: k, l

    src_ptr => NULL()

    do k=1,size(sources)

        do l=1,size(sources(k)%modules_provided)

            if (module_name == sources(k)%modules_provided(l)%s) then
                src_ptr => sources(k)
                exit
            end if

        end do
        
    end do

end function find_module_dependency

end module fpm_sources
