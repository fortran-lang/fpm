module FPM_Sourcefiles
use FPM_Strings
use FPM_Filesystem, only: read_lines
implicit none

private
public srcfile_ptr, srcfile_t
public scan_f_sources

integer, parameter, public :: FPM_UNIT_UNKNOWN = -1
integer, parameter, public :: FPM_UNIT_PROGRAM = 1
integer, parameter, public :: FPM_UNIT_MODULE = 2
integer, parameter, public :: FPM_UNIT_SUBMODULE = 3
integer, parameter, public :: FPM_UNIT_SUBPROGRAM = 4
integer, parameter, public :: FPM_UNIT_CSOURCE = 5
integer, parameter, public :: FPM_UNIT_CHEADER = 6

character(15), parameter :: INTRINSIC_MODULE_NAMES(*) =  &
                             ['iso_c_binding  ', &
                              'iso_fortran_env']

type srcfile_ptr
    ! For constructing arrays of src_file pointers
    type(srcfile_t), pointer :: ptr => NULL()
end type srcfile_ptr

type srcfile_t
    ! Type for encapsulating a source file 
    !  and it's metadata
    character(:), allocatable :: file_name
        ! File path relative to cwd
    character(:), allocatable :: unit_name
        ! Module/program name
    integer :: unit_type = FPM_UNIT_UNKNOWN
        ! Type of program unit
    type(string_t), allocatable :: module_dependencies(:)
        ! Modules USEd by this source file (lowerstring)
    type(string_t), allocatable :: include_dependencies(:)
        ! Files INCLUDEd by this source file
    type(srcfile_ptr), allocatable :: file_dependencies(:)
        ! Resolved source file dependencies

    logical :: built = .false.
    logical :: touched = .false.
end type srcfile_t


contains

subroutine scan_f_sources(file_names,f_sources)
    ! Enumerate Fortran sources and resolve file
    !  dependencies
    !
    type(string_t), intent(in) :: file_names(:)
    type(srcfile_t), allocatable, intent(out), target :: f_sources(:)

    integer :: i, j
    logical :: is_f_source(size(file_names))
    type(string_t), allocatable :: f_file_names(:)

    is_f_source = [(str_ends_with(lower(file_names(i)%s), ".f90"),i=1,size(file_names))]
    f_file_names = pack(file_names,is_f_source)

    allocate(f_sources(size(f_file_names)))

    do i = 1, size(f_file_names)

        f_sources(i) = parse_f_source(f_file_names(i)%s)

    end do

    do i=1,size(f_sources)
        write(*,*) 'Filename: "',f_sources(i)%file_name,'"'
        write(*,*) ' Module name: "',f_sources(i)%unit_name,'"'
        do j=1,size(f_sources(i)%module_dependencies)
            write(*,*) ' Uses: "',f_sources(i)%module_dependencies(j)%s,'"'
        end do
        do j=1,size(f_sources(i)%include_dependencies)
            write(*,*) ' Includes: "',f_sources(i)%include_dependencies(j)%s,'"'
        end do
    end do

    call resolve_f_dependencies(f_sources)

end subroutine scan_f_sources


function parse_f_source(f_filename) result(f_source)
    ! Rudimentary scan of Fortran source file and 
    !  extract program unit name and use/include dependencies
    !
    character(*), intent(in) :: f_filename
    type(srcfile_t) :: f_source

    integer :: fh, n_use, n_include, i, j, pass
    type(string_t), allocatable :: file_lines(:)
    character(:), allocatable :: line_parts(:)
    character(:), allocatable :: temp_string, use_module_name

    f_source%file_name = f_filename

    open(newunit=fh,file=f_filename,status='old')
    file_lines = read_lines(fh)
    close(fh)

    do pass = 1,2
        n_use = 0
        n_include = 0
        file_loop: do i=1,size(file_lines)

            ! Process 'USE' statements
            if (index(adjustl(lower(file_lines(i)%s)),'use') == 1) then

                if (index(file_lines(i)%s,'::') > 0) then

                    call split(file_lines(i)%s,line_parts,delimiters=':')
                    temp_string = line_parts(2)
                    call split(temp_string,line_parts,delimiters=' ,')
                    use_module_name = trim(lower(line_parts(1)))

                else

                    call split(file_lines(i)%s,line_parts,delimiters=' ,')
                    use_module_name = trim(lower(line_parts(2)))
                    
                end if

                if (any([(index(use_module_name,trim(INTRINSIC_MODULE_NAMES(j)))>0, &
                          j=1,size(INTRINSIC_MODULE_NAMES))])) then
                    cycle
                end if

                n_use = n_use + 1

                if (pass == 2) then

                    f_source%module_dependencies(n_use)%s = use_module_name

                end if

            end if

            ! Process 'INCLUDE' statements
            if (index(adjustl(lower(file_lines(i)%s)),'include') == 1) then
                        
                n_include = n_include + 1

                if (pass == 2) then
                    call split(file_lines(i)%s,line_parts,delimiters="'")
                    f_source%include_dependencies(n_include)%s = line_parts(2)
                end if

            end if

            ! Extract name of module if is module
            if (f_source%unit_type == FPM_UNIT_UNKNOWN .and. &
                 index(adjustl(lower(file_lines(i)%s)),'module') == 1) then
                        
                call split(file_lines(i)%s,line_parts,delimiters=' ')

                f_source%unit_name = adjustl(trim(lower(line_parts(2))))
                f_source%unit_type = FPM_UNIT_MODULE

            end if

            ! Extract name of submodule if is submodule
            if (index(adjustl(lower(file_lines(i)%s)),'submodule') == 1) then
                        
                call split(file_lines(i)%s,line_parts,delimiters=' ()')

                f_source%unit_name = adjustl(trim(lower(line_parts(3))))
                f_source%unit_type = FPM_UNIT_SUBMODULE

                n_use = n_use + 1

                if (pass == 2) then

                    f_source%module_dependencies(n_use)%s = adjustl(trim(lower(line_parts(2))))

                end if

            end if

            ! Extract name of program if is program
            if (f_source%unit_type == FPM_UNIT_UNKNOWN .and. &
                index(adjustl(lower(file_lines(i)%s)),'program') == 1) then
                
                call split(file_lines(i)%s,line_parts,delimiters=' ')

                f_source%unit_name = adjustl(trim(lower(line_parts(2))))
                f_source%unit_type = FPM_UNIT_PROGRAM

            end if

        end do file_loop

        ! Default to subprogram unit type
        if (f_source%unit_type == FPM_UNIT_UNKNOWN) then
            f_source%unit_type = FPM_UNIT_SUBPROGRAM
        end if

        if (pass == 1) then
            allocate(f_source%module_dependencies(n_use))
            allocate(f_source%include_dependencies(n_include))
        end if

    end do

end function parse_f_source


subroutine resolve_f_dependencies(f_sources)
    ! After enumerating all source files: resolve file dependencies
    !  by searching on module names & include files
    !
    type(srcfile_t), intent(inout), target :: f_sources(:)

    integer :: n_use, n_include, n_depend
    integer ::  i, j, k

    do i=1,size(f_sources)
        
        n_use = size(f_sources(i)%module_dependencies)
        n_include = size(f_sources(i)%include_dependencies)
        n_depend = n_use + n_include

        allocate(f_sources(i)%file_dependencies(n_depend))

        do j=1,n_use

            do k=1,size(f_sources)

                if (f_sources(i)%module_dependencies(j)%s == f_sources(k)%unit_name) then
                    f_sources(i)%file_dependencies(j)%ptr => f_sources(k)
                    exit
                end if
                
            end do

            if (.not.associated(f_sources(i)%file_dependencies(j)%ptr)) then
                write(*,*) '(!) Unable to find source for module dependency: ',f_sources(i)%module_dependencies(j)%s
                stop
            end if

        end do

        do j=1,n_include

            do k=1,size(f_sources)

                if (index(f_sources(k)%file_name,f_sources(i)%include_dependencies(j)%s) > 0) then
                    f_sources(i)%file_dependencies(n_use+j)%ptr => f_sources(k)
                    exit
                end if
                
            end do

            if (.not.associated(f_sources(i)%file_dependencies(n_use+j)%ptr)) then
                write(*,*) '(!) Unable to find source for include dependency: ',f_sources(i)%include_dependencies(j)%s
                stop
            end if

        end do

    end do

end subroutine resolve_f_dependencies



end module FPM_Sourcefiles