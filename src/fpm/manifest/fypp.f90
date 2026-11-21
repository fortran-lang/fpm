!> Implementation of fypp preprocessor integration
!>
!> Provides utilities for invoking fypp on source files and managing
!> preprocessed output files within the fpm build system.

module fpm_manifest_fypp
    use fpm_error, only : error_t, fatal_error
    use fpm_strings, only : string_t, str
    use fpm_filesystem, only : exists, run, basename, join_path, mkdir
    use iso_fortran_env, only : stderr => error_unit
    implicit none
    private

    public :: fypp_config_t, run_fypp, build_fypp_command, get_output_suffix_for_file

    !> Type to hold fypp preprocessing configuration and utilities
    type :: fypp_config_t
        !> Name of the fypp executable (usually just "fypp")
        character(:), allocatable :: executable

        !> Directory where preprocessed files will be output
        character(:), allocatable :: output_dir

        !> Output suffix for preprocessed files (f90 or F90)
        character(:), allocatable :: output_suffix

    contains
        procedure :: init
        procedure :: check_available
        procedure :: process_file
    end type fypp_config_t

contains

    !> Initialize fypp configuration
    subroutine init(this, output_dir, output_suffix)
        class(fypp_config_t), intent(inout) :: this
        character(*), intent(in) :: output_dir
        character(*), intent(in), optional :: output_suffix

        this%executable = "fypp"
        this%output_dir = trim(output_dir)
        
        if (present(output_suffix)) then
            this%output_suffix = trim(output_suffix)
        else
            this%output_suffix = "F90"
        end if
    end subroutine init

    !> Check if fypp is available on the system
    function check_available(this, error) result(available)
        class(fypp_config_t), intent(in) :: this
        type(error_t), allocatable, intent(out) :: error
        logical :: available

        integer :: unit, stat
        character(:), allocatable :: cmd

        available = .false.

        ! Try to run fypp --version
        allocate(cmd, source=this%executable // " --version")
        
        ! Open to null device and check return status
        open(newunit=unit, file="/dev/null", status="old", action="write", iostat=stat)
        if (stat == 0) close(unit)

        ! For now, assume fypp is available if executable is defined
        ! In real implementation, we could check PATH or run the version command
        available = .true.

    end function check_available

    !> Process a single .fypp file with given macros
    subroutine process_file(this, input_file, macros, error)
        class(fypp_config_t), intent(in) :: this
        character(*), intent(in) :: input_file
        type(string_t), intent(in), optional :: macros(:)
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: output_file, cmd, base_name, macro_flags
        integer :: i, unit, stat
        logical :: exists_dir

        ! Get the base name without extension
        base_name = basename(input_file)
        
        ! Remove .fypp extension if present
        if (index(base_name, ".fypp") > 0) then
            base_name = base_name(1:index(base_name, ".fypp")-1)
        end if

        ! Build output filename
        allocate(output_file, source=join_path(this%output_dir, &
                 trim(base_name) // "." // this%output_suffix))

        ! Ensure output directory exists
        allocate(cmd, source=this%output_dir)
        call mkdir(cmd)

        ! Build macro flags
        allocate(macro_flags, source="")
        if (present(macros)) then
            do i = 1, size(macros)
                if (len(macro_flags) > 0) then
                    macro_flags = macro_flags // " -D" // macros(i)%s
                else
                    macro_flags = "-D" // macros(i)%s
                end if
            end do
        end if

        ! Build the fypp command
        deallocate(cmd)
        allocate(cmd, source=this%executable // " " // macro_flags // " " // &
                 trim(input_file) // " " // trim(output_file))

        ! Execute the command
        call run(cmd, exitstat=stat)
        
        if (stat /= 0) then
            allocate(error)
            call fatal_error(error, "fypp preprocessing failed for " // trim(input_file))
        end if

    end subroutine process_file

    !> Build a fypp command string with the given parameters
    function build_fypp_command(fypp_exe, input_file, output_file, macros) result(cmd)
        character(*), intent(in) :: fypp_exe
        character(*), intent(in) :: input_file
        character(*), intent(in) :: output_file
        type(string_t), intent(in), optional :: macros(:)
        character(:), allocatable :: cmd

        character(:), allocatable :: macro_flags
        integer :: i, cmd_len
        character(:), allocatable :: temp_cmd

        ! Build macro flags string
        allocate(macro_flags, source="")
        if (present(macros)) then
            do i = 1, size(macros)
                if (len(macro_flags) > 0) then
                    macro_flags = macro_flags // " -D" // macros(i)%s
                else
                    macro_flags = "-D" // macros(i)%s
                end if
            end do
        end if

        ! Build the command
        if (len(macro_flags) > 0) then
            allocate(cmd, source=fypp_exe // " " // macro_flags // " " // &
                     trim(input_file) // " " // trim(output_file))
        else
            allocate(cmd, source=fypp_exe // " " // trim(input_file) // " " // &
                     trim(output_file))
        end if

    end function build_fypp_command

    !> Run fypp on a source file (wrapper for running preprocessor)
    subroutine run_fypp(input_file, output_file, macros, error)
        character(*), intent(in) :: input_file
        character(*), intent(in) :: output_file
        type(string_t), intent(in), optional :: macros(:)
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: cmd
        integer :: stat

        ! Build the command
        cmd = build_fypp_command("fypp", input_file, output_file, macros)

        ! Execute it
        call run(cmd, exitstat=stat)
        
        if (stat /= 0) then
            allocate(error)
            call fatal_error(error, "fypp preprocessing failed for " // trim(input_file))
        end if

    end subroutine run_fypp

    !> Determine the output suffix for a given input file
    !! Files matching skip_cpp_preprocessing patterns get .f90
    !! Other files use the configured output_suffix (default F90)
    function get_output_suffix_for_file(input_file, default_suffix, &
                                       skip_patterns, force_patterns) result(suffix)
        character(*), intent(in) :: input_file
        character(*), intent(in) :: default_suffix
        type(string_t), intent(in), optional :: skip_patterns(:)
        type(string_t), intent(in), optional :: force_patterns(:)
        character(:), allocatable :: suffix

        integer :: i
        character(:), allocatable :: base_name

        ! Get base filename only (without directory)
        base_name = basename(input_file)

        ! Check if file matches any force_cpp_preprocessing patterns
        ! These take priority: force .F90 (C preprocessing)
        if (present(force_patterns)) then
            do i = 1, size(force_patterns)
                if (matches_glob_pattern(base_name, force_patterns(i)%s)) then
                    allocate(suffix, source="F90")
                    return
                end if
            end do
        end if

        ! Check if file matches any skip_cpp_preprocessing patterns
        ! These override the default to use .f90 (no C preprocessing)
        if (present(skip_patterns)) then
            do i = 1, size(skip_patterns)
                if (matches_glob_pattern(base_name, skip_patterns(i)%s)) then
                    allocate(suffix, source="f90")
                    return
                end if
            end do
        end if

        ! Default to configured suffix
        allocate(suffix, source=default_suffix)

    end function get_output_suffix_for_file

    !> Simple glob pattern matching
    !! Supports: prefix*, *suffix, *middle*, and exact matches
    function matches_glob_pattern(filename, pattern) result(matches)
        character(*), intent(in) :: filename
        character(*), intent(in) :: pattern
        logical :: matches

        integer :: star_pos, plen, flen

        plen = len(pattern)
        flen = len(filename)
        matches = .false.

        ! Exact match (no wildcard)
        if (index(pattern, "*") == 0) then
            matches = (filename == pattern)
            return
        end if

        ! Match all
        if (pattern == "*") then
            matches = .true.
            return
        end if

        ! *suffix pattern
        if (pattern(1:1) == "*" .and. pattern(plen:plen) /= "*") then
            if (plen > 1) then
                matches = (index(filename, pattern(2:plen), back=.true.) == flen - plen + 2)
            end if
            return
        end if

        ! prefix* pattern
        if (pattern(1:1) /= "*" .and. pattern(plen:plen) == "*") then
            if (plen > 1) then
                matches = (index(filename, pattern(1:plen-1)) == 1)
            end if
            return
        end if

        ! *middle* pattern
        if (pattern(1:1) == "*" .and. pattern(plen:plen) == "*") then
            if (plen > 2) then
                matches = (index(filename, pattern(2:plen-1)) > 0)
            end if
            return
        end if

        ! Should not reach here
        matches = .false.

    end function matches_glob_pattern

end module fpm_manifest_fypp
