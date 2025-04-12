!># Store compiler commands in a `compile_commands.json` table
module fpm_compile_commands
    use fpm_toml, only: serializable_t, set_string, set_list, get_value, get_list, toml_table, add_table, &
        toml_array, add_array, toml_stat
    use jonquil, only: json_serialize, json_ser_config
    use fpm_strings, only: string_t, operator(==)
    use fpm_error, only: error_t, syntax_error, fatal_error
    use fpm_os, only: get_current_directory
    use shlex_module, only: shlex_split => split 
    implicit none
    
    !> Definition of a build command
    type, extends(serializable_t) :: compile_command_t
        
        type(string_t) :: directory
        
        type(string_t), allocatable :: arguments(:)
        
        type(string_t) :: file
        
        contains
        
        !> Serialization interface
        procedure :: serializable_is_same => compile_command_is_same
        procedure :: dump_to_toml         => compile_command_dump_toml
        procedure :: load_from_toml       => compile_command_load_toml
        
    end type compile_command_t    
    
    type, extends(serializable_t) :: compile_command_table_t
        
        type(compile_command_t), allocatable :: command(:)
        
        contains
        
        !> Operation
        procedure :: destroy              => cct_destroy
        procedure :: register             => cct_register
        procedure :: write                => cct_write
        
        !> Serialization interface
        procedure :: serializable_is_same => cct_is_same
        procedure :: dump_to_toml         => cct_dump_toml
        procedure :: load_from_toml       => cct_load_toml
        
        
    end type compile_command_table_t    
    
    contains
        
    !> Dump compile_command_t to toml table
    subroutine compile_command_dump_toml(self, table, error)

        !> Instance of the serializable object
        class(compile_command_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call set_string(table, "directory", self%directory, error, 'compile_command_t')
        if (allocated(error)) return
        call set_list(table, "arguments", self%arguments, error)
        if (allocated(error)) return
        call set_string(table, "file", self%file, error, 'compile_command_t')
        if (allocated(error)) return    

    end subroutine compile_command_dump_toml

    !> Read compile_command_t from toml table (no checks made at this stage)
    subroutine compile_command_load_toml(self, table, error)

        !> Instance of the serializable object
        class(compile_command_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        call get_value(table, "directory", self%directory, error, 'compile_command_t')
        if (allocated(error)) return
        call get_list(table, "arguments", self%arguments, error)
        if (allocated(error)) return   
        call get_value(table, "file", self%file, error, 'compile_command_t')
        if (allocated(error)) return

    end subroutine compile_command_load_toml

    !> Check that two compile_command_t objects are equal
    logical function compile_command_is_same(this,that)
        class(compile_command_t), intent(in) :: this
        class(serializable_t), intent(in) :: that

        compile_command_is_same = .false.

        select type (other=>that)
           type is (compile_command_t)

              if (.not.this%directory==other%directory) return
              if (.not.this%arguments==other%arguments) return
              if (.not.this%file==other%file) return

           class default
              ! Not the same type
              return
        end select

        !> All checks passed!
        compile_command_is_same = .true.

    end function compile_command_is_same
    
    !> Dump compile_command_table_t to a toml array
    subroutine cct_dump_array(self, array, error)
        !> Instance of the serializable object
        class(compile_command_table_t), intent(inout) :: self

        !> Data structure
        type(toml_array), intent(inout) :: array

        !> Error handling
        type(error_t), allocatable, intent(out) :: error      
        
        integer :: ii, stat
        type(toml_table), pointer :: item  
        
        if (.not.allocated(self%command)) return
        
        do ii = 1, size(self%command)
            associate (cmd => self%command(ii))
            
               ! Set node for this command
               call add_table(array, item, stat)
               if (stat /= toml_stat%success) then
                   call fatal_error(error, "Cannot store entry in compile_command_table_t array")
                   return
               end if                    
               call cmd%dump_to_toml(item, error)
               if (allocated(error)) return

            endassociate
        end do                
        
    end subroutine cct_dump_array
        
    !> Dump compile_command_table_t to toml table
    subroutine cct_dump_toml(self, table, error)

        !> Instance of the serializable object
        class(compile_command_table_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        integer :: stat, ii
        type(toml_array), pointer :: array
        
        if (.not.allocated(self%command)) return
        
        ! Create array
        call add_array(table, 'compile_commands', array, stat=stat)
        if (stat/=toml_stat%success .or. .not.associated(array)) then 
            call fatal_error(error,"compile_command_table_t cannot create entry")
            return
        end if
        
        ! Dump to it
        call cct_dump_array(self, array, error)

    end subroutine cct_dump_toml
    
    !> Write compile_commands.json file. Because Jonquil does not support non-named arrays, 
    !> create a custom json here. 
    subroutine cct_write(self, filename, error)

        !> Instance of the serializable object
        class(compile_command_table_t), intent(inout) :: self

        !> The file name
        character(*), intent(in) :: filename

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        type(toml_array) :: array
        type(json_ser_config) :: cfg
        integer :: stat, lun
        
        ! Init array
        array = toml_array()
        
        ! Dump information to the array
        call cct_dump_array(self, array, error)
        if (allocated(error)) return
        
        ! Open file and write to it
        open(newunit=lun,file=filename,form='formatted',action='write',status='replace',iostat=stat)
        if (stat/=0) then 
            call fatal_error(error, 'cannot open file '//filename//' for writing')
            return
        end if
        
        ! Ensure the array has no key
        if (allocated(array%key)) deallocate(array%key)
        
        cfg%indent = repeat(' ',3)
        write (lun, '(A)', iostat=stat, err=1) json_serialize(array, cfg)                
        close(lun,iostat=stat)
        
        1 if (stat/=0) then 
            call fatal_error(error, 'cannot close file '//filename//' after writing')
            return
        end if

    end subroutine cct_write
    
    !> Cleanup a compile command table
    elemental subroutine cct_destroy(self)

        !> Instance of the serializable object
        class(compile_command_table_t), intent(inout) :: self
        
        if (allocated(self%command)) deallocate(self%command)
        
    end subroutine cct_destroy
    
    !> Register a new compile command
    subroutine cct_register(self, command, error)

        !> Instance of the serializable object
        class(compile_command_table_t), intent(inout) :: self

        !> Data structure
        character(len=*), intent(in) :: command
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error    
        
        ! Local variables
        type(compile_command_t) :: cmd
        character(len=:), allocatable :: args(:), cwd, source_file
        logical :: sh_success
        integer :: i,n
        
        ! Early check
        if (len_trim(command) <= 0) then
            call syntax_error(error, "compile_command_table_t trying to register an empty command")
            return
        end if

        ! Tokenize the input command into args(:)
        args = shlex_split(command, join_spaced=.false., keep_quotes=.true., success=sh_success)
        n = size(args)
        
        if (n==0 .or. .not.sh_success) then 
            call syntax_error(error, "compile_command_table_t failed tokenizing: <"//command//">")
            return
        end if
        
        ! Get current working directory
        call get_current_directory(cwd, error)
        if (allocated(error)) return

        ! Try to find the source file
        allocate(character(len=0) :: source_file)
        find_source_file: do i = 1, n-1
            if (args(i) == "-c") then
                source_file = trim(args(i+1))
                exit find_source_file
            end if
        end do find_source_file

        ! Fallback: use last argument if not found
        if (len_trim(source_file)==0) source_file = trim(args(n))

        ! Fill in the compile_command_t
        cmd = compile_command_t(directory = string_t(cwd), &
                                arguments = [(string_t(trim(args(i))), i=1,n)], &
                                file = string_t(source_file))
        
        if (allocated(self%command)) then         
           self%command = [self%command, cmd]
        else
           allocate(self%command(1), source=cmd) 
        end if

    end subroutine cct_register
        
    !> Read compile_command_table_t from toml table (no checks made at this stage)
    subroutine cct_load_toml(self, table, error)

        !> Instance of the serializable object
        class(compile_command_table_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
!        call get_value(table, "directory", self%directory, error, 'compile_command_table_t')
!        if (allocated(error)) return
!        call get_list(table, "arguments", self%arguments, error)
!        if (allocated(error)) return   
!        call get_value(table, "file", self%file, error, 'compile_command_table_t')
!        if (allocated(error)) return

    end subroutine cct_load_toml

    !> Check that two compile_command_table_t objects are equal
    logical function cct_is_same(this,that)
        class(compile_command_table_t), intent(in) :: this
        class(serializable_t), intent(in) :: that
        
        integer :: i

        cct_is_same = .false.

        select type (other=>that)
           type is (compile_command_table_t)
            
              if (allocated(this%command).neqv.allocated(other%command)) return 
              if (allocated(this%command)) then
                  if (.not.(size  (this%command)  ==size  (other%command))) return
                  if (.not.(ubound(this%command,1)==ubound(other%command,1))) return
                  if (.not.(lbound(this%command,1)==lbound(other%command,1))) return
                  do i=lbound(this%command,1),ubound(this%command,1)
                     if (.not.this%command(i)==other%command(i)) return
                  end do
              end if

           class default
              ! Not the same type
              return
        end select

        !> All checks passed!
        cct_is_same = .true.

    end function cct_is_same        
    
end module fpm_compile_commands
