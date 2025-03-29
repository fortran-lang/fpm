!># Store compiler commands in a `compile_commands.json` table
module fpm_compile_commands
    use fpm_toml, only: serializable_t, set_string, set_list, get_value, get_list, toml_table
    use fpm_strings, only: string_t, operator(==)
    use fpm_error, only: error_t
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
        
    !> Dump compile_command_table_t to toml table
    subroutine cct_dump_toml(self, table, error)

        !> Instance of the serializable object
        class(compile_command_table_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

!        call set_string(table, "directory", self%directory, error, 'compile_command_table_t')
!        if (allocated(error)) return
!        call set_list(table, "arguments", self%arguments, error)
!        if (allocated(error)) return
!        call set_string(table, "file", self%file, error, 'compile_command_table_t')
!        if (allocated(error)) return    

    end subroutine cct_dump_toml

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
