!> Implementation of the meta data for preprocessing.
!>
!> A preprocess table can currently have the following fields
!>
!> ```toml
!> [preprocess]
!> [preprocess.cpp]
!> suffixes = ["F90", "f90"]
!> directories = ["src/feature1", "src/models"]
!> macros = []
!> ```

module fpm_manifest_preprocess
   use fpm_error, only : error_t, syntax_error
   use fpm_strings, only : string_t, operator(==)
   use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list, serializable_t, set_value, set_list, &
                        set_string
   use,intrinsic :: iso_fortran_env, only : stderr=>error_unit
   implicit none
   private

   public :: preprocess_config_t, new_preprocess_config, new_preprocessors, operator(==)

   !> Configuration meta data for a preprocessor
   type, extends(serializable_t) :: preprocess_config_t

      !> Name of the preprocessor
      character(len=:), allocatable :: name

      !> Suffixes of the files to be preprocessed
      type(string_t), allocatable :: suffixes(:)

      !> Directories to search for files to be preprocessed
      type(string_t), allocatable :: directories(:)

      !> Macros to be defined for the preprocessor
      type(string_t), allocatable :: macros(:)

   contains

      !> Print information on this instance
      procedure :: info

      !> Serialization interface
      procedure :: serializable_is_same => preprocess_is_same
      procedure :: dump_to_toml
      procedure :: load_from_toml

      !> Operations
      procedure :: destroy
      procedure :: add_config

      !> Properties
      procedure :: is_cpp
      procedure :: is_fypp

   end type preprocess_config_t

   character(*), parameter, private :: class_name = 'preprocess_config_t'

contains

   !> Construct a new preprocess configuration from TOML data structure
   subroutine new_preprocess_config(self, table, error)

      !> Instance of the preprocess configuration
      type(preprocess_config_t), intent(out) :: self

      !> Instance of the TOML data structure.
      type(toml_table), intent(inout) :: table

      !> Error handling
      type(error_t), allocatable, intent(out) :: error

      call check(table, error)
      if (allocated(error)) return

      call table%get_key(self%name)

      call get_list(table, "suffixes", self%suffixes, error)
      if (allocated(error)) return

      call get_list(table, "directories", self%directories, error)
      if (allocated(error)) return

      call get_list(table, "macros", self%macros, error)
      if (allocated(error)) return

   end subroutine new_preprocess_config

   !> Check local schema for allowed entries
   subroutine check(table, error)

      !> Instance of the TOML data structure.
      type(toml_table), intent(inout) :: table

      !> Error handling
      type(error_t), allocatable, intent(inout) :: error

      character(len=:), allocatable :: name
      type(toml_key), allocatable :: list(:)
      integer :: ikey

      call table%get_key(name)
      call table%get_keys(list)

      do ikey = 1, size(list)
         select case(list(ikey)%key)
         !> Valid keys.
         case("suffixes", "directories", "macros")
         case default
            call syntax_error(error, "Key '"//list(ikey)%key//"' not allowed in preprocessor '"//name//"'."); exit
         end select
      end do
   end subroutine check

   !> Construct new preprocess array from a TOML data structure.
   subroutine new_preprocessors(preprocessors, table, error)

      !> Instance of the preprocess configuration
      type(preprocess_config_t), allocatable, intent(out) :: preprocessors(:)

      !> Instance of the TOML data structure
      type(toml_table), intent(inout) :: table

      !> Error handling
      type(error_t), allocatable, intent(out) :: error

      type(toml_table), pointer :: node
      type(toml_key), allocatable :: list(:)
      integer :: iprep, stat

      call table%get_keys(list)

      ! An empty table is not allowed
      if (size(list) == 0) then
         call syntax_error(error, "No preprocessors defined")
      end if

      allocate(preprocessors(size(list)))
      do iprep = 1, size(list)
         call get_value(table, list(iprep)%key, node, stat=stat)
         if (stat /= toml_stat%success) then
            call syntax_error(error, "Preprocessor "//list(iprep)%key//" must be a table entry")
            exit
         end if
         call new_preprocess_config(preprocessors(iprep), node, error)
         if (allocated(error)) exit
      end do

   end subroutine new_preprocessors

   !> Write information on this instance
   subroutine info(self, unit, verbosity)

      !> Instance of the preprocess configuration
      class(preprocess_config_t), intent(in) :: self

      !> Unit for IO
      integer, intent(in) :: unit

      !> Verbosity of the printout
      integer, intent(in), optional :: verbosity

      integer :: pr, ilink
      character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

      if (present(verbosity)) then
         pr = verbosity
      else
         pr = 1
      end if

      if (pr < 1) return

      write(unit, fmt) "Preprocessor"
      if (allocated(self%name)) then
         write(unit, fmt) "- name", self%name
      end if
      if (allocated(self%suffixes)) then
         write(unit, fmt) " - suffixes"
         do ilink = 1, size(self%suffixes)
            write(unit, fmt) "   - " // self%suffixes(ilink)%s
         end do
      end if
      if (allocated(self%directories)) then
         write(unit, fmt) " - directories"
         do ilink = 1, size(self%directories)
            write(unit, fmt) "   - " // self%directories(ilink)%s
         end do
      end if
      if (allocated(self%macros)) then
         write(unit, fmt) " - macros"
         do ilink = 1, size(self%macros)
            write(unit, fmt) "   - " // self%macros(ilink)%s
         end do
      end if

   end subroutine info

   logical function preprocess_is_same(this,that)
      class(preprocess_config_t), intent(in) :: this
      class(serializable_t), intent(in) :: that

      integer :: istr

      preprocess_is_same = .false.

      select type (other=>that)
         type is (preprocess_config_t)
            if (allocated(this%name).neqv.allocated(other%name)) return
            if (allocated(this%name)) then
                if (.not.(this%name==other%name)) return
            endif

            if (.not.(this%suffixes==other%suffixes)) return
            if (.not.(this%directories==other%directories)) return
            if (.not.(this%macros==other%macros)) return

         class default
            ! Not the same type
            return
      end select

      !> All checks passed!
      preprocess_is_same = .true.

    end function preprocess_is_same

    !> Dump install config to toml table
    subroutine dump_to_toml(self, table, error)

       !> Instance of the serializable object
       class(preprocess_config_t), intent(inout) :: self

       !> Data structure
       type(toml_table), intent(inout) :: table

       !> Error handling
       type(error_t), allocatable, intent(out) :: error

       call set_string(table, "name", self%name, error)
       if (allocated(error)) return
       call set_list(table, "suffixes", self%suffixes, error)
       if (allocated(error)) return
       call set_list(table, "directories", self%directories, error)
       if (allocated(error)) return
       call set_list(table, "macros", self%macros, error)
       if (allocated(error)) return

     end subroutine dump_to_toml

     !> Read install config from toml table (no checks made at this stage)
     subroutine load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(preprocess_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call get_value(table, "name", self%name)
        call get_list(table, "suffixes", self%suffixes, error)
        if (allocated(error)) return
        call get_list(table, "directories", self%directories, error)
        if (allocated(error)) return
        call get_list(table, "macros", self%macros, error)
        if (allocated(error)) return

     end subroutine load_from_toml

    !> Clean preprocessor structure
    elemental subroutine destroy(this)
       class(preprocess_config_t), intent(inout) :: this

       if (allocated(this%name))deallocate(this%name)
       if (allocated(this%suffixes))deallocate(this%suffixes)
       if (allocated(this%directories))deallocate(this%directories)
       if (allocated(this%macros))deallocate(this%macros)

    end subroutine destroy

    !> Add preprocessor settings
    subroutine add_config(this,that)
       class(preprocess_config_t), intent(inout) :: this
        type(preprocess_config_t), intent(in) :: that

        if (.not.that%is_cpp()) then
            write(stderr, '(a)') 'Warning: Preprocessor ' // that%name // &
                                 ' is not supported; will ignore it'
            return
        end if

        if (.not.allocated(this%name)) this%name = that%name

        ! Add macros
        if (allocated(that%macros)) then
            if (allocated(this%macros)) then
                this%macros = [this%macros, that%macros]
            else
                allocate(this%macros, source = that%macros)
            end if
        endif

        ! Add suffixes
        if (allocated(that%suffixes)) then
            if (allocated(this%suffixes)) then
                this%suffixes = [this%suffixes, that%suffixes]
            else
                allocate(this%suffixes, source = that%suffixes)
            end if
        endif

        ! Add directories
        if (allocated(that%directories)) then
            if (allocated(this%directories)) then
                this%directories = [this%directories, that%directories]
            else
                allocate(this%directories, source = that%directories)
            end if
        endif

    end subroutine add_config

    ! Check cpp
    logical function is_cpp(this)
       class(preprocess_config_t), intent(in) :: this
       is_cpp = .false.
       if (allocated(this%name)) is_cpp = this%name == "cpp"
    end function is_cpp

    ! Check cpp
    logical function is_fypp(this)
       class(preprocess_config_t), intent(in) :: this
       is_fypp = .false.
       if (allocated(this%name)) is_fypp = this%name == "fypp"
    end function is_fypp

end module fpm_manifest_preprocess
