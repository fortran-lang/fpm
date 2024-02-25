!> Implementation of the meta data for an executables.
!>
!> An executable table can currently have the following fields
!>
!>```toml
!>[[ executable ]]
!>name = "string"
!>source-dir = "path"
!>main = "file"
!>link = ["lib"]
!>[executable.dependencies]
!>```
module fpm_manifest_executable
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies, resize
    use fpm_error, only : error_t, syntax_error, bad_name_error, fatal_error
    use fpm_strings, only : string_t, operator(==)
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list, serializable_t, add_table, &
                          set_string, set_list
    implicit none
    private

    public :: executable_config_t, new_executable


    !> Configuation meta data for an executable
    type, extends(serializable_t) :: executable_config_t

        !> Name of the resulting executable
        character(len=:), allocatable :: name

        !> Source directory for collecting the executable
        character(len=:), allocatable :: source_dir

        !> Name of the source file declaring the main program
        character(len=:), allocatable :: main

        !> Dependency meta data for this executable
        type(dependency_config_t), allocatable :: dependency(:)

        !> Libraries to link against
        type(string_t), allocatable :: link(:)

    contains

        !> Print information on this instance
        procedure :: info

        !> Serialization interface
        procedure :: serializable_is_same => exe_is_same
        procedure :: dump_to_toml
        procedure :: load_from_toml

    end type executable_config_t

    character(*), parameter, private :: class_name = 'executable_config_t'


contains


    !> Construct a new executable configuration from a TOML data structure
    subroutine new_executable(self, table, error)

        !> Instance of the executable configuration
        type(executable_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve executable name")
           return
        end if
        if (bad_name_error(error,'executable',self%name))then
           return
        endif
        call get_value(table, "source-dir", self%source_dir, "app")
        call get_value(table, "main", self%main, "main.f90")

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, error=error)
            if (allocated(error)) return
        end if

        call get_list(table, "link", self%link, error)
        if (allocated(error)) return

    end subroutine new_executable


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        logical :: name_present
        integer :: ikey

        name_present = .false.

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Executable section does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed as executable entry")
                exit

            case("name")
                name_present = .true.

            case("source-dir", "main", "dependencies", "link")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Executable name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the executable configuration
        class(executable_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ii
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)', &
            & fmti = '("#", 1x, a, t30, i0)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Executable target"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if
        if (allocated(self%source_dir)) then
            if (self%source_dir /= "app" .or. pr > 2) then
                write(unit, fmt) "- source directory", self%source_dir
            end if
        end if
        if (allocated(self%main)) then
            if (self%main /= "main.f90" .or. pr > 2) then
                write(unit, fmt) "- program source", self%main
            end if
        end if

        if (allocated(self%dependency)) then
            if (size(self%dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- dependencies", size(self%dependency)
            end if
            do ii = 1, size(self%dependency)
                call self%dependency(ii)%info(unit, pr - 1)
            end do
        end if

    end subroutine info


    logical function exe_is_same(this,that)
        class(executable_config_t), intent(in) :: this
        class(serializable_t), intent(in) :: that

        integer :: ii

        exe_is_same = .false.

        select type (other=>that)
           type is (executable_config_t)
              if (.not.this%link==other%link) return
              if (.not.allocated(this%name).eqv.allocated(other%name)) return
              if (.not.this%name==other%name) return
              if (.not.allocated(this%source_dir).eqv.allocated(other%source_dir)) return
              if (.not.this%source_dir==other%source_dir) return
              if (.not.allocated(this%main).eqv.allocated(other%main)) return
              if (.not.this%main==other%main) return
              if (.not.allocated(this%dependency).eqv.allocated(other%dependency)) return
              if (allocated(this%dependency)) then
                 if (.not.(size(this%dependency)==size(other%dependency))) return
                 do ii = 1, size(this%dependency)
                    if (.not.(this%dependency(ii)==other%dependency(ii))) return
                 end do
              end if
           class default
              ! Not the same type
              return
        end select

        !> All checks passed!
        exe_is_same = .true.

    end function exe_is_same

    !> Dump install config to toml table
    subroutine dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(executable_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Local variables
        integer :: ierr, ii
        type(toml_table), pointer :: ptr_deps,ptr
        character(27) :: unnamed

        call set_string(table, "name", self%name, error)
        if (allocated(error)) return
        call set_string(table, "source-dir", self%source_dir, error)
        if (allocated(error)) return
        call set_string(table, "main", self%main, error)
        if (allocated(error)) return

        if (allocated(self%dependency)) then

           ! Create dependency table
           call add_table(table, "dependencies", ptr_deps)
           if (.not. associated(ptr_deps)) then
              call fatal_error(error, class_name//" cannot create dependency table ")
              return
           end if

           do ii = 1, size(self%dependency)
              associate (dep => self%dependency(ii))

                 !> Because dependencies are named, fallback if this has no name
                 !> So, serialization will work regardless of size(self%dep) == self%ndep
                 if (len_trim(dep%name)==0) then
                    write(unnamed,1) ii
                    call add_table(ptr_deps, trim(unnamed), ptr)
                 else
                    call add_table(ptr_deps, dep%name, ptr)
                 end if
                 if (.not. associated(ptr)) then
                    call fatal_error(error, class_name//" cannot create entry for dependency "//dep%name)
                    return
                 end if
                 call dep%dump_to_toml(ptr, error)
                 if (allocated(error)) return
              end associate
           end do

        endif

        call set_list(table, "link", self%link, error)
        if (allocated(error)) return

        1 format('UNNAMED_DEPENDENCY_',i0)

    end subroutine dump_to_toml

    !> Read install config from toml table (no checks made at this stage)
    subroutine load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(executable_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Local variables
        type(toml_key), allocatable :: keys(:),dep_keys(:)
        type(toml_table), pointer :: ptr_deps,ptr
        integer :: ii, jj, ierr

        call table%get_keys(keys)

        call get_value(table, "name", self%name)
        if (allocated(error)) return
        call get_value(table, "source-dir", self%source_dir)
        if (allocated(error)) return
        call get_value(table, "main", self%main)
        if (allocated(error)) return
        call get_list(table, "link", self%link, error)

        find_deps_table: do ii = 1, size(keys)
            if (keys(ii)%key=="dependencies") then

               call get_value(table, keys(ii), ptr_deps)
               if (.not.associated(ptr_deps)) then
                  call fatal_error(error,class_name//': error retrieving dependency table from TOML table')
                  return
               end if

               !> Read all dependencies
               call ptr_deps%get_keys(dep_keys)
               call resize(self%dependency, size(dep_keys))

               do jj = 1, size(dep_keys)

                   call get_value(ptr_deps, dep_keys(jj), ptr)
                   call self%dependency(jj)%load_from_toml(ptr, error)
                   if (allocated(error)) return

               end do

               exit find_deps_table

            endif
        end do find_deps_table

    end subroutine load_from_toml


end module fpm_manifest_executable
