!> Implementation for interacting with git repositories.
module fpm_git
    use fpm_error, only: error_t, fatal_error
    implicit none

    public :: git_target_t
    public :: git_target_default, git_target_branch, git_target_tag, &
        & git_target_revision


    !> Possible git target
    type :: enum_descriptor

        !> Default target
        integer :: default = 200

        !> Branch in git repository
        integer :: branch = 201

        !> Tag in git repository
        integer :: tag = 202

        !> Commit hash
        integer :: revision = 203

    end type enum_descriptor

    !> Actual enumerator for descriptors
    type(enum_descriptor), parameter :: git_descriptor = enum_descriptor()


    !> Description of an git target
    type :: git_target_t
        private

        !> Kind of the git target
        integer :: descriptor = git_descriptor%default

        !> Target URL of the git repository
        character(len=:), allocatable :: url

        !> Additional descriptor of the git object
        character(len=:), allocatable :: object

    contains

        !> Fetch and checkout in local directory
        procedure :: checkout

        !> Show information on instance
        procedure :: info

    end type git_target_t


contains


    !> Default target
    function git_target_default(url) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%default
        self%url = url

    end function git_target_default


    !> Target a branch in the git repository
    function git_target_branch(url, branch) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Name of the branch of interest
        character(len=*), intent(in) :: branch

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%branch
        self%url = url
        self%object = branch

    end function git_target_branch


    !> Target a specific git revision
    function git_target_revision(url, sha1) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Commit hash of interest
        character(len=*), intent(in) :: sha1

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%revision
        self%url = url
        self%object = sha1

    end function git_target_revision


    !> Target a git tag
    function git_target_tag(url, tag) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Tag name of interest
        character(len=*), intent(in) :: tag

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%tag
        self%url = url
        self%object = tag

    end function git_target_tag


    subroutine checkout(self,local_path, error)

        !> Instance of the git target
        class(git_target_t), intent(in) :: self

        !> Local path to checkout in
        character(*), intent(in) :: local_path

        !> Error
        type(error_t), allocatable, intent(out) :: error

        !> git object ref
        character(:), allocatable :: object

        !> Stat for execute_command_line
        integer :: stat

        if (allocated(self%object)) then
            object = self%object
        else
            object = 'HEAD'
        end if

        call execute_command_line("git init "//local_path, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while initiating git repository for remote dependency')
            return
        end if

        call execute_command_line("git -C "//local_path//" fetch "//self%url//&
                                        " "//object, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while fetching git repository for remote dependency')
            return
        end if

        call execute_command_line("git -C "//local_path//" checkout -qf FETCH_HEAD", exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while checking out git repository for remote dependency')
            return
        end if

    end subroutine checkout


    !> Show information on git target
    subroutine info(self, unit, verbosity)

        !> Instance of the git target
        class(git_target_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Git target"
        if (allocated(self%url)) then
            write(unit, fmt) "- URL", self%url
        end if
        if (allocated(self%object)) then
            select case(self%descriptor)
            case default
                write(unit, fmt) "- object", self%object
            case(git_descriptor%tag)
                write(unit, fmt) "- tag", self%object
            case(git_descriptor%branch)
                write(unit, fmt) "- branch", self%object
            case(git_descriptor%revision)
                write(unit, fmt) "- sha1", self%object
            end select
        end if

    end subroutine info


end module fpm_git
