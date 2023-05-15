!> Implementation for interacting with git repositories.
module fpm_git
    use fpm_error, only: error_t, fatal_error
    use fpm_filesystem, only : get_temp_filename, getline, join_path, execute_and_read_output
    implicit none

    public :: git_target_t, git_target_default, git_target_branch, git_target_tag, git_target_revision, git_revision, &
            & git_archive, git_matches_manifest, operator(==)

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


    interface operator(==)
        module procedure git_target_eq
    end interface

    !> Common output format for writing to the command line
    character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

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

    !> Check that two git targets are equal
    logical function git_target_eq(this,that) result(is_equal)

        !> Two input git targets
        type(git_target_t), intent(in) :: this,that

        is_equal = this%descriptor == that%descriptor .and. &
                   this%url        == that%url        .and. &
                   this%object     == that%object

    end function git_target_eq

    !> Check that a cached dependency matches a manifest request
    logical function git_matches_manifest(cached,manifest,verbosity,iunit)

        !> Two input git targets
        type(git_target_t), intent(in) :: cached,manifest

        integer, intent(in) :: verbosity,iunit

        git_matches_manifest = cached%url == manifest%url
        if (.not.git_matches_manifest) then
            if (verbosity>1) write(iunit,out_fmt) "GIT URL has changed: ",cached%url," vs. ", manifest%url
            return
        endif

        !> The manifest dependency only contains partial information (what's requested),
        !> while the cached dependency always stores a commit hash because it's built
        !> after the repo is available (saved as git_descriptor%revision==revision).
        !> So, comparing against the descriptor is not reliable
        git_matches_manifest = cached%object == manifest%object
        if (.not.git_matches_manifest) then
            if (verbosity>1) write(iunit,out_fmt) "GIT OBJECT has changed: ",cached%object," vs. ", manifest%object
        end if

    end function git_matches_manifest


    subroutine checkout(self, local_path, error)

        !> Instance of the git target
        class(git_target_t), intent(in) :: self

        !> Local path to checkout in
        character(*), intent(in) :: local_path

        !> Error
        type(error_t), allocatable, intent(out) :: error

        integer :: stat
        character(len=:), allocatable :: object, workdir

        if (allocated(self%object)) then
            object = self%object
        else
            object = 'HEAD'
        end if
        workdir = "--work-tree="//local_path//" --git-dir="//join_path(local_path, ".git")

        call execute_command_line("git init "//local_path, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while initiating git repository for remote dependency')
            return
        end if

        call execute_command_line("git "//workdir//" fetch --depth=1 "// &
                                  self%url//" "//object, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while fetching git repository for remote dependency')
            return
        end if

        call execute_command_line("git "//workdir//" checkout -qf FETCH_HEAD", exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while checking out git repository for remote dependency')
            return
        end if

    end subroutine checkout


    subroutine git_revision(local_path, object, error)

        !> Local path to checkout in
        character(*), intent(in) :: local_path

        !> Git object reference
        character(len=:), allocatable, intent(out) :: object

        !> Error
        type(error_t), allocatable, intent(out) :: error

        integer :: stat, unit, istart, iend
        character(len=:), allocatable :: temp_file, line, iomsg, workdir
        character(len=*), parameter :: hexdigits = '0123456789abcdef'

        workdir = "--work-tree="//local_path//" --git-dir="//join_path(local_path, ".git")
        allocate(temp_file, source=get_temp_filename())
        line = "git "//workdir//" log -n 1 > "//temp_file
        call execute_command_line(line, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error, "Error while retrieving commit information")
            return
        end if

        open(file=temp_file, newunit=unit)
        call getline(unit, line, stat, iomsg)

        if (stat /= 0) then
            call fatal_error(error, iomsg)
            return
        end if
        close(unit, status="delete")

        ! Tokenize:
        ! commit 0123456789abcdef (HEAD, ...)
        istart = scan(line, ' ') + 1
        iend = verify(line(istart:), hexdigits) + istart - 1
        if (iend < istart) iend = len(line)
        object = line(istart:iend)

    end subroutine git_revision


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

  !> Archive a folder using `git archive`.
  subroutine git_archive(source, destination, error)
    !> Directory to archive.
    character(*), intent(in) :: source
    !> Destination of the archive.
    character(*), intent(in) :: destination
    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    integer :: stat
    character(len=:), allocatable :: cmd_output, archive_format

    call execute_and_read_output('git archive -l', cmd_output, error)
    if (allocated(error)) return

    if (index(cmd_output, 'tar.gz') /= 0) then
      archive_format = 'tar.gz'
    else
      call fatal_error(error, "Cannot find a suitable archive format for 'git archive'."); return
    end if

    call execute_command_line('git archive HEAD --format='//archive_format//' -o '// destination, exitstat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Error packing '"//source//"'."); return
    end if
  end


end module fpm_git
