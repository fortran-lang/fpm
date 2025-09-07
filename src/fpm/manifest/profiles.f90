!> Implementation of the profiles configuration.
!>
!> A profile is a named collection of features that can be applied together.
!> Profiles provide a convenient way to group features for different use cases,
!> such as debug builds, release builds, or specific target configurations.
!>
!> A profile table has the following structure:
!>```toml
!>[profiles.debug]
!>features = ["debug-flags", "development-tools"]
!>
!>[profiles.release]  
!>features = ["optimized", "strip-symbols"]
!>```
module fpm_manifest_profile
    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_strings, only: string_t, operator(==)
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat, len
    use fpm_toml, only: get_value, serializable_t, set_string, set_list, get_list, add_table
    
    implicit none
    private

    public :: profile_config_t, new_profile, new_profiles

    !> Configuration data for a profile
    type, extends(serializable_t) :: profile_config_t

        !> Profile name
        character(len=:), allocatable :: name

        !> List of features to apply
        type(string_t), allocatable :: features(:)

    contains

        !> Print information on this instance
        procedure :: info

        !> Serialization interface
        procedure :: serializable_is_same => profile_is_same
        procedure :: dump_to_toml
        procedure :: load_from_toml

    end type profile_config_t

    character(len=*), parameter, private :: class_name = 'profile_config_t'

contains

    !> Construct a new profile configuration from a TOML array
    subroutine new_profile(self, features_array, profile_name, error)

        !> Instance of the profile configuration
        type(profile_config_t), intent(out) :: self

        !> TOML array containing the feature names
        type(toml_array), intent(inout) :: features_array

        !> Name of the profile
        character(len=*), intent(in) :: profile_name

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i, stat
        character(len=:), allocatable :: feature_name

        ! Set profile name
        self%name = profile_name

        ! Get feature names from array
        if (len(features_array) > 0) then
            allocate(self%features(len(features_array)))
            do i = 1, len(features_array)
                call get_value(features_array, i, feature_name, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Failed to read feature name from profile " // profile_name)
                    return
                end if
                self%features(i)%s = feature_name
            end do
        else
            allocate(self%features(0))
        end if

    end subroutine new_profile


    !> Construct new profiles array from a TOML data structure
    subroutine new_profiles(profiles, table, error)

        !> Instance of the profile configuration array
        type(profile_config_t), allocatable, intent(out) :: profiles(:)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_array), pointer :: array_node
        type(toml_key), allocatable :: list(:)
        integer :: iprofile, stat

        call table%get_keys(list)

        if (size(list) < 1) then
            allocate(profiles(0))
            return
        end if

        allocate(profiles(size(list)))

        do iprofile = 1, size(list)
            call get_value(table, list(iprofile)%key, array_node, stat=stat)
            if (stat /= toml_stat%success) then
                call fatal_error(error, "Profile "//list(iprofile)%key//" must be an array of feature names")
                exit
            end if
            call new_profile(profiles(iprofile), array_node, list(iprofile)%key, error)
            if (allocated(error)) exit
        end do

    end subroutine new_profiles

    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the profile configuration
        class(profile_config_t), intent(in) :: self

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

        write(unit, fmt) "Profile"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if

        if (allocated(self%features)) then
            if (size(self%features) > 0) then
                write(unit, fmti) "- features", size(self%features)
                if (pr > 1) then
                    do ii = 1, size(self%features)
                        write(unit, fmt) "  - feature", self%features(ii)%s
                    end do
                end if
            end if
        end if

    end subroutine info

    !> Check that two profile configs are equal
    logical function profile_is_same(this, that)
        class(profile_config_t), intent(in) :: this
        class(serializable_t), intent(in) :: that

        integer :: ii

        profile_is_same = .false.

        select type (other=>that)
            type is (profile_config_t)

                if (allocated(this%name).neqv.allocated(other%name)) return
                if (allocated(this%name)) then
                    if (.not.(this%name==other%name)) return
                end if

                if (allocated(this%features).neqv.allocated(other%features)) return
                if (allocated(this%features)) then
                    if (.not.(size(this%features)==size(other%features))) return
                    do ii = 1, size(this%features)
                        if (.not.(this%features(ii)==other%features(ii))) return
                    end do
                end if

            class default
                return
        end select

        profile_is_same = .true.

    end function profile_is_same

    !> Dump profile to toml table
    subroutine dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(profile_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call set_string(table, "name", self%name, error, class_name)
        if (allocated(error)) return

        call set_list(table, "features", self%features, error)
        if (allocated(error)) return

    end subroutine dump_to_toml

    !> Read profile from toml table (no checks made at this stage)
    subroutine load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(profile_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call get_value(table, "name", self%name)

        call get_list(table, "features", self%features, error)
        if (allocated(error)) return

    end subroutine load_from_toml

end module fpm_manifest_profile
