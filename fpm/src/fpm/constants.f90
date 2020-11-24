module fpm_constants
  implicit none
  public

  !> Name of the project's manifest file
  character(len=*), parameter :: fpm_manifest_file = "fpm.toml"

  !> Name of the build directory
  character(len=*), parameter :: fpm_build_dir = "build"

  !> Name of the dependency subdirectory
  character(len=*), parameter :: fpm_dependency_dir = "dependencies"

  !> Name of the dependency lock file
  character(len=*), parameter :: fpm_lock_file = "cache.toml"

end module fpm_constants
