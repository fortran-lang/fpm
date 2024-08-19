program metapackage_hdf5
  use hdf5
  implicit none

  integer :: error

  call h5open_f(error)
  if (error/=0) stop -1

  call h5close_f(error)
  if (error/=0) stop -2

  stop 0

end program metapackage_hdf5
