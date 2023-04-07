program main
  use tomlf_version, only: tomlf_version_string
  implicit none

  print *, 'using version =',tomlf_version_string
  print *, 'should be     =0.3.1'

  if (tomlf_version_string=="0.3.1") then 
     stop 0
  else
     stop 1
  endif

end program main
