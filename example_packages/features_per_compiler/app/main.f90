program main
  use iso_fortran_env, only: compiler_options
  implicit none

  character(len=:), allocatable :: options_str,detected_compiler
  logical :: debug_active, release_active, verbose_active, fast_active, strict_active
  logical :: all_checks_passed
  integer :: failed_checks

  ! Get compiler flags used to build this file
  allocate(options_str, source=compiler_options())

  ! Display compiler information
  print '(a)', '================================='
  print '(a)', 'Features Per Compiler Demo'
  print '(a)', '================================='
  print '(a)', ''

  ! Detect compiler type using the function
  allocate(detected_compiler, source=compiled_with())

  print '(2a)', 'Detected compiler: ', detected_compiler
  print '(a)', ''
  print '(2a)', 'Compiler options: ', trim(options_str)
  print '(a)', ''

  ! Check for feature flags
  debug_active   = index(options_str, ' -g ') > 0 
  release_active = index(options_str, '-O3') > 0
  verbose_active = index(options_str, ' -v') > 0 .or. index(options_str, ' -v ') > 0
  fast_active    = index(options_str, 'fast') > 0
  strict_active  = index(options_str, '-std=f2018') > 0 .or. index(options_str, '-stand f18') > 0

  ! Display active features
  print '(a)', 'Active features detected:'
  if (debug_active) print '(a)', '  ✓ DEBUG: debug flags found'
  if (release_active) print '(a)', '  ✓ RELEASE: -O flags found'
  if (verbose_active) print '(a)', '  ✓ VERBOSE: -v flag found'
  if (fast_active) print '(a)', '  ✓ FAST: fast optimization flags found'
  if (strict_active) print '(a)', '  ✓ STRICT: standard compliance flags found'

  print '(a)', ''

  ! Check compiler-specific flags and validate
  failed_checks = check_compiler_flags(detected_compiler, options_str, debug_active, release_active, fast_active, strict_active)

  print '(a)', ''

  ! Determine overall result
  all_checks_passed = (failed_checks == 0)

  if (all_checks_passed) then
    print '(a)', '✓ All compiler flag checks PASSED'
    print '(a)', ''
  else
    print '(a,i0,a)', '✗ ', failed_checks, ' compiler flag checks FAILED'
    print '(a)', ''
  end if

  ! Exit with appropriate code
  stop merge(0,1,all_checks_passed)

contains

  function check_compiler_flags(compiler, options, debug_on, release_on, fast_on, strict_on) result(failed_count)
    character(len=*), intent(in) :: compiler, options
    logical, intent(in) :: debug_on, release_on, fast_on, strict_on
    integer :: failed_count

    failed_count = 0
    select case (compiler)
      case ('gfortran')
        failed_count = check_gfortran_flags(options, debug_on, release_on, fast_on, strict_on)
      case ('ifort')
        failed_count = check_ifort_flags(options, debug_on, release_on, fast_on, strict_on)
      case ('ifx')
        failed_count = check_ifx_flags(options, debug_on, release_on, fast_on, strict_on)
      case default
        print '(a)', 'Compiler-specific checks: Unknown compiler - only base flags checked'
    end select
  end function

  function check_flag(options, flag_name, feature_name, description) result(found)
    character(len=*), intent(in) :: options, flag_name, feature_name, description
    logical :: found

    found = index(options, flag_name) > 0
    if (found) then
      print '(a,a,a,a,a)', '  ✓ ', feature_name, ': ', description, ' found'
    else
      print '(a,a,a,a,a)', '  ✗ ', feature_name, ': ', description, ' NOT found'
    end if
  end function

  function compiled_with() result(msg)
    use iso_fortran_env, only: compiler_version
    character(len=:), allocatable :: msg
    character(len=:), allocatable :: version_str

    allocate(version_str, source=compiler_version())

    if (index(version_str, 'GCC') > 0) then
      msg = 'gfortran'
    else if (index(version_str, 'Classic') > 0) then
      msg = 'ifort'
    else if (index(version_str, 'Intel') > 0) then
      msg = 'ifx'
    else
      msg = 'any'
    end if
  end function

  function check_gfortran_flags(options, debug_on, release_on, fast_on, strict_on) result(failed_count)
    character(len=*), intent(in) :: options
    logical, intent(in) :: debug_on, release_on, fast_on, strict_on
    integer :: failed_count

    failed_count = 0
    print '(a)', 'Compiler-specific flag checks (gfortran):'

    if (debug_on) then
      if (.not. check_flag(options, '-Wall', 'Debug', '-Wall')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-Wextra', 'Debug', '-Wextra')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-fcheck=bounds', 'Debug', '-fcheck=bounds')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-fbacktrace', 'Debug', '-fbacktrace')) failed_count = failed_count + 1
    end if

    if (release_on) then
      ! Check for either -march or -mcpu (Apple Silicon uses -mcpu; -match=native may be re-resolved by gcc)
      if (.not. (index(options, '-march') > 0 .or. index(options, '-mcpu') > 0)) then
        print '(a)', '  ✗ Release: neither -march=native nor -mcpu found'
        failed_count = failed_count + 1
      else
        if (index(options, '-march=native') > 0) then
          print '(a)', '  ✓ Release: -march found'
        else
          print '(a)', '  ✓ Release: -mcpu found'
        end if
      end if
      if (.not. check_flag(options, '-funroll-loops', 'Release', '-funroll-loops')) failed_count = failed_count + 1
    end if

    if (fast_on) then
      if (.not. check_flag(options, '-ffast', 'Fast', '-fast')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-ffast-math', 'Fast', '-ffast-math')) failed_count = failed_count + 1
    end if

    if (strict_on) then
      if (.not. check_flag(options, '-Wpedantic', 'Strict', '-Wpedantic')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-Werror', 'Strict', '-Werror')) failed_count = failed_count + 1
    end if
  end function

  function check_ifort_flags(options, debug_on, release_on, fast_on, strict_on) result(failed_count)
    character(len=*), intent(in) :: options
    logical, intent(in) :: debug_on, release_on, fast_on, strict_on
    integer :: failed_count

    failed_count = 0
    print '(a)', 'Compiler-specific flag checks (ifort):'

    if (debug_on) then
      if (.not. check_flag(options, '-warn all', 'Debug', '-warn all')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-check bounds', 'Debug', '-check bounds')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-traceback', 'Debug', '-traceback')) failed_count = failed_count + 1
    end if

    if (release_on) then
      if (.not. check_flag(options, '-unroll', 'Release', '-unroll')) failed_count = failed_count + 1
    end if

    if (fast_on) then
      if (.not. check_flag(options, '-fast', 'Fast', '-ffast')) failed_count = failed_count + 1
    end if

    if (strict_on) then
      if (.not. check_flag(options, '-stand f18', 'Strict', '-stand f18')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-warn errors', 'Strict', '-warn errors')) failed_count = failed_count + 1
    end if
  end function

  function check_ifx_flags(options, debug_on, release_on, fast_on, strict_on) result(failed_count)
    character(len=*), intent(in) :: options
    logical, intent(in) :: debug_on, release_on, fast_on, strict_on
    integer :: failed_count

    failed_count = 0
    print '(a)', 'Compiler-specific flag checks (ifx):'

    if (debug_on) then
      if (.not. check_flag(options, '-warn all', 'Debug', '-warn all')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-check bounds', 'Debug', '-check bounds')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-traceback', 'Debug', '-traceback')) failed_count = failed_count + 1
    end if

    if (release_on) then
      if (.not. check_flag(options, '-unroll', 'Release', '-unroll')) failed_count = failed_count + 1
    end if

    if (fast_on) then
      if (.not. check_flag(options, '-fast', 'Fast', '-ffast')) failed_count = failed_count + 1
    end if

    if (strict_on) then
      if (.not. check_flag(options, '-stand f18', 'Strict', '-stand f18')) failed_count = failed_count + 1
      if (.not. check_flag(options, '-warn errors', 'Strict', '-warn errors')) failed_count = failed_count + 1
    end if
  end function

end program main
