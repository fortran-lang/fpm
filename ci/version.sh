#!/usr/bin/env bash
set -ex

# Helper function that wraps a string into a fortran character(*), parameter definition
fortran_character_parameter()
{
    line="character(len=*), parameter :: $1 = \"$2\""
    echo $line    
}

# define include file for version caching
INCLUDE_FILE="$(dirname $0)/../include/fpm_version_parameters.f90"

# Get latest release version. Exclude trunk, which is named `current` on the fpm repo
latest_release=$(git describe --tags --exclude current)
if [ $? -ne 0 ]; then
  echo "Could not query the current release from git. Check that git is installed on this system."
  exit 1
fi

# Extract numbered version 
no_v=${latest_release#*v}    # Remove heading v
no_commit=${no_v%-*}         # Remove commit #
version=${no_commit%-*}      # Remove increment

echo $no_v
echo $no_commit
echo $version

# Write to a fortran include file
echo $(fortran_character_parameter fpm_version_ID $version ) > $INCLUDE_FILE
echo $(fortran_character_parameter fpm_version_long $latest_release ) >> $INCLUDE_FILE

