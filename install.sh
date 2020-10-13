#!/bin/sh

set -u # error on use of undefined variable
set -e # exit on error

# Install Haskell Stack to /usr/local/bin/stack
if command -v stack &> /dev/null ; then
  echo "found stack"
else
  curl -sSL https://get.haskellstack.org/ | sh
fi


# Check for Stack in /usr/local/bin/stack

# On macOS, it might be necessary to run 'xcode-select --install' and/or
#      'open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg'
#      to set up the Xcode command-line tools, which Stack uses.

# Add '${USER}/.local/bin' the beginning of PATH in the fpm-setup.sh

#cd boostrap
#stack install

# Check for fpm in ${USER}/.local/bin
