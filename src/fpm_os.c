#include <stdlib.h>
#include <stdio.h>

/// @brief Determine the absolute, canonicalized path for a given path.
/// @param path
/// @param resolved_path
/// @param maxLength
/// @return
char* c_realpath(char* path, char* resolved_path, int maxLength) {
// Checking macro in C because it doesn't work with gfortran on Windows, even
// when exported manually.
#ifndef _WIN32
  return realpath(path, resolved_path);
#else
  return _fullpath(resolved_path, path, maxLength);
#endif
}



