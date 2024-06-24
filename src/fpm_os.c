#include <stdlib.h>

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

/// @brief Set environment variable using the C standard library
/// @param envname: points to a string containing the name of an environment variable to be added or altered.
/// @param envval: points to the value the environment variable is set to
/// @param overwrite: flag to determine whether an old value should be overwritten
/// @return success flag, 0 on successful execution
int c_setenv(const char *envname, const char *envval, int overwrite) {
#ifndef _WIN32    
   return setenv(envname, envval, overwrite);
#else
   int errcode = 0;
   if(!overwrite) {
       size_t envsize = 0;
       errcode = getenv_s(&envsize, NULL, 0, envname);
       if (errcode || envsize) return errcode;
   }
   return _putenv_s(envname, envval);    
#endif
} 

/// @brief Delete environment variable using the C standard library
/// @param envname: points to a string containing the name of an environment variable.
/// @return success flag, 0 on successful execution
int c_unsetenv(const char *envname) {
#ifndef _WIN32    
   return unsetenv(envname);
#else
   return _putenv_s(envname,NULL);    
#endif   
} 


