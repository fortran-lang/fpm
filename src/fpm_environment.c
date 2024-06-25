#include <stdlib.h>
#include <stdio.h>

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
   char* str = malloc(64*sizeof(char));
   *str = '\0';
   int errcode = _putenv_s(envname,str);
   // Windows returns a non-0 code when setting empty variable
   if (errcode==-1) errcode=0;
   free(str);
   return errcode;
#endif
}
