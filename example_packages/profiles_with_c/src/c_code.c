#include <sys/stat.h>
/*
 *  Decides whether a given file name is a directory.
 *  return 1 if file exists and is a directory
 *  Source (Public domain): https://github.com/urbanjost/M_system
 */
int my_isdir (const char *path) {
   struct stat sb;
   return stat(path, &sb) == 0 && S_ISDIR (sb.st_mode);
}