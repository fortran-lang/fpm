#include <sys/stat.h>
#include <dirent.h>

#if defined(__APPLE__) && !defined(__aarch64__) && !defined(__ppc__) && !defined(__i386__)
DIR * opendir$INODE64( const char * dirName );
struct dirent * readdir$INODE64( DIR * dir );
#define opendir opendir$INODE64
#define readdir readdir$INODE64
#endif

int c_is_dir(const char *path)
{
    struct stat m;
    int r = stat(path, &m);
    return r == 0 && S_ISDIR(m.st_mode);
}

/// @brief Size and modification time of a file, following symbolic links.
///
/// @param path  Null-terminated path.
/// @param size  Receives the size in bytes.
/// @param mtime Receives the modification time, in seconds since the epoch.
/// @return 0 on success, nonzero when the file cannot be stat'ed.
int c_file_stamp(const char *path, long long *size, long long *mtime)
{
    struct stat m;
    if (stat(path, &m) != 0) return 1;
    *size = (long long) m.st_size;
    *mtime = (long long) m.st_mtime;
    return 0;
}

/// @brief Modification time of a file, following symbolic links, to the nanosecond where the
/// platform records it (whole seconds on Windows).
///
/// @param path Null-terminated path.
/// @param sec  Receives the seconds since the epoch.
/// @param nsec Receives the nanoseconds within that second.
/// @return 0 on success, nonzero when the file cannot be stat'ed.
int c_file_mtime(const char *path, long long *sec, long long *nsec)
{
    struct stat m;
    if (stat(path, &m) != 0) return 1;
#if defined(__APPLE__)
    *sec = (long long) m.st_mtimespec.tv_sec;
    *nsec = (long long) m.st_mtimespec.tv_nsec;
#elif defined(_WIN32)
    *sec = (long long) m.st_mtime;
    *nsec = 0;
#else
    *sec = (long long) m.st_mtim.tv_sec;
    *nsec = (long long) m.st_mtim.tv_nsec;
#endif
    return 0;
}

const char *get_d_name(struct dirent *d)
{
    return (const char *) d->d_name;
}

DIR *c_opendir(const char *dirname)
{
    return opendir(dirname);
}

struct dirent *c_readdir(DIR *dirp)
{
    return readdir(dirp);
}
