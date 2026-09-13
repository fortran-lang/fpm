#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

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

int c_mkdir_p(const char *path)
{
#ifdef _WIN32
    return -1;
#else
    char *tmp;
    char *p;
    size_t len;
    int result;
    struct stat m;

    if (path == NULL || path[0] == '\0') {
        return -1;
    }

    len = strlen(path);
    tmp = malloc(len + 1);
    if (tmp == NULL) {
        return -1;
    }
    memcpy(tmp, path, len + 1);

    for (p = tmp + 1; *p != '\0'; ++p) {
        if (*p != '/') {
            continue;
        }
        *p = '\0';
        if (mkdir(tmp, 0777) != 0 && errno != EEXIST) {
            free(tmp);
            return -1;
        }
        *p = '/';
    }

    result = mkdir(tmp, 0777);
    free(tmp);
    if (result != 0 && errno != EEXIST) {
        return -1;
    }

    return stat(path, &m) == 0 && S_ISDIR(m.st_mode) ? 0 : -1;
#endif
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
