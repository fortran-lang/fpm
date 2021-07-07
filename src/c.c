/* FIXME: fpm --flag '-DENABLE_C_WRAPPER' currently doesn't work with .c files. Use #if..#endif below for the time being. */
#if ((defined(_WIN32) && (defined(__MINGW32__) || defined(__MINGW64__))) || defined(__linux__) || defined(__APPLE__) || defined(__OpenBSD__))
#define ENABLE_C_WRAPPER
#endif

#ifdef ENABLE_C_WRAPPER
#include <sys/stat.h>
#include <dirent.h>

int is_dir(const char *path)
{
    struct stat m;
    int r = stat(path, &m);
    return r == 0 && S_ISDIR(m.st_mode);
}

const char *get_d_name(struct dirent *d)
{
    return (const char *) d->d_name;
}

#endif
