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
