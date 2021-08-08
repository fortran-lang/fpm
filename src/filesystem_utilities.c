#include <sys/stat.h>
#include <dirent.h>

#ifdef __APPLE__
DIR * opendir$INODE64( const char * dirName );
struct dirent * readdir$INODE64( DIR * dir );
#endif

int c_is_dir(const char *path)
{
    struct stat m;
    int r = stat(path, &m);
    return r == 0 && S_ISDIR(m.st_mode);
}

const char *get_d_name(struct dirent *d)
{
    return (const char *) d->d_name;
}



DIR *c_opendir(const char *dirname){

#ifdef __APPLE__
    return opendir$INODE64(dirname);
#else
    return opendir(dirname);
#endif

}

struct dirent *c_readdir(DIR *dirp){

#ifdef __APPLE__
    return readdir$INODE64(dirp);
#else
    return readdir(dirp);
#endif

}