#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

#ifndef _WIN32

#include <unistd.h>

#else

#include <io.h>
#include <sys\stat.h>

#define open _open

#endif

// @brief A thread-safe version of strerror using malloc.
// @param errnum
char *my_strerror(int errnum) {
    const int BUFSIZE = 256;
    char *buf = malloc(BUFSIZE);

// POSIX strerror_r and Windows strerror_s are both thread-safe versions of
// strerror with the same interface except for the order of the arguments.
#ifndef _WIN32
    int stat = strerror_r(errnum, buf, BUFSIZE);
#else
    int stat = strerror_s(buf, BUFSIZE, errnum);
#endif
    if (stat != 0) {
        const char *MSG = "Unknown error";
        memcpy(buf, MSG, strlen(MSG));
    }

    return buf;
}

/// @brief Create a file if it doesn't already exist in an atomic manner.
/// @param path
/// @param iostat Zero if file was successfully created, nonzero otherwise.
/// @param iomsg Points to an error message if an error occurred, NULL otherwise.
/// @param exsits Zero if the file didn't exist already, nonzero otherwise.
void c_create(char *path, int *iostat, char **iomsg, int *exists) {
    int fd = open(path,
                  O_RDONLY | O_CREAT | O_EXCL,
                  S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);

    if (fd == -1 && errno != EEXIST) {  // Some unexpected error occurred.
        *iostat = 1;
        *iomsg = my_strerror(errno);
        return;
    }

    if (fd == -1 && errno == EEXIST) {  // The lock-file already exists.
        *exists = 1;
    }

    if (fd != -1) {  // The lock-file was created.
        *exists = 0;

        int stat = close(fd);
        if (stat == -1) {
            *iostat = 1;
            *iomsg = my_strerror(errno);
            return;
        }
    }

    *iostat = 0;
    *iomsg = NULL;
}

// @brief Remove a file/directory in an atomic manner.
// @param path
// @param iostat
// @param iomsg
void c_remove(char *path, int *iostat, char **iomsg) {
    int stat = remove(path);
    if (stat == -1) {
        *iostat = 1;
        *iomsg = my_strerror(errno);
        return;
    }
    *iostat = 0;
    *iomsg = NULL;
}
