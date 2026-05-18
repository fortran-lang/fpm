#include <sys/stat.h>
#include <dirent.h>

#ifndef _WIN32
#include <errno.h>
#include <fcntl.h>
#include <spawn.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

extern char **environ;
#endif

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

int c_run_argv(const char *joined, int argc, const char *redirect)
{
#ifdef _WIN32
    (void)joined;
    (void)argc;
    (void)redirect;
    return -1;
#else
    char **argv = NULL;
    const char *p = joined;
    pid_t pid;
    int spawn_status;
    int wait_status;
    int i;
    int use_redirect = redirect != NULL && redirect[0] != '\0';
    posix_spawn_file_actions_t actions;
    posix_spawn_file_actions_t *actions_ptr = NULL;

    if (argc < 1) {
        return 0;
    }

    argv = calloc((size_t)argc + 1, sizeof(char *));
    if (argv == NULL) {
        return -1;
    }

    for (i = 0; i < argc; ++i) {
        argv[i] = (char *)p;
        p += strlen(p) + 1;
    }

    if (use_redirect) {
        if (posix_spawn_file_actions_init(&actions) != 0) {
            free(argv);
            return -1;
        }
        actions_ptr = &actions;
        if (posix_spawn_file_actions_addopen(&actions, STDOUT_FILENO, redirect,
                                             O_WRONLY | O_CREAT | O_TRUNC, 0666) != 0 ||
            posix_spawn_file_actions_adddup2(&actions, STDOUT_FILENO, STDERR_FILENO) != 0) {
            posix_spawn_file_actions_destroy(&actions);
            free(argv);
            return -1;
        }
    }

    spawn_status = posix_spawnp(&pid, argv[0], actions_ptr, NULL, argv, environ);
    if (use_redirect) {
        posix_spawn_file_actions_destroy(&actions);
    }
    free(argv);

    if (spawn_status != 0) {
        return spawn_status;
    }

    do {
        spawn_status = waitpid(pid, &wait_status, 0);
    } while (spawn_status < 0 && errno == EINTR);

    if (spawn_status < 0) {
        return -1;
    }

    if (WIFEXITED(wait_status)) {
        return WEXITSTATUS(wait_status);
    }
    if (WIFSIGNALED(wait_status)) {
        return 128 + WTERMSIG(wait_status);
    }
    return -1;
#endif
}
