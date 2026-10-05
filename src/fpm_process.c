#ifdef _WIN32
#include <windows.h>
#else
#include <errno.h>
#include <signal.h>
#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#ifdef __APPLE__
#include <crt_externs.h>
#define FPM_ENVIRON (*_NSGetEnviron())
#else
extern char **environ;
#define FPM_ENVIRON environ
#endif
#endif

/// @brief Run a command through `/bin/sh -c` and wait for it to finish.
///
/// Behaves like system(3), which is what execute_command_line calls, but
/// without the process-wide lock Apple's libc holds for the whole of a
/// system() call: that lock lets only one command run at a time, so on
/// macOS every target of a parallel build waits for the previous one.
/// posix_spawn takes no such lock, and is safe to call from several
/// threads at once on both macOS and glibc.
///
/// @param cmd      Null-terminated command line, handed to the shell as is.
/// @param exitstat Exit status when the shell exited normally, as
///                 execute_command_line reports it; the raw wait status
///                 when it was killed by a signal.
/// @return 0 when the shell ran, 1 when it could not be started (errno is
///         set), 2 on Windows, where the caller uses execute_command_line.
int c_run_command(const char *cmd, int *exitstat)
{
#ifdef _WIN32
    (void)cmd;
    (void)exitstat;
    return 2;
#else
    posix_spawnattr_t attr;
    sigset_t none, dflt;
    pid_t pid;
    int status, err;
    char *argv[] = {"sh", "-c", (char *)cmd, NULL};

    /* Give the child what system() gives it: no blocked signals, and the
       default action for SIGINT and SIGQUIT whatever the caller set. */
    sigemptyset(&none);
    sigemptyset(&dflt);
    sigaddset(&dflt, SIGINT);
    sigaddset(&dflt, SIGQUIT);
    posix_spawnattr_init(&attr);
    posix_spawnattr_setsigmask(&attr, &none);
    posix_spawnattr_setsigdefault(&attr, &dflt);
    posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSIGDEF);

    err = posix_spawn(&pid, "/bin/sh", NULL, &attr, argv, FPM_ENVIRON);
    posix_spawnattr_destroy(&attr);
    if (err != 0) {
        errno = err;
        return 1;
    }

    while (waitpid(pid, &status, 0) == -1) {
        if (errno != EINTR) return 1;
    }

    *exitstat = WIFEXITED(status) ? WEXITSTATUS(status) : status;
    return 0;
#endif
}

/// @brief Pause the calling thread for `ms` milliseconds.
///
/// Used by the build scheduler when a worker finds no target ready to build,
/// so that it waits for a running target to finish without spinning.
///
/// @param ms Milliseconds to pause.
void c_sleep_ms(int ms)
{
#ifdef _WIN32
    Sleep((DWORD)ms);
#else
    struct timespec ts;

    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (long)(ms % 1000) * 1000000L;
    while (nanosleep(&ts, &ts) == -1 && errno == EINTR) {
    }
#endif
}
