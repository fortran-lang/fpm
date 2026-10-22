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

#ifdef _WIN32
#include <windows.h>
#include <stdlib.h>
#include <string.h>
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

#ifdef _WIN32
/* Quote one argv token using the CommandLineToArgvW rules so that
   CreateProcess reconstructs the exact token. Caller frees the result. */
static char *win_quote_arg(const char *arg)
{
    size_t len = strlen(arg);
    size_t i;
    int needs = (len == 0);
    char *out;
    char *q;

    for (i = 0; i < len; ++i) {
        char c = arg[i];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\v' || c == '"') {
            needs = 1;
            break;
        }
    }

    if (!needs) {
        out = (char *)malloc(len + 1);
        if (out) memcpy(out, arg, len + 1);
        return out;
    }

    out = (char *)malloc(2 * len + 3);
    if (!out) return NULL;
    q = out;
    *q++ = '"';
    for (i = 0; i < len;) {
        size_t nbs = 0;
        size_t k;
        while (i < len && arg[i] == '\\') { ++nbs; ++i; }
        if (i == len) {
            for (k = 0; k < 2 * nbs; ++k) *q++ = '\\';
            break;
        } else if (arg[i] == '"') {
            for (k = 0; k < 2 * nbs + 1; ++k) *q++ = '\\';
            *q++ = '"';
            ++i;
        } else {
            for (k = 0; k < nbs; ++k) *q++ = '\\';
            *q++ = arg[i++];
        }
    }
    *q++ = '"';
    *q = '\0';
    return out;
}
#endif

int c_run_argv(const char *joined, int argc, const char *redirect)
{
#ifdef _WIN32
    const char *p = joined;
    int i;
    int use_redirect = redirect != NULL && redirect[0] != '\0';
    const char **argv = NULL;
    char *cmd = NULL;
    size_t cap = 1, n = 0;
    char apppath[MAX_PATH];
    char *appname = NULL;
    DWORD found;
    HANDLE hout = INVALID_HANDLE_VALUE;
    SECURITY_ATTRIBUTES sa;
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    BOOL ok;
    int result;

    if (argc < 1) return 0;

    argv = (const char **)calloc((size_t)argc + 1, sizeof(char *));
    if (argv == NULL) return -1;
    for (i = 0; i < argc; ++i) {
        argv[i] = p;
        p += strlen(p) + 1;
    }

    /* Reassemble a properly quoted command line for CreateProcess. */
    cmd = (char *)malloc(cap);
    if (cmd == NULL) { free((void *)argv); return -1; }
    cmd[0] = '\0';
    for (i = 0; i < argc; ++i) {
        char *qa = win_quote_arg(argv[i]);
        size_t ql;
        size_t need;
        if (qa == NULL) { free(cmd); free((void *)argv); return -1; }
        ql = strlen(qa);
        need = n + ql + 2;
        if (need > cap) {
            char *t;
            cap = need * 2;
            t = (char *)realloc(cmd, cap);
            if (t == NULL) { free(qa); free(cmd); free((void *)argv); return -1; }
            cmd = t;
        }
        if (i > 0) cmd[n++] = ' ';
        memcpy(cmd + n, qa, ql);
        n += ql;
        cmd[n] = '\0';
        free(qa);
    }

    /* Resolve the program through PATH with a default .exe extension. */
    found = SearchPathA(NULL, argv[0], ".exe", MAX_PATH, apppath, NULL);
    if (found > 0 && found < MAX_PATH) appname = apppath;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    /* Only override the child's std handles when redirecting. Setting
       STARTF_USESTDHANDLES with the (often non-inheritable) console handles
       can strip the child's console, so leave them alone otherwise and let
       the child inherit the parent console normally. */
    if (use_redirect) {
        ZeroMemory(&sa, sizeof(sa));
        sa.nLength = sizeof(sa);
        sa.bInheritHandle = TRUE;
        hout = CreateFileA(redirect, GENERIC_WRITE, FILE_SHARE_READ, &sa,
                           CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (hout == INVALID_HANDLE_VALUE) { free(cmd); free((void *)argv); return -1; }
        si.dwFlags |= STARTF_USESTDHANDLES;
        si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
        si.hStdOutput = hout;
        si.hStdError = hout;
    }

    ok = CreateProcessA(appname, cmd, NULL, NULL, use_redirect, 0, NULL, NULL, &si, &pi);
    if (!ok) {
        result = -1;
    } else {
        DWORD code = 1;
        WaitForSingleObject(pi.hProcess, INFINITE);
        GetExitCodeProcess(pi.hProcess, &code);
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
        /* Keep a real (possibly high-bit) exit code positive so the caller
           does not mistake it for the stat < 0 spawn-failure sentinel. */
        result = (code > 0x7fffffff) ? 1 : (int)code;
    }

    if (hout != INVALID_HANDLE_VALUE) CloseHandle(hout);
    free(cmd);
    free((void *)argv);
    return result;
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
