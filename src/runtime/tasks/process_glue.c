#ifndef _WIN32
#define _POSIX_C_SOURCE 200809L
#endif
#include <uv.h>
#include <signal.h>

int dusk_process_spawn(uv_loop_t *loop, uv_process_t *process, const char *file, char **args, char **env, const char *cwd, uv_stream_t **pipes, const int *modes, uv_exit_cb on_exit) {
    uv_stdio_container_t stdio[3] = {0};
    for (int i = 0; i < 3; i++) {
        if (modes[i] == 0) {
            stdio[i].flags = UV_INHERIT_FD;
            stdio[i].data.fd = i;
        } else if (modes[i] == 2) {
            stdio[i].flags = UV_CREATE_PIPE | (i == 0 ? UV_READABLE_PIPE : UV_WRITABLE_PIPE);
            stdio[i].data.stream = pipes[i];
        } else {
            stdio[i].flags = UV_IGNORE;
        }
    }
    uv_process_options_t options = {0};
    options.exit_cb = on_exit;
    options.file = file;
    options.args = args;
    options.env = env;
    options.cwd = cwd;
    options.flags = UV_PROCESS_WINDOWS_HIDE;
    options.stdio_count = 3;
    options.stdio = stdio;
    return uv_spawn(loop, process, &options);
}

int dusk_process_kill(uv_process_t *process) {
#ifdef SIGKILL
    return uv_process_kill(process, SIGKILL);
#else
    return uv_process_kill(process, 9);
#endif
}
