#ifndef _WIN32
#define _POSIX_C_SOURCE 200809L
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#ifdef _WIN32
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <direct.h>
#define close _close
#define getcwd _getcwd
#else
#include <unistd.h>
#include <time.h>
#endif

static void sleep_ms(int ms) {
#ifdef _WIN32
    Sleep((DWORD)ms);
#else
    struct timespec duration = {ms / 1000, (ms % 1000) * 1000000L};
    nanosleep(&duration, NULL);
#endif
}

int main(int argc, char **argv) {
#ifdef _WIN32
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
    _setmode(_fileno(stderr), _O_BINARY);
#endif
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
    if (argc < 2) {
        return 2;
    }
    const char *mode = argv[1];
    if (strcmp(mode, "args") == 0) {
        for (int i = 2; i < argc; i++) {
            printf("%zu:%s\n", strlen(argv[i]), argv[i]);
        }
        return 0;
    }
    if (strcmp(mode, "exit") == 0) {
        return atoi(argv[2]);
    }
    if (strcmp(mode, "dual") == 0) {
        int remaining = atoi(argv[2]);
        char out[4096];
        char err[4096];
        memset(out, 'o', sizeof(out));
        memset(err, 'e', sizeof(err));
        while (remaining > 0) {
            size_t count = remaining < 4096 ? (size_t)remaining : 4096;
            const int has_write_failed = fwrite(out, 1, count, stdout) != count || fwrite(err, 1, count, stderr) != count;
            if (has_write_failed) {
                return 3;
            }
            remaining -= (int)count;
        }
        return 0;
    }
    if (strcmp(mode, "invalid") == 0) {
        unsigned char bytes[] = {0xff, 0x00, 0xfe};
        fwrite(bytes, 1, sizeof(bytes), stdout);
        fputs("ok", stderr);
        return 0;
    }
    if (strcmp(mode, "copy") == 0) {
        char bytes[4096];
        size_t count;
        while ((count = fread(bytes, 1, sizeof(bytes), stdin)) > 0) {
            fwrite(bytes, 1, count, stdout);
        }
        fputs("eof", stderr);
        return 0;
    }
    if (strcmp(mode, "env") == 0) {
        const char *value = getenv(argv[2]);
        if (value == NULL) {
            return 3;
        }
        fputs(value, stdout);
        return 0;
    }
    if (strcmp(mode, "cwd") == 0) {
        char cwd[32768];
        if (getcwd(cwd, sizeof(cwd)) == NULL) {
            return 3;
        }
        fputs(cwd, stdout);
        return 0;
    }
    if (strcmp(mode, "closed_sleep") == 0) {
        close(1);
        close(2);
        sleep_ms(5000);
        return 0;
    }
    const int should_wait = strcmp(mode, "ready_sleep") == 0 || strcmp(mode, "ignore_term") == 0 || strcmp(mode, "pid_ready") == 0;
    if (should_wait) {
        const int should_ignore_term = strcmp(mode, "ignore_term") == 0 || strcmp(mode, "pid_ready") == 0;
        if (should_ignore_term) {
            signal(SIGTERM, SIG_IGN);
        }
        if (strcmp(mode, "pid_ready") == 0) {
#ifdef _WIN32
            printf("%lu\n", (unsigned long)GetCurrentProcessId());
#else
            printf("%ld\n", (long)getpid());
#endif
        } else {
            puts("ready");
        }
        sleep_ms(5000);
        return 0;
    }
    if (strcmp(mode, "finish_file") == 0) {
        sleep_ms(40);
        FILE *file = fopen(argv[2], "wb");
        if (file == NULL) {
            return 3;
        }
        fputs("finished", file);
        fclose(file);
        return 0;
    }
    if (strcmp(mode, "wait_file") == 0) {
        for (int i = 0; i < 500; i++) {
            FILE *file = fopen(argv[2], "rb");
            if (file != NULL) {
                fclose(file);
                fputs("done", stdout);
                return 0;
            }
            sleep_ms(10);
        }
        return 4;
    }
    if (strcmp(mode, "signal") == 0) {
        raise(SIGTERM);
        return 3;
    }
#ifndef _WIN32
    if (strcmp(mode, "descendant") == 0) {
        pid_t pid = fork();
        if (pid == 0) {
            sleep_ms(300);
            _exit(0);
        }
        return pid < 0 ? 3 : 0;
    }
#endif
    return 2;
}
