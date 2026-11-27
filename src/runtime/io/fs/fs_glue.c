#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define DUSK_FS_OK 0
#define DUSK_FS_NOT_FOUND -1
#define DUSK_FS_PERMISSION_DENIED -2
#define DUSK_FS_NOT_EMPTY -3
#define DUSK_FS_NOT_DIRECTORY -4
#define DUSK_FS_OTHER -5

#ifndef _WIN32

#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#define DUSK_FS_MAX_OPEN_DIRS 64

typedef struct {
    DIR *dir;
    int fd;
    char *name;
} DirFrame;

typedef struct {
    DirFrame *items;
    size_t len;
    size_t cap;
    size_t open_dirs;
} DirStack;

static int map_errno(int code) {
    if (code == ENOENT) {
        return DUSK_FS_NOT_FOUND;
    }
    if (code == EACCES || code == EPERM) {
        return DUSK_FS_PERMISSION_DENIED;
    }
    if (code == ENOTEMPTY || code == EEXIST) {
        return DUSK_FS_NOT_EMPTY;
    }
    if (code == ENOTDIR) {
        return DUSK_FS_NOT_DIRECTORY;
    }
    return DUSK_FS_OTHER;
}

static char *dupe_name(const char *name) {
    size_t len = strlen(name) + 1;
    char *copy = (char *)malloc(len);
    if (copy == NULL) {
        return NULL;
    }
    memcpy(copy, name, len);
    return copy;
}

static void close_frame_dir(DirStack *stack, DirFrame *frame) {
    if (frame->dir == NULL) return;
    closedir(frame->dir);
    frame->dir = NULL;
    frame->fd = -1;
    stack->open_dirs -= 1;
}

static void free_stack(DirStack *stack) {
    for (size_t i = 0; i < stack->len; i++) {
        close_frame_dir(stack, &stack->items[i]);
        free(stack->items[i].name);
    }
    free(stack->items);
    stack->items = NULL;
    stack->len = 0;
    stack->cap = 0;
    stack->open_dirs = 0;
}

static int push_frame(DirStack *stack, DIR *dir, int fd, char *name) {
    if (stack->len == stack->cap) {
        size_t next_cap = stack->cap == 0 ? 16 : stack->cap * 2;
        DirFrame *next = (DirFrame *)realloc(stack->items, next_cap * sizeof(DirFrame));
        if (next == NULL) {
            return DUSK_FS_OTHER;
        }
        stack->items = next;
        stack->cap = next_cap;
    }
    stack->items[stack->len].dir = dir;
    stack->items[stack->len].fd = fd;
    stack->items[stack->len].name = name;
    stack->len += 1;
    stack->open_dirs += 1;
    return DUSK_FS_OK;
}

static void release_oldest_open_dir(DirStack *stack) {
    if (stack->open_dirs < DUSK_FS_MAX_OPEN_DIRS) return;
    for (size_t i = 1; i < stack->len; i++) {
        if (stack->items[i].dir != NULL) {
            close_frame_dir(stack, &stack->items[i]);
            return;
        }
    }
}

static int ensure_frame_open(DirStack *stack, size_t target) {
    if (stack->items[target].dir != NULL) {
        return DUSK_FS_OK;
    }
    size_t ancestor = target;
    while (ancestor > 0 && stack->items[ancestor].dir == NULL) {
        ancestor -= 1;
    }
    for (size_t i = ancestor + 1; i <= target; i++) {
        int parent_fd = stack->items[i - 1].fd;
        int child_fd = openat(parent_fd, stack->items[i].name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        if (child_fd < 0) {
            return map_errno(errno);
        }
        DIR *child_dir = fdopendir(child_fd);
        if (child_dir == NULL) {
            int saved = errno;
            close(child_fd);
            return map_errno(saved);
        }
        stack->items[i].dir = child_dir;
        stack->items[i].fd = dirfd(child_dir);
        stack->open_dirs += 1;
        if (stack->open_dirs > DUSK_FS_MAX_OPEN_DIRS && i > 1) {
            close_frame_dir(stack, &stack->items[i - 1]);
        }
    }
    return DUSK_FS_OK;
}

static int is_dot_entry(const char *name) {
    if (name[0] != '.') {
        return 0;
    }
    return name[1] == '\0' || (name[1] == '.' && name[2] == '\0');
}

int dusk_fs_delete_tree(const char *path) {
    int root_fd = open(path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (root_fd < 0) {
        int err = errno;
        if (err == ENOTDIR || err == ELOOP) {
            if (unlink(path) == 0) {
                return DUSK_FS_OK;
            }
            return map_errno(errno);
        }
        return map_errno(err);
    }

    DIR *root_dir = fdopendir(root_fd);
    if (root_dir == NULL) {
        int saved = errno;
        close(root_fd);
        return map_errno(saved);
    }

    DirStack stack = {0};
    if (push_frame(&stack, root_dir, dirfd(root_dir), NULL) != DUSK_FS_OK) {
        closedir(root_dir);
        return DUSK_FS_OTHER;
    }

    int result = DUSK_FS_OK;
    while (stack.len > 0) {
        size_t top_index = stack.len - 1;
        result = ensure_frame_open(&stack, top_index);
        if (result != DUSK_FS_OK) {
            break;
        }
        DirFrame *top = &stack.items[top_index];
        errno = 0;
        struct dirent *entry = readdir(top->dir);
        if (entry == NULL) {
            if (errno != 0) {
                result = map_errno(errno);
                break;
            }
            char *finished_name = top->name;
            close_frame_dir(&stack, top);
            stack.len -= 1;
            if (stack.len == 0) {
                free(finished_name);
                if (rmdir(path) != 0) {
                    result = map_errno(errno);
                }
                break;
            }
            size_t parent_index = stack.len - 1;
            result = ensure_frame_open(&stack, parent_index);
            if (result != DUSK_FS_OK) {
                free(finished_name);
                break;
            }
            if (unlinkat(stack.items[parent_index].fd, finished_name, AT_REMOVEDIR) != 0) {
                result = map_errno(errno);
                free(finished_name);
                break;
            }
            free(finished_name);
            continue;
        }

        if (is_dot_entry(entry->d_name)) continue;

        struct stat st;
        if (fstatat(top->fd, entry->d_name, &st, AT_SYMLINK_NOFOLLOW) != 0) {
            if (errno == ENOENT) continue;
            result = map_errno(errno);
            break;
        }

        if (!S_ISDIR(st.st_mode)) {
            if (unlinkat(top->fd, entry->d_name, 0) != 0 && errno != ENOENT) {
                result = map_errno(errno);
                break;
            }
            continue;
        }

        int child_fd = openat(top->fd, entry->d_name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        if (child_fd < 0) {
            int err = errno;
            if (err == ENOENT) continue;
            if (err == ELOOP || err == ENOTDIR) {
                if (unlinkat(top->fd, entry->d_name, 0) != 0 && errno != ENOENT) {
                    result = map_errno(errno);
                    break;
                }
                continue;
            }
            result = map_errno(err);
            break;
        }

        DIR *child_dir = fdopendir(child_fd);
        if (child_dir == NULL) {
            int saved = errno;
            close(child_fd);
            result = map_errno(saved);
            break;
        }

        char *name_copy = dupe_name(entry->d_name);
        if (name_copy == NULL) {
            closedir(child_dir);
            result = DUSK_FS_OTHER;
            break;
        }

        release_oldest_open_dir(&stack);
        result = push_frame(&stack, child_dir, dirfd(child_dir), name_copy);
        if (result != DUSK_FS_OK) {
            closedir(child_dir);
            free(name_copy);
            break;
        }
    }

    free_stack(&stack);
    return result;
}

#else

int dusk_fs_delete_tree(const char *path) {
    (void)path;
    return DUSK_FS_OTHER;
}

#endif
