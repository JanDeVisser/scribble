/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>
#include <string.h>
#include <sys/fcntl.h>
#include <sys/wait.h>
#include <unistd.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <process.h>

Process * process_create_sl(StringView cmd, StringList *args)
{
    Process *p = allocate_new(Process);
    p->command = cmd;
    p->arguments = sl_copy(args);
    p->out = sb_acreate(get_allocator());
    p->err = sb_acreate(get_allocator());
    return p;
}

Process * process_vcreate(StringView cmd, va_list args)
{
    StringList sl_args = sl_acreate(get_allocator());
    for (char const *arg = va_arg(args, char const*); arg; arg = va_arg(args, char const*)) {
        sl_push(&sl_args, sv_copy_cstr(arg));
    }
    return process_create_sl(cmd, &sl_args);
}

Process * _process_create(StringView cmd, ...)
{
    va_list args;
    va_start(args, cmd);
    Process *ret = process_vcreate(cmd, args);
    va_end(args);
    return ret;
}

ErrorOrInt process_execute(Process *p)
{
    size_t sz = p->arguments.size;
    char** argv = allocate_array(char*, sz + 2);
    argv[0] = (char *) sv_cstr(p->command);
    for (size_t ix = 0u; ix < sz; ++ix) {
        argv[ix + 1] = (char *) sv_cstr(p->arguments.strings[ix]);
    }
    argv[sz + 1] = NULL;
    printf("[CMD] %.*s %.*s\n", SV_ARG(p->command), SV_ARG(sl_join(&p->arguments, sv_from(" "))));

    int filedes[2];
    if (pipe(filedes) == -1) {
        ERROR(Int, ProcessError, errno, "pipe() failed");
    }

    pid_t pid = fork();
    if (pid == -1) {
        ERROR(Int, ProcessError, errno, "fork() failed");
    }
    if (pid == 0) {
        while ((dup2(filedes[1], STDOUT_FILENO) == -1) && (errno == EINTR)) { }
        close(filedes[0]);
        close(filedes[1]);
        execvp(sv_cstr(p->command), argv);
        ERROR(Int, ProcessError, errno, "execvp() failed");
    }
    close(filedes[1]);

    char buffer[4096];
    while (true) {
        ssize_t count = read(filedes[0], buffer, sizeof(buffer) - 1);
        if (count == -1) {
            if (errno == EINTR) {
                continue;
            } else {
                ERROR(Int, ProcessError, errno, "Error reading child process output");
            }
        }
        if (count == 0)
            break;
        buffer[count] = '\0';
        sb_append_chars(&p->out, buffer, count);
    }
    close(filedes[0]);

    int exit_code;
    if (waitpid(pid, &exit_code, 0) == -1) {
        ERROR(Int, ProcessError, errno, "waitpid() failed");
    }
    if (!WIFEXITED(exit_code)) {
        ERROR(Int, ProcessError, errno, "Child program %.*s crashed due to signal %d", SV_ARG(p->command), WTERMSIG(exit_code));
    }
    RETURN(Int, WEXITSTATUS(exit_code));
}

ErrorOrInt execute_sl(StringView cmd, StringList *args)
{
    Process *p = process_create_sl(cmd, args);
    return process_execute(p);
}

ErrorOrInt _execute(StringView cmd, ...)
{
    va_list args;
    Process *p;

    va_start(args, cmd);
    p = process_vcreate(cmd, args);
    va_end(args);
    return process_execute(p);
}
