/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <error_or.h>
#include <sv.h>

#ifndef __PROCESS_H__
#define __PROCESS_H__

typedef struct process {
    StringView    command;
    StringList    arguments;
    StringBuilder out;
    StringBuilder err;
} Process;

extern Process   *process_create_sl(StringView cmd, StringList *args);
extern Process   *process_vcreate(StringView cmd, va_list args);
extern Process   *_process_create(StringView cmd, ...);
extern ErrorOrInt process_execute(Process *p);
ErrorOrInt        execute_sl(StringView cmd, StringList *args);
ErrorOrInt        _execute(StringView cmd, ...);

#define process_create(cmd, ...) _process_create(cmd __VA_OPT__(, ) __VA_ARGS__, NULL)
#define execute(cmd, ...) _execute(cmd __VA_OPT__(, ) __VA_ARGS__, NULL)

#endif /* __PROCESS_H__ */
