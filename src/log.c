/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdarg.h>

#include <log.h>

typedef enum log_level {
    LL_TRACE,
    LL_PANIC,
} LogLevel;

static void vemit_log_message(LogLevel level, char const *msg, va_list args);
static void emit_log_message(LogLevel level, char const *msg, ...);

static LogLevel log_level;

void emit_log_message(LogLevel level, char const *msg, ...)
{
    va_list args;
    va_start(args, msg);
    vemit_log_message(level, msg, args);
}

void vemit_log_message(LogLevel level, char const *msg, va_list args)
{
    if (level >= log_level) {
        vfprintf(stderr, msg, args);
        fprintf(stderr, "\n");
    }
}

void trace(char const *msg, ...)
{
    va_list args;
    va_start(args, msg);
    vemit_log_message(LL_TRACE, msg, args);
}

void vtrace(char const *msg, va_list args)
{
    vemit_log_message(LL_TRACE, msg, args);
}

void fatal(char const *msg, ...)
{
    va_list args;
    va_start(args, msg);
    vfatal(msg, args);
}

void vfatal(char const *msg, va_list args)
{
    vemit_log_message(LL_PANIC, msg, args);
    emit_log_message(LL_PANIC, "Aborting...");
    exit(1);
}

void log_init(bool trace_on)
{
    log_level = (trace_on) ? LL_TRACE : LL_PANIC;
}
