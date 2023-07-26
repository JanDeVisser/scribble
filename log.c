/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdarg.h>

#include <log.h>

void fatal(char const* msg, ...)
{
    va_list args;
    va_start(args, msg);
    vfatal(msg, args);
}


void vfatal(char const* msg, va_list args)
{
    vfprintf(stderr, msg, args);
    exit(1);
}
