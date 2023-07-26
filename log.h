/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>
#include <stdlib.h>

#ifndef __LOG_H__
#define __LOG_H__

[[noreturn]] extern void fatal(char const* msg, ...);
[[noreturn]] extern void vfatal(char const* msg, va_list args);

#define UNREACHABLE() fatal("Unreachable")
#define NYI(what) fatal("Not yet implemented")
#define OUT_OF_MEMORY(where) fatal("Out of memory at %s:%d: %s", __FILE__, __LINE__, where)

#define assert(cond) if (!(cond)) fatal(#cond)

#endif // __LOG_H__
