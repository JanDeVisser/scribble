/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef __LOG_H__
#define __LOG_H__

#ifdef __clang__
#define noreturn [[noreturn]]
#else
#define noreturn __attribute__((noreturn))
#endif

extern void          log_init(bool trace_on);
extern void          trace(char const *msg, ...);
extern void          vtrace(char const *msg, va_list args);
noreturn extern void fatal(char const *msg, ...);
noreturn extern void vfatal(char const *msg, va_list args);

#define UNREACHABLE() fatal("%s:%d: Unreachable", __FILE__, __LINE__)
#define NYI(what, ...) fatal("%s:%d: Not yet implemented in %s: " what, __FILE__, __LINE__, __func__ __VA_OPT__(, ) __VA_ARGS__)
#define OUT_OF_MEMORY(where) fatal("Out of memory at %s:%d: %s", __FILE__, __LINE__, where)

#define assert(cond) \
    if (!(cond))     \
    fatal(__FILE__ ":%d: assert(" #cond ") FAILED", __LINE__, #cond)
#define assert_msg(cond, msg, ...) \
    if (!(cond))                   \
    fatal(__FILE__ ":%d: assert(" #cond ") " msg, __LINE__ __VA_OPT__(, ) __VA_ARGS__)

#endif // __LOG_H__
