/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <config.h>

#ifndef __LOG_H__
#define __LOG_H__

#define TRACECATEGORIES(S) \
    S(NONE)                \
    S(LIB)                 \
    S(MEM)                 \
    S(SV)                  \
    S(PARSE)               \
    S(BIND)                \
    S(IR)                  \
    S(EXECUTE)             \
    S(COMPILE)             \
    S(IPC)

typedef enum trace_category {
#undef TRACECATEGORY
#define TRACECATEGORY(cat) CAT_##cat,
    TRACECATEGORIES(TRACECATEGORY)
#undef TRACECATEGORY
        CAT_COUNT
} TraceCategory;

// clang-format off
extern void log_init();
extern format_args(2, 3)          void trace(TraceCategory category, char const *msg, ...);
extern                            void vtrace(char const *msg, va_list args);
noreturn extern format_args(1, 2) void _fatal(char const *msg, ...);
noreturn extern                   void vfatal(char const *msg, va_list args);

#define fatal(msg, ...)           _fatal("%s:%d: " msg, __FILE__, __LINE__ __VA_OPT__(, ) __VA_ARGS__)
#define UNREACHABLE()             fatal("Unreachable")
#define NYI(msg, ...)             fatal("Not yet implemented in %s: " msg, __func__ __VA_OPT__(, ) __VA_ARGS__)
#define OUT_OF_MEMORY(msg, ...)   fatal("Out of memory in %s: " msg, __func__ __VA_OPT(, ) __VA_ARGS__)
// clang-format on

#define assert(cond)                                                    \
    if (!(cond)) {                                                      \
        fatal("%s:%d: assert('%s') FAILED", __FILE__, __LINE__, #cond); \
    }
#define assert_msg(cond, msg, ...)                                                                \
    if (!(cond)) {                                                                                \
        fatal("%s:%d: assert('%s'): " msg, __FILE__, __LINE__, #cond __VA_OPT__(, ) __VA_ARGS__); \
    }

#endif // __LOG_H__
