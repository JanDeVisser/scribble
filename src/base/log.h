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

#ifdef HAVE_LEGACY_ATTRIBUTE_NORETURN
#define noreturn __attribute__((noreturn))
#elif defined(HAVE_C23_ATTRIBUTE_NORETURN)
#define noreturn [[noreturn]]
#else
#define noreturn
#endif

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

extern void          log_init();
extern void          trace(TraceCategory category, char const *msg, ...);
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
