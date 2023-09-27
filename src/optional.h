/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __OPTIONAL_H__
#define __OPTIONAL_H__

#define OPTIONAL(T)               \
    typedef struct optional_##T { \
        bool has_value;           \
        T    value;               \
    } Optional##T;

#define MUST_OPTIONAL(T, var, expr)                                 \
    Optional##T var##_maybe = (expr);                               \
    if (!var##_maybe.has_value) {                                   \
        fatal("Optional expression '%s' (%s:%d) returned no value", \
            #expr, __FILE_NAME__, __LINE__);                        \
    }                                                               \
    T var = var##_maybe.value;

#endif /* __OPTIONAL_H__ */
