/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __OPTIONAL_H__
#define __OPTIONAL_H__

#define OPTIONAL(T)                                                 \
    typedef struct optional_##T {                                   \
        bool has_value;                                             \
        T    value;                                                 \
    } Optional##T;                                                  \
    static inline Optional##T Optional##T##_create(T value)         \
    {                                                               \
        return (Optional##T) { .has_value = true, .value = value }; \
    }                                                               \
    static inline Optional##T Optional##T##_empty()                 \
    {                                                               \
        return (Optional##T) { .has_value = false };                \
    }

#define MUST_OPTIONAL(T, var, expr)                                     \
    T var;                                                              \
    {                                                                   \
        Optional##T var##_maybe = (expr);                               \
        if (!var##_maybe.has_value) {                                   \
            fatal("Optional expression '%s' (%s:%d) returned no value", \
                #expr, __FILE_NAME__, __LINE__);                        \
        }                                                               \
        var = var##_maybe.value;                                        \
    }

#endif /* __OPTIONAL_H__ */
