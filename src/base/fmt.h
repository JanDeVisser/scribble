/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>

#include <sv.h>

#ifndef __FMT_H__
#define __FMT_H__

typedef struct fmt_arg {
    union {
        Integer     integer;
        double      flt;
        StringView  sv;
        char const *cstr;
        char        padding[16];
    };
} FMTArg;

extern StringView fmt_format(StringView fmt, size_t num, FMTArg args[]);

#endif /* __FMT_H__ */
