/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>

#include <da.h>
#include <sv.h>

#ifndef __FMT_H__
#define __FMT_H__

typedef struct fmt_arg {
    union {
        Integer     integer;
        double      flt;
        StringView  sv;
        void       *pointer;
        char const *cstr;
        char        padding[16];
    };
} FMTArg;

DA(FMTArg)
typedef DA_FMTArg FMTArgs;

extern StringView fmt_format(StringView fmt, FMTArgs);
extern StringView vformat(StringView fmt, va_list args);
extern StringView format(StringView fmt, ...);

#endif /* __FMT_H__ */
