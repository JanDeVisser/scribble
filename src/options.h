/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <sv.h>

#ifndef __OPTIONS_H__
#define __OPTIONS_H__

typedef struct _option_list {
    StringView           option;
    StringView           value;
    struct _option_list *next;
} OptionList;

extern void        set_option(StringView option, StringView value);
extern StringView  get_option(StringView option);
extern OptionList *get_option_values(StringView option);

#define OPT_DEBUG (!sv_empty(get_option(sv_from("debug"))))
#define OPT_TRACE (!sv_empty(get_option(sv_from("trace"))))
#define OPT_GRAPH (!sv_empty(get_option(sv_from("graph"))))
#define OPT_RUN (!sv_empty(get_option(sv_from("run"))))
#define OPT_STATIC (!sv_empty(get_option(sv_from("static"))))

#endif /* __OPTIONS_H__ */
