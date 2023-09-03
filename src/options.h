/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <sv.h>

#ifndef __OPTIONS_H__
#define __OPTIONS_H__

extern void set_option(StringView option, StringView value);
extern StringView get_option(StringView option);

#define OPT_DEBUG (!sv_empty(get_option(sv_from("debug"))))
#define OPT_TRACE (!sv_empty(get_option(sv_from("trace"))))
#define OPT_GRAPH (!sv_empty(get_option(sv_from("graph"))))

#endif /* __OPTIONS_H__ */
