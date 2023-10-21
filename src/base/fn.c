/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <fn.h>

DECLARE_SHARED_ALLOCATOR(sv)

StringView fn_barename(StringView path)
{
    StringView basename = fn_basename(path);
    int        dot = sv_last(basename, '.');
    if (dot < 0) {
        return basename;
    }
    return (StringView) {
        .ptr = basename.ptr,
        .length = dot,
    };
}

StringView fn_basename(StringView path)
{
    int slash = sv_last(path, '/');
    if (slash < 0) {
        return path;
    }
    return (StringView) {
        .ptr = path.ptr + slash + 1,
        .length = path.length - slash - 1
    };
}

StringView fn_dirname(StringView path)
{
    int slash = sv_last(path, '/');
    if (slash < 0) {
        return sv_null();
    }
    return (StringView) {
        .ptr = path.ptr,
        .length = slash
    };
}

StringView fn_extension(StringView path)
{
    StringView basename = fn_basename(path);
    int        dot = sv_last(basename, '.');
    if (dot < 0) {
        return sv_null();
    }
    return (StringView) {
        .ptr = basename.ptr,
        .length = dot,
    };
}

StringList fn_split_path(StringView path)
{
    StringList result = sl_create();
    StringView rest = path;
    while (sv_not_empty(rest)) {
        int slash = sv_first(rest, '/');
        if (slash < 0) {
            sl_push(&result, rest);
            break;
        }
        sl_push(&result, (StringView) { .ptr = rest.ptr, .length = slash });
        rest = (StringView) {
            .ptr = rest.ptr + slash + 1,
            .length = rest.length - slash - 1
        };
    }
    return result;
}
