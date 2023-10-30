/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <sv.h>

#include <allocate.h>

DECLARE_SHARED_ALLOCATOR(sv)

DA_IMPL_ELEMENTS(StringView, strings)

StringList sl_create()
{
    StringList ret = { 0 };
    return ret;
}

StringList sl_copy(StringList *sl)
{
    assert(sl);
    StringList ret = sl_create();
    for (size_t ix = 0; ix < sl->size; ++ix) {
        sl_push(&ret, sl->strings[ix]);
    }
    return ret;
}

StringList *sl_push(StringList *sl, StringView sv)
{
    da_append_StringView(sl, sv);
    return sl;
}

StringList *sl_extend(StringList *sl, StringList *with)
{
    assert(sl);
    assert(with);
    for (size_t ix = 0; ix < with->size; ++ix) {
        da_append_StringView(sl, with->strings[ix]);
    }
    return sl;
}

StringView sl_pop(StringList *sl)
{
    if (sl_empty(sl)) {
        return sv_null();
    }
    return sl->strings[sl->size--];
}

StringView sl_join(StringList *sl, StringView sep)
{
    StringBuilder sb = sb_create();
    for (size_t ix = 0; ix < sl->size; ++ix) {
        if (ix > 0) {
            sb_append_sv(&sb, sep);
        }
        sb_append_sv(&sb, sl->strings[ix]);
    }
    return sb.view;
}

StringView sl_front(StringList *sl)
{
    if (sl_empty(sl)) {
        return sv_null();
    }
    return sl->strings[0];
}

StringView sl_back(StringList *sl)
{
    if (sl_empty(sl)) {
        return sv_null();
    }
    return sl->strings[sl->size - 1];
}

bool sl_empty(StringList *sl)
{
    return !sl || (sl->size == 0);
}

size_t sl_size(StringList *sl)
{
    return (sl) ? sl->size : 0;
}
