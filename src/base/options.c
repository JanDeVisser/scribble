/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "sv.h"
#define STATIC_ALLOCATOR
#include <allocate.h>
#include <options.h>

static OptionList *s_option_list_head;

void set_option(StringView option, StringView value)
{
    OptionList *entry = allocate_new(OptionList);
    entry->option = sv_copy_with_allocator(option, get_allocator());
    entry->value = sv_copy_with_allocator(value, get_allocator());
    entry->next = s_option_list_head;
    s_option_list_head = entry;
}

StringView get_option(StringView option)
{
    for (OptionList *entry = s_option_list_head; entry; entry = entry->next) {
        if (sv_eq(entry->option, option)) {
            return entry->value;
        }
    }
    return sv_null();
}

StringList get_option_values(StringView option)
{
    StringList ret = sl_create();
    for (OptionList *entry = s_option_list_head; entry; entry = entry->next) {
        if (sv_eq(entry->option, option)) {
            sl_push(&ret, entry->value);
        }
    }
    return ret;
}
