/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

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

OptionList *get_option_values(StringView option)
{
    OptionList *ret = NULL;
    for (OptionList *entry = s_option_list_head; entry; entry = entry->next) {
        if (sv_eq(entry->option, option)) {
            OptionList *return_entry = allocate_new(OptionList);
            return_entry->option = option;
            return_entry->value = entry->value;
            return_entry->next = ret;
            ret = return_entry;
        }
    }
    return ret;
}
