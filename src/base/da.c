/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <string.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <da.h>

void  da_resize(DA_void *array, size_t elem_size, size_t cap)
{
    if (array->cap >= cap) {
        return;
    }
    if (array->cap == 0) {
        array->elements = array_allocate(elem_size, 16);
        array->cap = 16;
    } else {
        size_t new_cap = array->cap;
        do {
            new_cap *= 2;
        } while (new_cap < cap);
        void *new_elements = array_allocate(elem_size, new_cap);
        memcpy(new_elements, array->elements, array->cap * elem_size);
        array->elements = new_elements;
        array->cap = new_cap;
    }
}

size_t da_append(DA_void *array, void *elem, size_t elem_size)
{
    da_resize(array, elem_size, array->size + 1);
    memcpy(array->elements + elem_size * (array->size++), elem, elem_size);
    return array->size - 1;
}

void *da_element(DA_void *array, size_t ix, size_t elem_size)
{
    assert(ix < array->size);
    return array->elements + elem_size * ix;
}
