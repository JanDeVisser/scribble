/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __DA_H__
#define __DA_H__

#define DA_STRUCT(T, S)                                                    \
    void *array_allocate(size_t size_of_elem, size_t num_of_elems);        \
    typedef struct _da_##T {                                               \
        size_t num;                                                        \
        size_t cap;                                                        \
        S     *elements;                                                   \
    } DA_##T;                                                              \
    static inline void da_resize_##T(DA_##T *array, size_t cap)            \
    {                                                                      \
        if (array->cap >= cap) {                                           \
            return;                                                        \
        }                                                                  \
        if (array->cap == 0) {                                             \
            array->elements = array_allocate(sizeof(T), 16);               \
            array->cap = 16;                                               \
        } else {                                                           \
            size_t new_cap = array->cap * 2;                               \
            while (new_cap < cap) {                                        \
                new_cap *= 2;                                              \
            }                                                              \
            T *new_elements = array_allocate(sizeof(T), new_cap);          \
            memcpy(new_elements, array->elements, array->cap * sizeof(T)); \
            array->elements = new_elements;                                \
            array->cap = new_cap;                                          \
        }                                                                  \
    }                                                                      \
    static inline size_t da_append_##T(DA_##T *array, T elem)              \
    {                                                                      \
        da_resize_##T(array, array->num + 1);                              \
        array->elements[array->num++] = elem;                              \
        return array->num - 1;                                             \
    }

#define DA(T) DA_STRUCT(T, T)

#endif /* __DA_H__ */
