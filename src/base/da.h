/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __DA_H__
#define __DA_H__

#define DIA_ELEMENTS(T, E) \
    size_t size;           \
    size_t cap;            \
    T     *E

#define DIA(T) DIA_ELEMENTS(T, elements)

#define DIA_APPEND_ELEMENT(T, E, obj, elem)                             \
    do {                                                                \
        if ((obj)->size == obj->cap) {                                  \
            if ((obj)->cap == 0) {                                      \
                (obj)->E = array_allocate(sizeof(T), 4);                \
                (obj)->cap = 4;                                         \
            } else {                                                    \
                size_t new_cap = (obj)->cap;                            \
                do {                                                    \
                    new_cap *= 2;                                       \
                } while (new_cap < (obj)->size);                        \
                T *new_elements = array_allocate(sizeof(T), new_cap);   \
                memcpy(new_elements, (obj)->E, (obj)->cap * sizeof(T)); \
                (obj)->E = new_elements;                                \
                (obj)->cap = new_cap;                                   \
            }                                                           \
        }                                                               \
        (obj)->E[(obj)->size++] = (elem);                               \
    } while (0)

#define DIA_APPEND(T, obj, elem) DIA_APPEND_ELEMENT(T, elements, obj, elem)

#define DA_STRUCT_ELEMENTS(T, S, E)                  \
    typedef struct _da_##T {                         \
        size_t size;                                 \
        size_t cap;                                  \
        S     *E;                                    \
    } DA_##T;                                        \
    void   da_resize_##T(DA_##T *array, size_t cap); \
    size_t da_append_##T(DA_##T *array, T elem);

#define DA_IMPL_ELEMENTS(T, E)                                             \
    static void *array_allocate(size_t size_of_elem, size_t num_of_elems); \
    void         da_resize_##T(DA_##T *array, size_t cap)                  \
    {                                                                      \
        if (array->cap >= cap) {                                           \
            return;                                                        \
        }                                                                  \
        if (array->cap == 0) {                                             \
            array->E = array_allocate(sizeof(T), 16);                      \
            array->cap = 16;                                               \
        } else {                                                           \
            size_t new_cap = array->cap;                                   \
            do {                                                           \
                new_cap *= 2;                                              \
            } while (new_cap < cap);                                       \
            T *new_elements = array_allocate(sizeof(T), new_cap);          \
            memcpy(new_elements, array->E, array->cap * sizeof(T));        \
            array->E = new_elements;                                       \
            array->cap = new_cap;                                          \
        }                                                                  \
    }                                                                      \
    size_t da_append_##T(DA_##T *array, T elem)                            \
    {                                                                      \
        da_resize_##T(array, array->size + 1);                             \
        array->E[array->size++] = elem;                                    \
        return array->size - 1;                                            \
    }

#define DA_STRUCT(T, S) DA_STRUCT_ELEMENTS(T, S, elements)
#define DA(T) DA_STRUCT(T, T)
#define DA_ELEMENTS(T, E) DA_STRUCT_ELEMENTS(T, T, E)
#define DA_IMPL(T) DA_IMPL_ELEMENTS(T, elements)

#endif /* __DA_H__ */
