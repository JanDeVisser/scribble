/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdlib.h>

#include "log.h"

#ifndef __MEM_H__
#define __MEM_H__

#ifndef MEM_IMPL
typedef void Arena;
#endif

typedef struct {
    size_t index;
    size_t ptr;
} SlabPointer;

Arena      *arena_new();
void       *arena_allocate(Arena *arena, size_t size);
void       *arena_allocate_array(Arena *arena, size_t size, size_t num);
void        arena_free(Arena *arena);
SlabPointer arena_save(Arena *arena);
void        arena_release(Arena *arena, SlabPointer pointer);
void       *mem_allocate(size_t size);
void       *mem_allocate_array(size_t count, size_t element_size);
void        mem_free();
SlabPointer mem_save();
void        mem_release(SlabPointer pointer);

#endif /* __MEM_H__ */
