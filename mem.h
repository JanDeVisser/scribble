/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdlib.h>

#include <log.h>

#ifndef __MEM_H__
#define __MEM_H__

#ifndef MEM_IMPL
typedef void Arena;
#endif

Arena *arena_new();
void *arena_allocate(Arena *arena, size_t size);
void arena_free(Arena *arena);

void* mem_allocate(size_t size);
void* mem_allocate_array(size_t count, size_t element_size);
void mem_free();

#endif /* __MEM_H__ */
