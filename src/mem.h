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
typedef void Allocator;

typedef struct {
    void  *v;
    size_t index;
    size_t ptr;
} AllocatorState;
#endif

#define MALLOC(t) ((t *) malloc_fatal(sizeof(t), "allocating " #t))
#define MALLOC_ARR(t, num) ((t *) malloc_fatal((num * sizeof(t)), "allocating array of %d " #t "s", num));

void          *malloc_fatal(size_t size, char const *where, ...);
Allocator     *allocator_new_with_size(size_t slabs, size_t slab_size);
Allocator     *allocator_new();
void           allocator_init(Allocator *alloc, size_t slabs, size_t slab_size);
void          *allocator_allocate(Allocator *alloc, size_t size);
void          *allocator_allocate_array(Allocator *alloc, size_t size, size_t num);
void           allocator_reset(Allocator *alloc);
AllocatorState allocator_save(Allocator *alloc);
void           allocator_release(Allocator *alloc, AllocatorState state);
void          *mem_allocate(size_t size);
void          *mem_allocate_array(size_t count, size_t element_size);
void           mem_free();
AllocatorState mem_save();
void           mem_release(AllocatorState state);

#endif /* __MEM_H__ */
