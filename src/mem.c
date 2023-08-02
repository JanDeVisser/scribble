/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <string.h>

#define SLABS 1024

typedef struct slab {
    size_t size;
    char *slab;
    size_t ptr;
} Slab;

typedef struct {
    Slab slabs[SLABS];
    int current;
} Arena;

const static size_t ARENA_SZ = 10 * 1024 * 1024;

#define MEM_IMPL
#include <mem.h>

void slab_free(Slab *slab)
{
    if (slab->slab) free(slab->slab);
    slab->ptr = 0;
    slab->size = 0;
    slab->slab = NULL;
}

void arena_allocate_slab(Arena *arena)
{
    if (arena->current >= SLABS) {
        fatal("Out of slabs");
    }
    if (arena->slabs[arena->current].slab) {
        ++arena->current;
    }
    arena->slabs[arena->current].size = ARENA_SZ;
    arena->slabs[arena->current].ptr = 0;
    if (!(arena->slabs[arena->current].slab = malloc(arena->slabs[arena->current].size))) {
        OUT_OF_MEMORY("Could not allocate arena slab");
    }
    memset(arena->slabs[arena->current].slab, '\0', ARENA_SZ);
}

Arena* arena_new()
{
    Arena *ret = (Arena*) malloc(sizeof(Arena));
    if (!ret) {
        OUT_OF_MEMORY("Could not malloc() new arena");
    }
    return ret;
}

void *arena_allocate(Arena *arena, size_t size)
{
    void* ret;

    if (!arena->slabs[arena->current].slab || (arena->slabs[arena->current].ptr + size > arena->slabs[arena->current].size)) {
        arena_allocate_slab(arena);
    }
    ret = arena->slabs[arena->current].slab + arena->slabs[arena->current].ptr;
    arena->slabs[arena->current].ptr += size;
    return ret;
}

void *arena_allocate_array(Arena *arena, size_t size, size_t num)
{
    return arena_allocate(arena, size*num);
}

void arena_reset(Arena *arena)
{
    for (int ix = 0; ix <= arena->current; ++ix) {
        slab_free(&arena->slabs[ix]);
    }
    memset(arena, 0, sizeof(Arena));
}

SlabPointer arena_save(Arena *arena)
{
    SlabPointer ptr = {0};
    ptr.index = arena->current;
    ptr.ptr = arena->slabs[arena->current].ptr;
    return ptr;
}

void arena_release(Arena *arena, SlabPointer ptr)
{
    while (arena->current > ptr.index) {
        arena->slabs[arena->current--].ptr = 0;
    }
    arena->slabs[arena->current].ptr = ptr.ptr;
}

static Arena s_arena = {0};

void* mem_allocate(size_t size)
{
    return arena_allocate(&s_arena, size);
}

void* mem_allocate_array(size_t count, size_t element_size)
{
    return mem_allocate(count * element_size);
}

void mem_free()
{
    arena_reset(&s_arena);
}

SlabPointer mem_save()
{
    return arena_save(&s_arena);
}

void mem_release(SlabPointer ptr)
{
    arena_release(&s_arena, ptr);
}
