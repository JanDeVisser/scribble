/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdarg.h>
#include <stdint.h>
#include <string.h>

typedef struct slab {
    size_t size;
    char  *slab;
    size_t ptr;
} Slab;

typedef struct arena {
    size_t        current;
    size_t        capacity;
    size_t        slab_size;
    Slab         *slabs;
    struct arena *prev;
    struct arena *next;
} Arena;

typedef struct allocator {
    Arena  arenas;
    Arena *current;
    Arena *last;
    size_t num_slabs_per_arena;
    size_t size_of_slab;
} Allocator;

typedef struct {
    Arena *arena;
    size_t index;
    size_t ptr;
} AllocatorState;

static size_t const SLABS = 1024;
static size_t const SLAB_SZ = 10 * 1024 * 1024;

#define MEM_IMPL
#include <mem.h>

Arena *arena_new(size_t slabs, size_t slab_size);
void   arena_init(Arena *arena, size_t slabs, size_t slab_size);
void   arena_destroy(Arena *arena);
void   arena_allocate_slab(Arena *arena, size_t slab_size);
void  *arena_allocate(Arena *arena, size_t size);
void   arena_release(Arena *arena, AllocatorState state);

void *malloc_fatal(size_t size, char const *where, ...)
{
    void *ret = malloc(size);
    if (!ret) {
        va_list args;
        va_start(args, where);
        char strbuf[256];
        strcpy(strbuf, "Out of memory ");
        strncpy(strbuf, where, 255 - strlen("Out of memory "));
        strbuf[255] = 0;
        vfatal(strbuf, args);
        va_end(args);
    }
    memset(ret, 0, size);
    return ret;
}

Arena *arena_new(size_t slabs, size_t slab_size)
{
    Arena *ret = MALLOC(Arena);
    arena_init(ret, slabs, slab_size);
    return ret;
}

void arena_init(Arena *arena, size_t slabs, size_t slab_size)
{
    arena->slabs = MALLOC_ARR(Slab, slabs);
    arena->capacity = slabs;
    arena->slab_size = slab_size;
}

void arena_destroy(Arena *arena)
{
    for (size_t ix = 0; ix < arena->capacity; ++ix) {
        if (arena->slabs[ix].slab) {
            free(arena->slabs[ix].slab);
        }
    }
    free(arena->slabs);
    arena->current = 0;
    arena->slabs = NULL;
}

void arena_allocate_slab(Arena *arena, size_t slab_size)
{
    if (!arena->slabs) {
        arena->slabs = MALLOC_ARR(Slab, arena->capacity);
    }
    if (arena->slabs[arena->current].slab) {
        ++arena->current;
    }
    arena->slabs[arena->current].size = slab_size;
    arena->slabs[arena->current].ptr = 0;
    arena->slabs[arena->current].slab = malloc_fatal(slab_size, "allocating new slab of size %d", slab_size);
}

void *arena_allocate(Arena *arena, size_t size)
{
    void *ret;

    if (!arena->slabs || !arena->slabs[arena->current].slab || (arena->slabs[arena->current].ptr + size > arena->slabs[arena->current].size)) {
        if (arena->current + 1 >= arena->capacity) {
            return NULL;
        }
        arena_allocate_slab(arena, arena->slab_size);
    }
    ret = arena->slabs[arena->current].slab + arena->slabs[arena->current].ptr;
    arena->slabs[arena->current].ptr += size;
    return ret;
}

void arena_release(Arena *arena, AllocatorState state)
{
    if (state.arena == arena) {
        arena->current = state.index;
        arena->slabs[arena->current].ptr = state.ptr;
        memset(arena->slabs[arena->current].slab + state.ptr, 0, arena->slab_size - state.ptr);
        return;
    }
    arena->current = 0;
    arena->slabs[arena->current].ptr = 0;
    memset(arena->slabs[arena->current].slab, 0, arena->slab_size);
}

Allocator *allocator_new_with_size(size_t slabs, size_t slab_size)
{
    Allocator *ret = MALLOC(Allocator);
    allocator_init(ret, slabs, slab_size);
    return ret;
}

Allocator *allocator_new()
{
    return allocator_new_with_size(SLABS, SLAB_SZ);
}

void allocator_init(Allocator *alloc, size_t slabs, size_t slab_size)
{
    alloc->num_slabs_per_arena = slabs;
    alloc->size_of_slab = slab_size;
    alloc->current = &alloc->arenas;
    alloc->last = alloc->current;
    arena_init(alloc->current, slabs, slab_size);
}

void allocator_reset(Allocator *alloc)
{
    Arena *next_to_free;
    for (Arena *arena = alloc->last; arena && arena != &alloc->arenas; arena = next_to_free) {
        next_to_free = arena->prev;
        next_to_free->next = NULL;
        alloc->last = next_to_free;
        if (alloc->current == arena) {
            alloc->current = alloc->last;
        }
        arena_destroy(arena);
        free(arena);
    }
    arena_destroy(&alloc->arenas);
}

void allocator_allocate_arena(Allocator *alloc)
{
    assert(alloc->current);
    if (alloc->current->next) {
        alloc->current = alloc->current->next;
        return;
    }
    Arena *arena = arena_new(alloc->num_slabs_per_arena, alloc->size_of_slab);
    alloc->current->next = arena;
    arena->prev = alloc->current;
    alloc->current = arena;
    alloc->last = alloc->current;
}

void *allocator_allocate(Allocator *alloc, size_t size)
{
    if (!alloc->num_slabs_per_arena) {
        allocator_init(alloc, SLABS, SLAB_SZ);
    } else if (!alloc->arenas.capacity) {
        arena_init(&alloc->arenas, alloc->num_slabs_per_arena, alloc->size_of_slab);
    }
    void *ret = NULL;
    if (size > alloc->size_of_slab) {
        while (size > alloc->size_of_slab) {
            alloc->size_of_slab *= 2;
        }
    } else {
        ret = arena_allocate(alloc->current, size);
    }
    if (!ret) {
        allocator_allocate_arena(alloc);
        ret = arena_allocate(alloc->current, size);
    }
    trace(CAT_MEM, "M:0x%08zx:%5zu:0x%08zx", (uint64_t) alloc, size, (uint64_t) ret);
    assert(ret);
    return ret;
}

void *allocator_allocate_array(Allocator *alloc, size_t size, size_t num)
{
    return allocator_allocate(alloc, size * num);
}

AllocatorState allocator_save(Allocator *alloc)
{
    AllocatorState state = { 0 };
    state.arena = alloc->current;
    state.index = alloc->current->current;
    state.ptr = state.arena->slabs[state.arena->current].ptr;
    return state;
}

void allocator_release(Allocator *alloc, AllocatorState state)
{
    Arena *arena = alloc->last;
    do {
        arena_release(arena, state);
        arena = arena->prev;
    } while (arena && arena != state.arena);
}

static Allocator s_alloc = { 0 };

void *mem_allocate(size_t size)
{
    return allocator_allocate(&s_alloc, size);
}

void *mem_allocate_array(size_t count, size_t element_size)
{
    return mem_allocate(count * element_size);
}

void mem_destroy()
{
    allocator_reset(&s_alloc);
}

AllocatorState mem_save()
{
    return allocator_save(&s_alloc);
}

void mem_release(AllocatorState state)
{
    allocator_release(&s_alloc, state);
}
