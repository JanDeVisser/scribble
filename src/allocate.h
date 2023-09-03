/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <mem.h>

#ifndef __ALLOCATE_H__
#define __ALLOCATE_H__

#ifdef STATIC_ALLOCATOR

#define allocate_new(t) allocate(sizeof(t))
#define allocate_array(t, num) array_allocate(sizeof(t), num)

static Allocator *s_alloc = NULL;

static Allocator * get_allocator()
{
    if (!s_alloc) {
#ifdef ALLOCATOR_SLAB_SZ
#ifndef ALLOCATOR_SLABS
#define ALLOCATOR_SLABS 1024
#endif
        s_alloc = allocator_new_with_size(ALLOCATOR_SLABS, ALLOCATOR_SLAB_SZ);
#else
        s_alloc = allocator_new();
#endif
    }
    return s_alloc;
}

static void *allocate(size_t size)
{
    return allocator_allocate(get_allocator(), size);
}

static void *array_allocate(size_t size_of_elem, size_t num_of_elems)
{
    return allocator_allocate_array(get_allocator(), size_of_elem, num_of_elems);
}

static AllocatorState save_allocator()
{
    return allocator_save(get_allocator());
}

static void release_allocator(AllocatorState savepoint)
{
    assert(s_alloc);
    allocator_release(get_allocator(), savepoint);
}

#endif /* STATIC_ALLOCATOR */

#ifdef SHARED_ALLOCATOR

void * SHARED_ALLOCATOR ## _allocate(size_t size);
void * SHARED_ALLOCATOR ## _array_allocate(size_t size_of_elem, size_t num_of_elems);
AllocatorState SHARED_ALLOCATOR ## _save_allocator();
void SHARED_ALLOCATOR ## _release_allocator(AllocatorState savepoint);

#define allocate_new(t) SHARED_ALLOCATOR ## _allocate(sizeof(t))
#define allocate_array(t, num) SHARED_ALLOCATOR ## _array_allocate(sizeof(t), num)
#define save_allocator() SHARED_ALLOCATOR ## _save_allocator()
#define release_allocator() SHARED_ALLOCATOR ## _release_allocator()

#ifdef SHARED_ALLOCATOR_IMPL

static Allocator *s_alloc_ ## SHARED_ALLOCATOR = NULL;

void * SHARED_ALLOCATOR ## _allocate(size_t size)
{
    if (!s_alloc_ ## SHARED_ALLOCATOR) {
        s_alloc_ ## SHARED_ALLOCATOR = allocator_new();
    }
    return allocator_allocate(s_alloc_ ## SHARED_ALLOCATOR, size);
}

void * SHARED_ALLOCATOR ## _array_allocate(size_t size_of_elem, size_t num_of_elems)
{
    if (!s_alloc) {
        s_alloc = allocator_new();
    }
    return allocator_allocate_array(s_alloc_ ## SHARED_ALLOCATOR, size_of_elem, num_of_elems);
}

AllocatorState SHARED_ALLOCATOR ## _save_allocator()
{
    if (!s_alloc) {
        s_alloc = allocator_new();
    }
    return allocator_save(s_alloc_ ## SHARED_ALLOCATOR);
}

void SHARED_ALLOCATOR ## _release_allocator(AllocatorState savepoint)
{
    if (!s_alloc) {
        s_alloc = allocator_new();
    }
    allocator_release(s_alloc_ ## SHARED_ALLOCATOR, savepoint);
}
#endif /* SHARED_ALLOCATOR_IMPL */

#endif /* SHARED_ALLOCATOR */

#endif /* __ALLOCATE_H__ */
