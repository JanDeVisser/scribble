/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>
#include <stdint.h>

#include <allocate.h>
#include <sv.h>

DECLARE_SHARED_ALLOCATOR(sv)
SHARED_ALLOCATOR_IMPL(sv)

#define BLOCKSIZES(S) S(64) S(128) S(256) S(512) S(1024) S(2048) S(4096) S(8192) S(16384)

static char *sb_freelist[10] = {
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

#define SENTINEL 0xBABECAFE

char *allocate_for_length(size_t length, size_t *capacity)
{
    char  *ret = NULL;
    size_t cap = (!capacity || !*capacity) ? 32 : *capacity;
    while (cap < length)
        cap *= 2;
    size_t bit = 0;
    for (size_t c = cap; c > 1; c >>= 1) {
        bit++;
    }
    assert(bit >= 5);
    if (bit < 15) {
        if (sb_freelist[bit - 5]) {
            ret = sb_freelist[bit - 5];
            sb_freelist[bit - 5] = *((char **) ret);
        }
    }
    if (!ret) {
        ret = allocate(cap + 2 * sizeof(size_t));
        *((size_t *) ret + 1) = SENTINEL;
        trace(CAT_SV, "SALOC:0x%08zx:%5zu:%2zu", (uint64_t) ret, cap, bit - 5);
    } else {
        trace(CAT_SV, "SRUSE:0x%08zx:%5zu:%2zu", (uint64_t) ret, cap, bit - 5);
    }
    *((size_t *) ret) = cap;
    if (capacity) {
        *capacity = cap;
    }
    return ret + 2 * sizeof(size_t);
}

size_t buffer_capacity(char const *buffer)
{
    return (buffer && *((size_t *) buffer - 1) == SENTINEL) ? *((size_t *) buffer - 2) : 0;
}

void free_buffer(char *buffer)
{
    size_t capacity = buffer_capacity(buffer);
    if (!capacity) {
        return;
    }
    size_t bit = 0;
    for (size_t c = capacity; c > 1; c >>= 1) {
        bit++;
    }
    assert(bit >= 5);
    trace(CAT_SV, "SFREE:0x%08zx:%5zu:%2zu", (uint64_t) buffer, capacity, bit - 5);
    if (bit < 15) {
        *((size_t *) buffer - 2) = 0;
        *((char **) buffer) = sb_freelist[bit - 5];
        sb_freelist[bit - 5] = buffer - 2 * sizeof(size_t);
    }
}

static void sb_reallocate(StringBuilder *sb, size_t new_len)
{
    char  *ret = NULL;
    size_t cap = buffer_capacity(sb->view.ptr);
    size_t new_cap = (cap) ? cap : 32;
    while (new_cap < new_len + 1)
        new_cap *= 2;
    if (new_cap <= cap) {
        return;
    }
    ret = allocate_for_length(new_len + 1, &cap);
    if (sb->view.ptr) {
        memcpy(ret, sb->view.ptr, cap);
        free_buffer((char *) sb->view.ptr);
    }
    sb->view.ptr = ret;
}

StringBuilder sb_create()
{
    StringBuilder sb = { 0 };
    return sb;
}

StringBuilder sb_createf(char const *fmt, ...)
{
    StringBuilder ret = sb_create();
    va_list       args;
    va_start(args, fmt);
    sb_vprintf(&ret, fmt, args);
    va_end(args);
    return ret;
}

StringBuilder sb_vcreatef(char const *fmt, va_list args)
{
    StringBuilder ret = sb_create();
    sb_vprintf(&ret, fmt, args);
    return ret;
}

StringBuilder sb_copy_chars(char const *ptr, size_t len)
{
    StringBuilder sb = sb_create();
    size_t        cap = buffer_capacity(sb.view.ptr);
    sb.view.ptr = allocate_for_length(len + 1, &cap);
    if (len > 0) {
        memcpy((char *) sb.view.ptr, ptr, len);
        sb.view.length = len;
    }
    trace(CAT_SV, "SBCPC:0x%08zx:%5zu:%.60s", (uint64_t) sb.view.ptr, buffer_capacity(sb.view.ptr), sb.view.ptr);
    return sb;
}

StringBuilder sb_copy_sv(StringView sv)
{
    return sb_copy_chars(sv.ptr, sv.length + 1);
}

StringBuilder sb_copy_cstr(char const *s)
{
    return sb_copy_chars(s, strlen(s) + 1);
}

void sb_append_chars(StringBuilder *sb, char const *ptr, size_t len)
{
    sb_reallocate(sb, sb->view.length + len + 1);
    char *p = (char *) sb->view.ptr;
    memcpy(p + sb->view.length, ptr, len);
    sb->view.length += len;
    p[sb->view.length] = '\0';
    trace(CAT_SV, "SBAPC:0x%08zx:%5zu:%.60s", (uint64_t) sb->view.ptr, buffer_capacity(sb->view.ptr), sb->view.ptr);
}

void sb_append_sv(StringBuilder *sb, StringView sv)
{
    sb_append_chars(sb, sv.ptr, sv.length);
}

void sb_append_cstr(StringBuilder *sb, char const *s)
{
    sb_append_chars(sb, s, strlen(s));
}

void sb_vprintf(StringBuilder *sb, char const *fmt, va_list args)
{
    va_list args2;
    va_copy(args2, args);
    size_t len = vsnprintf(NULL, 0, fmt, args2);
    va_end(args2);
    sb_reallocate(sb, sb->view.length + len + 1);
    vsnprintf((char *) sb->view.ptr + sb->view.length, len + 1, fmt, args);
    sb->view.length += len;
    trace(CAT_SV, "SBVPF:0x%08zx:%5zu:%.60s", (uint64_t) sb->view.ptr, buffer_capacity(sb->view.ptr), sb->view.ptr);
}

void sb_printf(StringBuilder *sb, char const *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    sb_vprintf(sb, fmt, args);
}

void sb_insert_sv(StringBuilder *sb, StringView sv, size_t at)
{
    if (at >= sb->view.length) {
        sb_append_sv(sb, sv);
        return;
    }
    sb_insert_chars(sb, sv.ptr, sv.length, at);
}

void sb_insert_chars(StringBuilder *sb, char const *ptr, size_t len, size_t at)
{
    if (at >= sb->view.length) {
        sb_append_chars(sb, ptr, len);
        return;
    }
    sb_reallocate(sb, sb->view.length + len + 1);
    char *p = (char *) sb->view.ptr;
    memmove(p + at + len, p + at, sb->view.length - at);
    memcpy(p + at, ptr, len);
    sb->view.length += len;
    p[sb->view.length] = '\0';
    trace(CAT_SV, "SBAPC:0x%08zx:%5zu:%.60s", (uint64_t) sb->view.ptr, buffer_capacity(sb->view.ptr), sb->view.ptr);
}

void sb_insert_cstr(StringBuilder *sb, char const *str, size_t at)
{
    if (at >= sb->view.length) {
        sb_append_cstr(sb, str);
        return;
    }
    sb_insert_chars(sb, str, strlen(str), at);
}

StringView sb_view(StringBuilder *sb)
{
    return sb->view;
}
