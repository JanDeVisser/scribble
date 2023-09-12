/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define HAVE_DLFCN_H

#include <rt.h>
#include <type.h>

#ifndef __NATIVE_H__
#define __NATIVE_H__

#ifdef HAVE_DLFCN_H
typedef void *lib_handle_t;
typedef int   resolve_error_t;
#elif defined(HAVE_WINDOWS_H)
typedef HMODULE lib_handle_t;
typedef DWORD   resolve_error_t;
#endif /* HAVE_DLFCN_H */

#include <sv.h>

typedef struct function_handle {
    StringView              name;
    void                   *function;
    struct function_handle *next;
} FunctionHandle;

typedef struct _resolve_handle {
    lib_handle_t            handle;
    StringView              image;
    StringView              platform_image;
    FunctionHandle         *functions;
    struct _resolve_handle *next;
} LibHandle;

typedef struct _resolve {
    LibHandle *images;
} Resolve;

Resolve   *resolve_get(void);
void       resolve_free(void);
LibHandle *resolve_open(Resolve *, StringView);
void_t     resolve_resolve(Resolve *, StringView, StringView);
bool       resolve_library(StringView);
void_t     resolve_function(StringView);

void native_call(StringView name, size_t argc, Datum **values, Datum *ret);

#endif /* __NATIVE_H__ */
