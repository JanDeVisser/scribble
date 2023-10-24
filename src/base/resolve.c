/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "sv.h"
#define HAVE_DLFCN_H

#include <limits.h>

#ifdef HAVE_DLFCN_H

#include <dlfcn.h>

#endif /* HAVE_DLFCN_H */

#include <string.h>

#define STATIC_ALLOCATOR

#include <allocate.h>
#include <config.h>
#include <error_or.h>
#include <log.h>
#include <mutex.h>
#include <resolve.h>
#include <threadonce.h>

#define SCRIBBLE_INIT "_scribble_init"

ErrorOr(DLResult, void *);

static ErrorOrDLResult _resolve_result_create(void *);

static LibHandle *_resolve_handle_create(StringView image);

static StringView _resolve_handle_get_platform_image(LibHandle *handle);

static LibHandle *_resolve_handle_open(LibHandle *handle);

static ErrorOrDLResult _resolve_handle_get_function(LibHandle *handle, StringView function_name);

static inline void __resolve_init(void);

static LibHandle *_resolve_open(Resolve *, StringView);

static Resolve *_singleton = NULL;

THREAD_ONCE(_resolve_once);
#define _resolve_init() ONCE(_resolve_once, __resolve_init)

static Mutex _resolve_mutex;

/* ------------------------------------------------------------------------ */

ErrorOrDLResult _resolve_result_create(void *result)
{
    char           *error;
    int             errorcode;
    ErrorOrDLResult ret;

    errorcode = 0;
    error = NULL;
    if (!result) {
#ifdef HAVE_DLFCN_H
        error = dlerror();
        trace(CAT_LIB, "dlerror(): %s", error);
        if (error && !strstr(error, "undefined symbol")) {
            error = strdup(error);
            errorcode = -1;
        } else {
            error = NULL;
        }
#elif defined(HAVE_WINDOWS_H)
        errorcode = GetLastError();
        if (errorcode == ERROR_PROC_NOT_FOUND) {
            errorcode = 0;
        } else if (errorcode) {
            FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, errorcode,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                (LPTSTR) &error, 0, NULL);
        } else {
            error = NULL;
        }
#endif /* HAVE_DLFCN_H */
    }
    if (errorcode) {
        ret.value = NULL;
        ret.error.cat = DLError;
        ret.error.code = errorcode;
        ret.error.message = error;
    } else {
        ret.value = result;
        ret.error.cat = NoError;
        ret.error.code = 0;
        ret.error.message = NULL;
    }
    return ret;
}

/* ------------------------------------------------------------------------ */

LibHandle *_resolve_handle_create(StringView image)
{
    LibHandle *ret = allocate_new(LibHandle);

    ret->image = image;
    ret->next = NULL;
    return _resolve_handle_open(ret);
}

StringView _resolve_handle_get_platform_image(LibHandle *handle)
{
    size_t len;
    char  *ptr;
    char  *canonical;
    char  *platform_image;
    int    ix;

    if (sv_empty(handle->image)) {
        handle->platform_image = sv_null();
        return handle->platform_image;
    }
    if (sv_empty(handle->platform_image)) {
        platform_image = allocate(PATH_MAX + 1);
        canonical = platform_image;
        len = sv_length(handle->image);
        if (len > PATH_MAX - 8)
            len = PATH_MAX - 8;
#ifdef __CYGWIN__
        strcpy(canonical, "cyg");
        canonical += strlen("cyg");
#endif /* __CYGWIN__ */
        for (ix = 0; ix < len; ix++) {
#ifdef __WIN32__
            canonical[ix] = tolower(handle->image[ix]);
#else  /* !__WIN32__ */
            canonical[ix] = handle->image.ptr[ix];
            if (canonical[ix] == '\\') {
                canonical[ix] = '/';
            }
#endif /* __WIN32__ */
        }
        canonical[len] = 0;

        if ((ptr = strrchr(canonical, '.'))) {
            ptr++;
            if ((*ptr != '/') && (*ptr != '\\')) {
                *ptr = 0;
            }
            canonical = ptr;
        } else {
            canonical[len + 1] = 0;
            canonical[len] = '.';
            canonical += (len + 1);
        }
#ifdef IS_WINDOWS
        strcpy(canonical, "dll");
#elif defined(IS_APPLE)
        strcpy(canonical, "dylib");
#else
        strcpy(canonical, "so");
#endif
        handle->platform_image = sv_from(platform_image);
    }
    return handle->platform_image;
}

LibHandle *_resolve_handle_try_open(LibHandle *handle, StringView dir)
{
    StringView      path;
    StringView      image;
    lib_handle_t    libhandle;
    ErrorOrDLResult res;

    image = _resolve_handle_get_platform_image(handle);
    if (!sv_empty(image)) {
        if (sv_not_empty(dir)) {
            path = sv_printf("%.*s/%.*s", SV_ARG(dir), SV_ARG(image));
        } else {
            path = _resolve_handle_get_platform_image(handle);
        }
        assert(*(path.ptr + path.length) == 0);
        trace(CAT_LIB, "Attempting to open library '%.*s'", SV_ARG(path));
    } else {
        path = sv_null();
        trace(CAT_LIB, "Attempting to open main program module");
    }
#ifdef HAVE_DLFCN_H
    dlerror();
    libhandle = dlopen(sv_not_empty(path) ? path.ptr : NULL, RTLD_NOW | RTLD_GLOBAL);
#elif defined(HAVE_WINDOWS_H)
    SetLastError(0);
    libhandle = (image) ? LoadLibrary(TEXT(path)) : GetModuleHandle(NULL);
#endif /* HAVE_DLFCN_H */
    res = _resolve_result_create((void *) libhandle);
    if (ErrorOrDLResult_has_value(res)) {
        handle->handle = (lib_handle_t) res.value;
    }
    if (handle->handle) {
        if ((sv_empty(path))) {
            path = sv_from("main program module");
        }
        trace(CAT_LIB, "Successfully opened '%.*s'", SV_ARG(path));
    }
    return handle;
}

LibHandle *_resolve_handle_open(LibHandle *handle)
{
    LibHandle      *ret = NULL;
    StringView      image;
    char           *obldir;
    ErrorOrDLResult result;

    image = _resolve_handle_get_platform_image(handle);
    if (sv_not_empty(image)) {
        trace(CAT_LIB, "resolve_open('%.*s') ~ '%.*s'", SV_ARG(handle->image), SV_ARG(image));
    } else {
        trace(CAT_LIB, "resolve_open('Main Program Image')");
    }
    handle->handle = NULL;
    if (sv_not_empty(image)) {
        obldir = getenv("SCRIBBLE_DIR");
        if (!obldir) {
            obldir = SCRIBBLE_DIR;
        }
        StringView workdir = sv_printf("%s/lib", obldir);
        _resolve_handle_try_open(handle, workdir);
        if (!handle->handle) {
            workdir = sv_printf("%s/bin", obldir);
            _resolve_handle_try_open(handle, workdir);
        }
        if (!handle->handle) {
            workdir = sv_substring(workdir, 0, sv_length(workdir) - 4);
            _resolve_handle_try_open(handle, workdir);
        }
        if (!handle->handle) {
            workdir = sv_printf("%s/share/lib", obldir);
            _resolve_handle_try_open(handle, workdir);
        }
        if (!handle->handle) {
            _resolve_handle_try_open(handle, sv_null());
        }
        if (!handle->handle) {
            _resolve_handle_try_open(handle, sv_from("lib"));
        }
        if (!handle->handle) {
            _resolve_handle_try_open(handle, sv_from("bin"));
        }
        if (!handle->handle) {
            _resolve_handle_try_open(handle, sv_from("share/lib"));
        }
    } else {
        _resolve_handle_try_open(handle, sv_null());
    }
    if (handle->handle) {
        ret = handle;
        if (sv_not_empty(image)) {
            result = _resolve_handle_get_function(handle, sv_from(SCRIBBLE_INIT));
            if (ErrorOrDLResult_has_value(result)) {
                trace(CAT_LIB, "resolve_open('%.*s'): Executing initializer", SV_ARG(handle->platform_image));
                ((void_t) result.value)();
            } else {
                trace(CAT_LIB, "resolve_open('%.*s'): No initializer", SV_ARG(handle->platform_image));
            }
        }
        trace(CAT_LIB, "Library '%.*s' opened successfully", SV_ARG(handle->platform_image));
    } else {
        fatal("resolve_open('%.*s') FAILED", SV_ARG(handle->platform_image));
    }
    return ret;
}

ErrorOrDLResult _resolve_handle_get_function(LibHandle *handle, StringView function_name)
{
    void_t function;

    trace(CAT_LIB, "dlsym('%.*s', '%.*s')", SV_ARG(_resolve_handle_get_platform_image(handle)), SV_ARG(function_name));
#ifdef HAVE_DLFCN_H
    dlerror();
    assert(*(function_name.ptr + function_name.length) == 0);
    function = (void_t) dlsym(handle->handle, function_name.ptr);
#elif defined(HAVE_WINDOWS_H)
    SetLastError(0);
    function = (void_t) GetProcAddress(handle->handle, function_name);
#endif /* HAVE_DLFCN_H */
    return _resolve_result_create(function);
}

/* ------------------------------------------------------------------------ */

void __resolve_init(void)
{
    assert(!_singleton);
    _resolve_mutex = mutex_create();
    _singleton = allocate_new(Resolve);
    _resolve_open(_singleton, sv_null());
    atexit(resolve_free);
}

Resolve *resolve_get(void)
{
    _resolve_init();
    return _singleton;
}

void resolve_free(void)
{
    LibHandle *image;
    if (_singleton) {
        trace(CAT_LIB, "resolve_free");
        while (_singleton->images) {
            image = _singleton->images;
            _singleton->images = image->next;
#ifdef HAVE_DLFCN_H
            dlclose(image->handle);
#elif defined(HAVE_WINDOWS_H)
            FreeLibrary(image->handle);
#endif /* HAVE_DLFCN_H */
        }
        _singleton = NULL;
    }
}

LibHandle *_resolve_open(Resolve *resolve, StringView image)
{
    LibHandle *handle = NULL;

    mutex_lock(_resolve_mutex);
    for (LibHandle *resolve_handle = resolve->images; resolve_handle; resolve_handle = resolve_handle->next) {
        if (sv_eq(image, resolve_handle->image)) {
            trace(CAT_LIB, "Image '%.*s' was cached", SV_ARG(image));
            handle = resolve_handle;
            break;
        }
    }
    if (!handle) {
        handle = _resolve_handle_create(image);
        handle->next = resolve->images;
        resolve->images = handle;
    }
    mutex_unlock(_resolve_mutex);
    return handle;
}

LibHandle *resolve_open(Resolve *resolve, StringView image)
{
    _resolve_init();
    return _resolve_open(resolve, image);
}

void_t resolve_resolve(Resolve *resolve, StringView lib_name, StringView func_name)
{
    LibHandle      *lib, *my_lib = NULL;
    ErrorOrDLResult result;

    int paren = sv_first(func_name, '(');
    if (paren > 0) {
        func_name = sv_copy(sv_substring(func_name, 0, paren));
    }

    for (lib = resolve->images; lib; lib = lib->next) {
        if (sv_eq(lib_name, lib->image)) {
            for (FunctionHandle *func_handle = lib->functions; func_handle; func_handle = func_handle->next) {
                if (sv_eq(func_name, func_handle->name)) {
                    trace(CAT_LIB, "Function '%s' was cached", func_name);
                    return func_handle->function;
                }
            }
            my_lib = lib;
        }
    }

    if (!my_lib) {
        my_lib = _resolve_open(resolve, lib_name);
        assert(my_lib);
    }
    trace(CAT_LIB, "dlsym('%.*s')", SV_ARG(func_name));
    void_t ret = MUST(DLResult, _resolve_handle_get_function(my_lib, func_name));
    if (ret) {
        FunctionHandle *fnc_handle = allocate_new(FunctionHandle);
        fnc_handle->name = sv_printf("%.*s", SV_ARG(func_name));
        fnc_handle->function = ret;
        fnc_handle->next = my_lib->functions;
        my_lib->functions = fnc_handle;
    }
    return ret;
}

bool resolve_library(StringView library)
{
    Resolve *resolve;

    resolve = resolve_get();
    assert(resolve);
    return resolve_open(resolve, library) != NULL;
}

void_t resolve_function(char const *func_name_cstr)
{
    StringView func_name = sv_from(func_name_cstr);
    Resolve   *resolve;
    StringView lib = sv_null();
    StringList lib_func = sv_split(func_name, sv_from(":"));

    resolve = resolve_get();
    assert(resolve);
    if (lib_func.size > 1) {
        lib = sv_copy(sl_front(&lib_func));
    }
    StringView func = sv_copy(sl_back(&lib_func));
    return resolve_resolve(resolve, lib, func);
}
