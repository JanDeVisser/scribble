/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define HAVE_DLFCN_H

#include <limits.h>

#ifdef HAVE_DLFCN_H

#include <dlfcn.h>

#endif /* HAVE_DLFCN_H */

#include <string.h>

#define STATIC_ALLOCATOR

#include <allocate.h>
#include <config.h>
#include <datum.h>
#include <error.h>
#include <log.h>
#include <mutex.h>
#include <native.h>
#include <threadonce.h>
#include <type.h>

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

ErrorOrDLResult _resolve_result_create(void *result) {
    char *error;
    int errorcode;
    ErrorOrDLResult ret;

    errorcode = 0;
    error = NULL;
    if (!result) {
#ifdef HAVE_DLFCN_H
        error = dlerror();
        trace("dlerror(): %s", error);
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

LibHandle *_resolve_handle_create(StringView image) {
    LibHandle *ret = allocate_new(LibHandle);

    ret->image = image;
    ret->next = NULL;
    return _resolve_handle_open(ret);
}

StringView _resolve_handle_get_platform_image(LibHandle *handle) {
    size_t len;
    char *ptr;
    char *canonical;
    char *platform_image;
    int ix;

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
#if defined(__WIN32__) || defined(__CYGWIN__)
        strcpy(canonical, "dll");
#elif defined(__APPLE__)
        strcpy(canonical, "dylib");
#else
        strcpy(canonical, "so");
#endif
        handle->platform_image = sv_from(platform_image);
    }
    return handle->platform_image;
}

LibHandle *_resolve_handle_try_open(LibHandle *handle, StringView dir) {
    StringView path;
    StringView image;
    lib_handle_t libhandle;
    ErrorOrDLResult res;

    image = _resolve_handle_get_platform_image(handle);
    if (!sv_empty(image)) {
        if (sv_not_empty(dir)) {
            path = sv_printf("%.*s/%.*s", SV_ARG(dir), SV_ARG(image));
        } else {
            path = _resolve_handle_get_platform_image(handle);
        }
        assert(*(path.ptr + path.length) == 0);
        trace("Attempting to open library '%.*s'", SV_ARG(path));
    } else {
        path = sv_null();
        trace("Attempting to open main program module");
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
        trace("Successfully opened '%.*s'", SV_ARG(path));
    }
    return handle;
}

LibHandle *_resolve_handle_open(LibHandle *handle) {
    LibHandle *ret = NULL;
    StringView image;
    char *obldir;
    ErrorOrDLResult result;

    image = _resolve_handle_get_platform_image(handle);
    if (sv_not_empty(image)) {
        trace("resolve_open('%.*s') ~ '%.*s'", SV_ARG(handle->image), SV_ARG(image));
    } else {
        trace("resolve_open('Main Program Image')");
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
                trace("resolve_open('%.*s'): Executing initializer", SV_ARG(handle->platform_image));
                ((void_t) result.value)();
            } else {
                trace("resolve_open('%.*s'): No initializer", SV_ARG(handle->platform_image));
            }
        }
        trace("Library '%.*s' opened successfully", SV_ARG(handle->platform_image));
    } else {
        fatal("resolve_open('%.*s') FAILED", SV_ARG(handle->platform_image));
    }
    return ret;
}

ErrorOrDLResult _resolve_handle_get_function(LibHandle *handle, StringView function_name) {
    void_t function;

    trace("dlsym('%.*s', '%.*s')", SV_ARG(_resolve_handle_get_platform_image(handle)), SV_ARG(function_name));
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

void __resolve_init(void) {
    assert(!_singleton);
    _resolve_mutex = mutex_create();
    _singleton = allocate_new(Resolve);
    _resolve_open(_singleton, sv_null());
    atexit(resolve_free);
}

Resolve *resolve_get(void) {
    _resolve_init();
    return _singleton;
}

void resolve_free(void) {
    LibHandle *image;
    if (_singleton) {
        trace("resolve_free");
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

LibHandle *_resolve_open(Resolve *resolve, StringView image) {
    LibHandle *handle = NULL;

    mutex_lock(_resolve_mutex);
    for (LibHandle *resolve_handle = resolve->images; resolve_handle; resolve_handle = resolve_handle->next) {
        if (sv_eq(image, resolve_handle->image)) {
            trace("Image '%.*s' was cached", SV_ARG(image));
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

LibHandle *resolve_open(Resolve *resolve, StringView image) {
    _resolve_init();
    return _resolve_open(resolve, image);
}

void_t resolve_resolve(Resolve *resolve, StringView lib_name, StringView func_name) {
    LibHandle *lib, *my_lib = NULL;
    ErrorOrDLResult result;

    int paren = sv_find_char(func_name, '(');
    if (paren > 0) {
        func_name = sv_copy(sv_substring(func_name, 0, paren));
    }

    for (lib = resolve->images; lib; lib = lib->next) {
        if (sv_eq(lib_name, lib->image)) {
            for (FunctionHandle *func_handle = lib->functions; func_handle; func_handle = func_handle->next) {
                if (sv_eq(func_name, func_handle->name)) {
                    trace("Function '%s' was cached", func_name);
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
    trace("dlsym('%.*s')", SV_ARG(func_name));
    MUST(DLResult, void_t, ret, _resolve_handle_get_function(my_lib, func_name))
    if (ret) {
        FunctionHandle *fnc_handle = allocate_new(FunctionHandle);
        fnc_handle->name = sv_printf("%.*s", SV_ARG(func_name));
        fnc_handle->function = ret;
        fnc_handle->next = my_lib->functions;
        my_lib->functions = fnc_handle;
    }
    return ret;
}

bool resolve_library(StringView library) {
    Resolve *resolve;

    resolve = resolve_get();
    assert(resolve);
    return resolve_open(resolve, library) != NULL;
}

void_t resolve_function(StringView func_name) {
    Resolve *resolve;
    StringView lib_func[2];
    StringView lib = sv_null();
    size_t has_lib = sv_split(func_name, sv_from(":"), 2, lib_func);

    resolve = resolve_get();
    assert(resolve);
    if (has_lib > 1) {
        lib = sv_copy(lib_func[0]);
    }
    StringView func = sv_copy(lib_func[has_lib - 1]);
    return resolve_resolve(resolve, lib, func);
}

typedef struct trampoline {
    void_t fnc;
    uint64_t x[8];
    double d[8];
    uint64_t int_return_value;
    double double_return_value;
} Trampoline;

void native_call(StringView name, size_t argc, Datum **values, Datum *ret) {
    if (argc > 8) {
        fatal("Can't do native calls with more than 8 parameters");
    }
    Trampoline t = {0};
    t.fnc = resolve_function(name);
    if (!t.fnc) {
        fatal("Function '%.*s' not found", SV_ARG(name));
    }

    // Stage A - Initialization
    // This stage is performed exactly once, before processing of the arguments
    // commences.

    // A.1 The Next General-purpose Register Number (NGRN) is set to zero.
    size_t ngrn = 0;
    // A.2 The Next SIMD and Floating-point Register Number (NSRN) is set to
    // zero.
    size_t nsrn = 0;
    // A.3 The Next Scalable Predicate Register Number (NPRN) is set to zero.
    size_t nprn = 0;
    // A.4 The next stacked argument address (NSAA) is set to the current
    // stack-pointer value (SP).
    size_t nsaa = 0;

    for (size_t ix = 0; ix < argc; ++ix) {
        ExpressionType *et = type_registry_get_type_by_id(values[ix]->type);

        // Stage B – Pre-padding and extension of arguments
        // For each argument in the list the first matching rule from the
        // following list is applied. If no rule matches the argument is used
        // unmodified.

        // B.1 If the argument type is a Pure Scalable Type, no change is made
        // at this stage.

        // B.2 If the argument type is a Composite Type whose size cannot be
        // statically determined by both the caller and the callee, the
        // argument is copied to memory and the argument is replaced by a
        // pointer to the copy. (There are no such types in C/C++ but they
        // exist in other languages or in language extensions).

        // B.3 If the argument type is an HFA or an HVA, then the argument is
        // used unmodified.
        // TODO

        // B.4 If the argument type is a Composite Type that is larger than 16
        // bytes, then the argument is copied to memory allocated by the caller
        // and the argument is replaced by a pointer to the copy.
        // TODO

        // B.5 If the argument type is a Composite Type then the size of the
        // argument is rounded up to the nearest multiple of 8 bytes.

        // B.6 If the argument is an alignment adjusted type its value is passed
        // as a copy of the actual value. The copy will have an alignment
        // defined as follows:
        //     • For a Fundamental Data Type, the alignment is the natural
        //       alignment of that type, after any promotions.
        //     • For a Composite Type, the alignment of the copy will have
        //       8-byte alignment if its natural alignment is ≤ 8 and 16-byte
        //       alignment if its natural alignment is ≥ 16.
        // The alignment of the copy is used for applying marshaling rules.

        // Stage C – Assignment of arguments to registers and stack
        // For each argument in the list the following rules are applied in
        // turn until the argument has been allocated. When an argument is
        // assigned to a register any unused bits in the register have
        // unspecified value. When an argument is assigned to a stack slot any
        // unused padding bytes have unspecified value.

        // C.1 If the argument is a Half-, Single-, Double- or Quad- precision
        // Floating-point or short vector type and the NSRN is less than 8, then
        // the argument is allocated to the least significant bits of register
        // v[NSRN]. The NSRN is incremented by one. The argument has now been
        // allocated.
        if ((et->type_id == FLOAT_ID) && (nsrn < 8)) {
            t.d[nsrn] = values[ix]->float_value;
            ++nsrn;
            continue;
        }

        // C.2 If the argument is an HFA or an HVA and there are sufficient
        // unallocated SIMD and Floating-point registers (NSRN + number of
        // members ≤ 8), then the argument is allocated to SIMD and
        // Floating-point registers (with one register per member of the HFA or
        // HVA). The NSRN is incremented by the number of registers used. The
        // argument has now been allocated.
        // TODO

        // C.3 If the argument is an HFA or an HVA then the NSRN is set to 8 and
        // the size of the argument is rounded up to the nearest multiple of 8
        // bytes.
        // TODO

        // C.4 If the argument is an HFA, an HVA, a Quad-precision
        // Floating-point or short vector type then the NSAA is rounded up to
        // the next multiple of 8 if its natural alignment is ≤ 8 or the next
        // multiple of 16 if its natural alignment is ≥ 16.
        // TODO

        // C.5 If the argument is a Half- or Single- precision Floating Point
        // type, then the size of the argument is set to 8 bytes. The effect is
        // as if the argument had been copied to the least significant bits of a
        // 64-bit register and the remaining bits filled with unspecified
        // values.
        // Not supported

        // C.6 If the argument is an HFA, an HVA, a Half-, Single-, Double- or
        // Quad- precision Floating-point or short vector type, then the
        // argument is copied to memory at the adjusted NSAA. The NSAA is
        // incremented by the size of the argument. The argument has now been
        // allocated.
        // TODO

        // C.7 If the argument is a Pure Scalable Type that consists of NV
        // Scalable Vector Types and NP Scalable Predicate Types, if the
        // argument is named, if NSRN+NV ≤ 8, and if NPRN+NP ≤ 4, then the
        // Scalable Vector Types are allocated in order to
        // z[NSRN]...z[NSRN+NV-1] inclusive and the Scalable Predicate Types are
        // allocated in order to p[NPRN]...p[NPRN+NP-1] inclusive. The NSRN is
        // incremented by NV and the NPRN is incremented by NP. The argument has
        // now been allocated.
        // TODO

        // C.8 If the argument is a Pure Scalable Type that has not been
        // allocated by the rules above, then the argument is copied to memory
        // allocated by the caller and the argument is replaced by a pointer to
        // the copy (as for B.4 above). The argument is then allocated according
        // to the rules below.
        // TODO

        // C.9 If the argument is an Integral or Pointer Type, the size of the
        // argument is less than or equal to 8 bytes and the NGRN is less than
        // 8, the argument is copied to the least significant bits in x[NGRN].
        // The NGRN is incremented by one. The argument has now been allocated.
        if ((type_kind(et) == TK_PRIMITIVE) && (ngrn < 8)) {
            PrimitiveType primitive_type = typeid_primitive_type(et->type_id);
            if (PrimitiveType_is_integer(primitive_type) || primitive_type == PT_POINTER) {
                t.x[ngrn] = datum_unsigned_integer_value(values[ix]);
                ++ngrn;
                continue;
            }
        }

        // C.10 If the argument has an alignment of 16 then the NGRN is rounded
        // up to the next even number.

        // C.11 If the argument is an Integral Type, the size of the argument
        // is equal to 16 and the NGRN is less than 7, the argument is copied to
        // x[NGRN] and x[NGRN+1]. x[NGRN] shall contain the lower addressed
        // double-word of the memory representation of the argument. The NGRN
        // is incremented by two. The argument has now been allocated.

        // C.12 If the argument is a Composite Type and the size in double-words
        // of the argument is not more than 8 minus NGRN, then the argument is
        // copied into consecutive general-purpose registers, starting at
        // x[NGRN]. The argument is passed as though it had been loaded into the
        // registers from a double-word-aligned address with an appropriate
        // sequence of LDR instructions loading consecutive registers from
        // memory (the contents of any unused parts of the registers are
        // unspecified by this standard). The NGRN is incremented by the number
        // of registers used. The argument has now been allocated.

        // C.13 The NGRN is set to 8.

        // C.14 The NSAA is rounded up to the larger of 8 or the Natural
        // Alignment of the argument’s type.

        // C.15 If the argument is a composite type then the argument is copied
        // to memory at the adjusted NSAA. The NSAA is incremented by the size
        // of the argument. The argument has now been allocated.

        // C.16 If the size of the argument is less than 8 bytes then the size
        // of the argument is set to 8 bytes. The effect is as if the argument
        // was copied to the least significant bits of a 64-bit register and the
        // remaining bits filled with unspecified values.

        // C.17 The argument is copied to memory at the adjusted NSAA. The NSAA
        // is incremented by the size of the argument. The argument has now been
        // allocated.
    }

    int trampoline_result = trampoline(&t);
    if (trampoline_result) {
        fatal("Error executing '%.*s'. Trampoline returned %d", SV_ARG(name), trampoline_result);
    }
    switch (typeid_primitive_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case PT_##dt:                                       \
        ret->n = (ct) t.int_return_value;               \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
        case PT_BOOL:
            ret->bool_value = (bool) t.int_return_value;
            break;
        case PT_POINTER:
            ret->pointer = (void *) t.int_return_value;
            break;
        case PT_FLOAT:
            ret->float_value = t.double_return_value;
            break;
        case PT_VOID:
            ret->void_value = 0;
            break;
        default:
            UNREACHABLE();
    }
}
