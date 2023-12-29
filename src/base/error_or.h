/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

#include <log.h>
#include <mem.h>

#ifndef __ERROR_H__
#define __ERROR_H__

#define ERRORCATEGORIES(S) \
    S(NoError, 0)          \
    S(DLError, 1)          \
    S(IOError, 2)          \
    S(OutOfMemory, 3)      \
    S(ProcessError, 4)     \
    S(TypeError, 5)        \
    S(LexerError, 6)       \
    S(ParserError, 7)      \
    S(CompilerError, 8)    \
    S(RuntimeError, 9)     \
    S(HttpError, 10)

typedef enum {
#undef ERRORCATEGORY_ENUM
#define ERRORCATEGORY_ENUM(code, value) code = value,
    ERRORCATEGORIES(ERRORCATEGORY_ENUM)
#undef ERRORCATEGORY_ENUM
} ErrorCategory;

typedef struct {
    ErrorCategory cat;
    int           code;
    char         *message;
} Error;

extern char const *ErrorCategory_name(ErrorCategory code);
extern char const *Error_to_string(Error error);

#define ErrorOr(name, typ)                                                                                         \
    typedef struct {                                                                                               \
        typ   value;                                                                                               \
        Error error;                                                                                               \
    } ErrorOr##name;                                                                                               \
                                                                                                                   \
    inline static ErrorOr##name ErrorOr##name##_verror(ErrorCategory cat, int code, char const *msg, va_list args) \
    {                                                                                                              \
        ErrorOr##name ret = { 0 };                                                                                 \
        ret.error.cat = cat;                                                                                       \
        ret.error.code = code;                                                                                     \
        size_t msg_len = vsnprintf(NULL, 0, msg, args) + 1;                                                            \
        ret.error.message = (char *) mem_allocate(msg_len);                                                        \
        vsnprintf(ret.error.message, msg_len, msg, args);                                                          \
        return ret;                                                                                                \
    }                                                                                                              \
                                                                                                                   \
    inline static ErrorOr##name ErrorOr##name##_error(ErrorCategory cat, int code, char const *msg, ...)           \
    {                                                                                                              \
        va_list args;                                                                                              \
        va_start(args, msg);                                                                                       \
        ErrorOr##name ret = ErrorOr##name##_verror(cat, code, msg, args);                                          \
        va_end(args);                                                                                              \
        return ret;                                                                                                \
    }                                                                                                              \
                                                                                                                   \
    inline static ErrorOr##name ErrorOr##name##_copy(Error error)                                                  \
    {                                                                                                              \
        ErrorOr##name ret = { 0 };                                                                                 \
        ret.error.cat = error.cat;                                                                                 \
        ret.error.code = error.code;                                                                               \
        ret.error.message = error.message;                                                                         \
        return ret;                                                                                                \
    }                                                                                                              \
                                                                                                                   \
    inline static ErrorOr##name ErrorOr##name##_return(typ value)                                                  \
    {                                                                                                              \
        ErrorOr##name ret = { 0 };                                                                                 \
        ret.value = value;                                                                                         \
        return ret;                                                                                                \
    }                                                                                                              \
                                                                                                                   \
    inline static bool ErrorOr##name##_is_error(ErrorOr##name error_or)                                            \
    {                                                                                                              \
        return error_or.error.cat != NoError;                                                                      \
    }                                                                                                              \
                                                                                                                   \
    inline static bool ErrorOr##name##_has_value(ErrorOr##name error_or)                                           \
    {                                                                                                              \
        return !ErrorOr##name##_is_error(error_or);                                                                \
    }

#define RETURN(name, expr) return ErrorOr##name##_return((expr))
#define ERROR(name, cat, code, msg, ...) return ErrorOr##name##_error(cat, code, msg __VA_OPT__(, ) __VA_ARGS__)
#define VERROR(name, cat, code, msg, args) return ErrorOr##name##_verror(cat, code, msg, args)

#define MUST(name, expr)                                \
    ({                                                  \
        ErrorOr##name name##_maybe = (expr);            \
        if (ErrorOr##name##_is_error(name##_maybe)) {   \
            fatal(Error_to_string(name##_maybe.error)); \
        }                                               \
        name##_maybe.value;                             \
    })

#define TRY(name, expr)                               \
    ({                                                \
        ErrorOr##name name##_maybe = (expr);          \
        if (ErrorOr##name##_is_error(name##_maybe)) { \
            return name##_maybe;                      \
        }                                             \
        name##_maybe.value;                           \
    })

#define TRY_OR_FALSE(name, expr) ({               \
    ErrorOr##name name##_maybe = (expr);          \
    if (ErrorOr##name##_is_error(name##_maybe)) { \
        return false;                             \
    }                                             \
    name##_maybe.value;                           \
})

#define TRY_OR_NULL(name, expr) ({                \
    ErrorOr##name name##_maybe = (expr);          \
    if (ErrorOr##name##_is_error(name##_maybe)) { \
        return NULL;                              \
    }                                             \
    name##_maybe.value;                           \
})

#define TRY_TO(name, to_name, expr)                       \
    ({                                                    \
        ErrorOr##name _maybe = (expr);                    \
        if (ErrorOr##name##_is_error(_maybe)) {           \
            return ErrorOr##to_name##_copy(_maybe.error); \
        }                                                 \
        _maybe.value;                                     \
    })

ErrorOr(Char, char *);
ErrorOr(VoidPtr, void *);
ErrorOr(UInt64, uint64_t);
ErrorOr(Int, int);

#endif /* __ERROR_H__ */
