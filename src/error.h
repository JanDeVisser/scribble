/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>

#include "log.h"

#ifndef __ERROR_H__
#define __ERROR_H__

#define ERRORCODES(S) \
    S(NoError, 0)     \
    S(IOError, 1)     \
    S(OutOfMemory, 2)

typedef enum {
#undef ERRORCODE_ENUM
#define ERRORCODE_ENUM(code, value) code = value,
    ERRORCODES(ERRORCODE_ENUM)
#undef ERRORCODE_ENUM
} ErrorCode;

typedef struct {
    ErrorCode   code;
    char const* message;
} Error;

extern char const* ErrorCode_name(ErrorCode code);
extern char const* Error_to_string(Error error);

#define ErrorOr(name, typ)                                                             \
    typedef struct {                                                                   \
        typ   value;                                                                   \
        Error error;                                                                   \
    } ErrorOr##name;                                                                   \
                                                                                       \
    inline static ErrorOr##name ErrorOr##name##_error(ErrorCode code, const char* msg) \
    {                                                                                  \
        ErrorOr##name ret = { 0 };                                                     \
        ret.error.code = code;                                                         \
        ret.error.message = msg;                                                       \
        return ret;                                                                    \
    }                                                                                  \
                                                                                       \
    inline static ErrorOr##name ErrorOr##name##_copy(Error error)                      \
    {                                                                                  \
        ErrorOr##name ret = { 0 };                                                     \
        ret.error.code = error.code;                                                   \
        ret.error.message = error.message;                                             \
        return ret;                                                                    \
    }                                                                                  \
                                                                                       \
    inline static ErrorOr##name ErrorOr##name##_return(typ value)                      \
    {                                                                                  \
        ErrorOr##name ret = { 0 };                                                     \
        ret.value = value;                                                             \
        return ret;                                                                    \
    }                                                                                  \
                                                                                       \
    inline static bool ErrorOr##name##_is_error(ErrorOr##name error_or)                \
    {                                                                                  \
        return error_or.error.code != NoError;                                         \
    }                                                                                  \
                                                                                       \
    inline static bool ErrorOr##name##_has_value(ErrorOr##name error_or)               \
    {                                                                                  \
        return !ErrorOr##name##_is_error(error_or);                                    \
    }

#define RETURN(name, expr) return ErrorOr##name##_return((expr))
#define ERROR(name, code, msg) return ErrorOr##name##_error(code, msg)

#define MUST(name, typ, var, expr)                     \
    typ var;                                           \
    {                                                  \
        ErrorOr##name var##_maybe = (expr);            \
        if (ErrorOr##name##_is_error(var##_maybe)) {   \
            fatal(Error_to_string(var##_maybe.error)); \
        }                                              \
        var = var##_maybe.value;                       \
    }

#define TRY(name, typ, var, expr)                    \
    typ var;                                         \
    {                                                \
        ErrorOr##name var##_maybe = (expr);          \
        if (ErrorOr##name##_is_error(var##_maybe)) { \
            return var##_maybe;                      \
        }                                            \
        var = var##_maybe.value;                     \
    }

#define TRY_TO(name, to_name, typ, var, expr)             \
    typ var;                                              \
    {                                                     \
        ErrorOr##name _maybe = (expr);                    \
        if (ErrorOr##name##_is_error(_maybe)) {           \
            return ErrorOr##to_name##_copy(_maybe.error); \
        }                                                 \
        var = var##_maybe.value;                          \
    }

ErrorOr(Char, char*);

#endif /* __ERROR_H__ */
