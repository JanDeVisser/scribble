/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "error.h"

char const* ErrorCode_name(ErrorCode code)
{
    switch (code) {
#undef ERRORCODE_ENUM
#define ERRORCODE_ENUM(code, value) \
    case code:                      \
        return #code;
        ERRORCODES(ERRORCODE_ENUM)
#undef ERRORCODE_ENUM
    default:
        UNREACHABLE();
    }
}

char const* Error_to_string(Error error)
{
    static char buffer[81];
    snprintf(buffer, 80, "ERROR: %s(%d): %s",
        ErrorCode_name(error.code), error.code, error.message);
    return buffer;
}
