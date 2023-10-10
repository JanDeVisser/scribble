/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <resolve.h>
#include <rt.h>
#include <type.h>

#ifndef __NATIVE_H__
#define __NATIVE_H__

void native_call(StringView name, size_t argc, Datum **values, Datum *ret);

#endif /* __NATIVE_H__ */
