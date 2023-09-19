/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>

#include <datum.h>
#include <type.h>

#ifndef __RT_H__
#define __RT_H__

size_t  endln();
size_t  putint(int64_t);
int     trampoline(void *trampoline);

#endif // __RT_H__
