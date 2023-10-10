/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>
#include <stdlib.h>

#ifndef __HASH_H__
#define __HASH_H__

unsigned int hash(void const *buf, size_t size);
unsigned int hashptr(void const *ptr);
unsigned int hashlong(long val);
unsigned int hashdouble(double val);
unsigned int hashblend(unsigned int h1, unsigned int h2);

#endif /* __HASH_H__ */
