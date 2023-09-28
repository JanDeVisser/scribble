/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>

int foo(int bar)
{
    fprintf(stderr, "IN FOO! I'M CALLED WITH 0x%04x\n", bar);
    return 2 * bar + 42;
}
