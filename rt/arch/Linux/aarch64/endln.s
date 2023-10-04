/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.include "arch/Linux/aarch64/syscalls.inc"
.align 4

.global endln
.global _endln

//
// endln - Prints a newline character
//
// In:

// Out:
//   x0: Number of characters printed.

// Work:
//   x16 - syscall

_endln:
endln:
    stp     fp,lr,[sp,#-16]!
    mov     fp,sp

    mov     x0,1
    adr     x1,__str_newline
    mov     x2,#1
    mov     w8,syscall_write
    svc     #0x00
    ldp     fp,lr,[sp],#16
    ret

__str_newline:
    .string "\n"
