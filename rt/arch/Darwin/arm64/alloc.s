/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.include "arch/Darwin/arm64/syscalls.inc"

.align 4
.global scribble$alloc

//
// scribble$alloc - Allocate a block of memory using mmap(2)
//
// In:
sz      .req x0     // Number of bytes to allocate

// Out:
//   x0: Pointer to the allocated block or 0 on failure

// Work:
//   ---

scribble$alloc:
    stp     fp,lr,[sp,#-16]!
    mov     fp,sp
    mov     x1,x0
    mov     x0,xzr
    mov     w2,#3
    mov     w3,#0x1002
    mov     w4,#-1
    mov     x5,xzr
    mov     x16,syscall_mmap
    svc     #0x00
    mov     sp,fp
    ldp     fp,lr,[sp],#16
    ret
