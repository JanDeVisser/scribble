/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.include "arch/Linux/aarch64/syscalls.inc"
.align 4

alloc:
    mov     x1,x0
    mov     x0,xzr
    mov     w2,#3
    mov     w3,#0x1002
    mov     w4,#-1
    mov     x5,xzr
    mov     w8,syscall_mmap
    svc     #0x00
