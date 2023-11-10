/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.global _start

.include "arch/Darwin/arm64/syscalls.inc"

argc    .req w11
argc64  .req x11
argv    .req x12
ptr     .req x13
str     .req x14
len     .req x0

.align 2
_start:
    mov     fp,sp

    mov     argc,w0
    mov     argv,x1

    mov     ptr,xzr
    mov     len,xzr
    stp     ptr,len,[sp,#-16]!
    cbz     argc,done

    add     str,argv,argc64,lsl 3
 loop:
    ldr     ptr,[str,#-8]!
    mov     x0,ptr
    bl      scribble$strlen
    stp     ptr,len,[sp,#-16]!
    cmp     str,argv
    b.ne    loop

 done:
    # bl      static_initializer

    mov     x0,sp
    mov     w1,argc
    bl      main

    mov     x16, syscall_exit
    svc     0
