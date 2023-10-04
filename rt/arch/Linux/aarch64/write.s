/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.include "arch/Linux/aarch64/syscalls.inc"

.align 4

.global scribble$write
.global _scribble$write

// In:
fd  .req x0    // fd to write to (0=stdin)
buffer .req x1 // Buffer to write from
len .req w2    // Number of characters to write

// Out
// x0: >0: bytes written, <0: -errno

scribble$write:
_scribble$write:
        mov     w8,syscall_write // Linux write system call
        svc     #0x80            // Call kernel to output the string
        b.lo    __write_ok
        neg     x0,x0
__write_ok:
        ret
