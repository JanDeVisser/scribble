/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.include "arch/Darwin/arm64/syscalls.inc"

.align 4
.global scribble$close

//
// scribble$close - Call the close(2) syscall
//
// In:
fh      .req x0     // File descriptor

// Out:
//   x0: 0 for success, otherwise -errno

// Work:
//   ---

scribble$close:
    stp         fp,lr,[sp,#-16]!
    mov         fp,sp
    mov         x16,syscall_close
    svc         #0x00
    mov         sp,fp
    ldp         fp,lr,[sp],#16
    ret
