/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.global alloc
.global close
.global exit
.global fsize
.global open
.global read
.global stat
.global write

.equ syscall_syscall,   0xFFFF
.equ syscall_clone,     0x00dc
.equ syscall_exit,      0x005d
.equ syscall_read,      0x0063
.equ syscall_write,     0x0040
.equ syscall_openat,    0x0038
.equ syscall_close,     0x0039
.equ syscall_wait4,     0x0104
.equ syscall_statx,     0x0123
.equ syscall_mmap,      0x00de

.align 4
