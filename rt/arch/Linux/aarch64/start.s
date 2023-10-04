/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.include "arch/Linux/aarch64/syscalls.inc"

.data

hello:
    .ascii        "Hello, ARM64!\n"
hello_len = . - hello

goodbye:
    .ascii        "Goodbye, ARM64!\n"
goodbye_len = . - goodbye

.text

.global _start

.align 2
_start:
    mov     fp,sp

    mov     x0, #1      /* fd := STDOUT_FILENO */
    ldr     x1, =hello    /* buf := msg */
    ldr     x2, =hello_len    /* count := len */
    mov     w8, syscall_write    /* write is syscall #64 */
    svc     #0          /* invoke syscall */

    # bl      static_initializer
    bl      main

    mov     x0, #1      
    ldr     x1, =goodbye
    ldr     x2, =goodbye_len
    mov     w8, syscall_write
    svc     #0          

    mov     w8, syscall_exit
    svc     0
