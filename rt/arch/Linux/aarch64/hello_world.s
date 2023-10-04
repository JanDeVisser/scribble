/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

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

    /* syscall write(int fd, const void *buf, size_t count) */
    mov     x0, #1              /* fd := STDOUT_FILENO  */
    ldr     x1, =hello          /* buf := hello         */
    ldr     x2, =hello_len      /* count := hello_len   */
    mov     w8, syscall_write   /* write is syscall #64 */
    svc     #0                  /* invoke syscall       */

    mov     w8, syscall_exit
    svc     0
