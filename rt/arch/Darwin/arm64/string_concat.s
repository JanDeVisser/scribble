/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.align 4

.global string_concat

//
// string_concat - Concatenate two strings
//

// In:
ptr1     .req x0     // Pointer to the string buffer 1
len1     .req w1     // Length of the string 1
ptr2     .req x2     // Pointer to the string buffer 1
len2     .req w3     // Length of the string 1

// Out:
//   w0: 1 if the strings are equals, 0 if not.

// Work:
ix      .req w9
ix64    .req x9
ch1     .req w10
ch2     .req w11

string_concat:
    stp     fp,lr,[sp,#-16]!
    sub     sp,sp,48
    mov     fp,sp

    stp     x0, x1, [sp,32]  ; lhs
    stp     x2, x3, [sp,16]  ; rhs
    bl      sb_create
    stp     x0, x1, [sp]     ; Store new stringbuilder on stack
    mov     x0, sp           ; Get pointer to new stringbuilder
    ldp     x1, x2, [sp, 32] ; Get lhs
    bl      sb_append        ; Copy lhs into stringbuilder
    mov     x0, sp           ; Get pointer to new stringbuilder
    ldp     x1, x2, [sp, 16] ; Get rhs
    bl      sb_append        ; Copy rhs into stringbuilder
    ldp     x0, x1, [sp]     ; Return stringbuilder

    mov     sp,fp
    ldp     fp,lr,[sp],#16
    ret
