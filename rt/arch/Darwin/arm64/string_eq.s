/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.align 4

.global string_eq

//
// string_eq - Compare two strings
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

string_eq:
    stp     fp,lr,[sp,#-16]!
    mov     fp,sp

    cbz     ptr1, ptr_null
    cbz     ptr2, ptr_null

compare_lengths:
    cmp     len1, len2
    b.eq    lengths_equal
    mov     w0, wzr
    b       done

lengths_equal:
    mov     ix, len1

loop:
    cbnz    ix, comp_char
    mov     w0,#1
    b       done

comp_char:
    ldrb    ch1,[ptr1,ix,uxtw]
    ldrb    ch2,[ptr2,ix,uxtw]
    sub     ix,ix,#1
    cmp     ch1,ch2
    b.eq    loop
    mov     w0,wzr
    b       done

ptr_null:
    sub     x0, x0, x1
    cset    w0, eq
    and     w0, w0, #0xFF

done:
    mov     sp,fp
    ldp     fp,lr,[sp],#16
    ret
