/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.align 4

.global string_cmp

//
// string_cmp - Compare two strings. Similar to strcmp(3).
//

// In:
ptr1     .req x0     // Pointer to the string buffer 1
len1     .req w1     // Length of the string 1
ptr2     .req x2     // Pointer to the string buffer 1
len2     .req w3     // Length of the string 1

// Out:
//   w0: >0 if the string 1 > string 2,
//       0 if they are equal,
//       <0 if string 1 < strings 2

// Work:
ix      .req w5
ch1     .req w6
ch2     .req w7

string_cmp:
    stp     fp,lr,[sp,#-16]!
    mov     fp,sp

    cbz     ptr1, ptr_null
    cbz     ptr2, ptr_null

compare_lengths:
    subs    ix,len1,len2
    b.eq    lengths_equal
    mov     w0, ix
    b       done

lengths_equal:
    mov     ix, len1

loop:
    cbnz    ix, comp_char
    mov     w0,wzr
    b       done

comp_char:
    ldrb    ch1,[ptr1,ix,uxtw]
    ldrb    ch2,[ptr2,ix,uxtw]
    sub     ix,ix,#1
    cmp     ch1,ch2
    b.eq    loop
    sub     w0,ch1,ch2

done:
    mov     sp,fp
    ldp     fp,lr,[sp],#16
    ret

ptr_null:
    cbz     ptr1, ptr1_null
    mov     w0, #1 // ptr1 != null => pt2 == null => 1
    b       done

ptr1_null:
    cbz    ptr2, both_null
    mov    w0, #-1 // ptr1 == null, ptr2 != null => -1
    b      done

both_null:
    mov     w0, wzr
    b       done
